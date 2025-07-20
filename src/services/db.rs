use std::collections::HashMap;

use aws_sdk_dynamodb::{
    types::{
        AttributeDefinition, AttributeValue, BillingMode, KeySchemaElement, KeyType,
        ScalarAttributeType,
    },
    Client, Error,
};
use log::info;

use crate::{
    models::{fact::FactData, record_type::RecordTypeData},
    services::persist::{PersistenceService, PersistenceServiceError},
    utils::codegen,
};

#[derive(Clone)]
pub struct DbService {
    client: Client,
}

impl DbService {
    pub async fn new(region: String, endpoint: String) -> Self {
        let config = aws_config::defaults(aws_config::BehaviorVersion::latest())
            .test_credentials()
            .region(aws_config::Region::new(region))
            .endpoint_url(endpoint)
            .load()
            .await;

        let dynamodb_local_config = aws_sdk_dynamodb::config::Builder::from(&config).build();

        let client = Client::from_conf(dynamodb_local_config);

        DbService { client }
    }

    pub async fn create_table_if_not_exists(
        &self,
        table: &str,
        key: &str,
    ) -> Result<(), PersistenceServiceError> {
        if self.table_exists(table).await? {
            return Ok(());
        }

        let ad = AttributeDefinition::builder()
            .attribute_name(key)
            .attribute_type(ScalarAttributeType::S)
            .build()?;

        let ks = KeySchemaElement::builder()
            .attribute_name(key)
            .key_type(KeyType::Hash)
            .build()?;

        let response = self
            .client
            .create_table()
            .table_name(table)
            .key_schema(ks)
            .attribute_definitions(ad)
            .billing_mode(BillingMode::PayPerRequest)
            .send()
            .await;

        match response {
            Ok(_) => {
                info!("Added table {} with key {}", table, key);
                Ok(())
            }
            Err(e) => Err(e.into()),
        }
    }

    async fn table_exists(&self, table: &str) -> Result<bool, Error> {
        let resp = self.client.describe_table().table_name(table).send().await;

        match resp {
            Ok(_) => Ok(true),
            Err(e) => {
                if let aws_sdk_dynamodb::error::SdkError::ServiceError(inner) = &e {
                    if inner.err().is_resource_not_found_exception() {
                        return Ok(false);
                    }
                }
                Err(e.into())
            }
        }
    }

    /// Maps DynamoDB attributes to a RecordTypeJson instance.
    fn map_item_to_record_type(item: &HashMap<String, AttributeValue>) -> RecordTypeData {
        RecordTypeData {
            name: item
                .get("name")
                .and_then(|v| v.as_s().ok())
                .map(String::from)
                .unwrap_or_default(),
            id_fields: item
                .get("id_fields")
                .and_then(|v| v.as_l().ok())
                .map(|v| {
                    v.iter()
                        .filter_map(|av| av.as_s().ok())
                        .map(String::from)
                        .collect()
                })
                .unwrap_or_default(),
            data_fields: item
                .get("data_fields")
                .and_then(|v| v.as_l().ok())
                .map(|v| {
                    v.iter()
                        .filter_map(|av| av.as_s().ok())
                        .map(String::from)
                        .collect()
                })
                .unwrap_or_default(),
            metadata_fields: item
                .get("metadata_fields")
                .and_then(|v| v.as_l().ok())
                .map(|v| {
                    v.iter()
                        .filter_map(|av| av.as_s().ok())
                        .map(String::from)
                        .collect()
                })
                .unwrap_or_default(),
        }
    }

    pub async fn get_all_record_types(
        &self,
    ) -> Result<Vec<RecordTypeData>, PersistenceServiceError> {
        let resp = self.client.scan().table_name("types").send().await?;
        let record_types = resp
            .items()
            .into_iter()
            .map(Self::map_item_to_record_type)
            .collect();
        Ok(record_types)
    }

    pub async fn get_record_type(
        &self,
        name: &str,
    ) -> Result<RecordTypeData, PersistenceServiceError> {
        let request = self
            .client
            .get_item()
            .table_name("types")
            .key("name", AttributeValue::S(name.to_owned()));

        let resp = request.send().await?;

        if let Some(item) = resp.item() {
            Ok(Self::map_item_to_record_type(item))
        } else {
            Err(PersistenceServiceError::RecordTypeNotFound(name.to_owned()))
        }
    }

    fn get_key_name(rt: &RecordTypeData) -> String {
        format!("{}-id", rt.name)
    }

    fn get_composite_key_value(f: &FactData) -> String {
        let map = f.to_all_values_map();
        f.type_
            .id_fields
            .iter()
            .chain(f.type_.metadata_fields.iter())
            .map(|k| map.get(k).cloned().unwrap_or_default())
            .collect::<Vec<_>>()
            .join("-")
    }

    pub async fn put_record_type(
        &self,
        rt: &RecordTypeData,
    ) -> Result<(), PersistenceServiceError> {
        self.create_table_if_not_exists("types", "name").await?;

        let composite_key = Self::get_key_name(&rt);

        let name = AttributeValue::S(rt.name.clone());
        let id_fields = AttributeValue::L(
            rt.id_fields
                .iter()
                .map(|s| AttributeValue::S(s.to_owned()))
                .collect(),
        );
        let data_fields = AttributeValue::L(
            rt.data_fields
                .iter()
                .map(|s| AttributeValue::S(s.to_owned()))
                .collect(),
        );
        let metadata_fields = AttributeValue::L(
            rt.metadata_fields
                .iter()
                .map(|s| AttributeValue::S(s.to_owned()))
                .collect(),
        );
        let request = self
            .client
            .put_item()
            .table_name("types")
            .item("name", name)
            .item("id_fields", id_fields)
            .item("data_fields", data_fields)
            .item("metadata_fields", metadata_fields);

        let _ = request.send().await?;

        self.create_table_if_not_exists(&rt.name, &composite_key)
            .await?;

        Ok(())
    }

    pub async fn delete_record_type(&self, name: &str) -> Result<(), PersistenceServiceError> {
        let request = self
            .client
            .delete_item()
            .table_name("types")
            .key("name", AttributeValue::S(name.to_string()));

        let _ = request.send().await?;
        info!("Deleted record type {}", name);

        Ok(())
    }

    pub async fn put_fact(&self, fact: FactData) -> Result<(), PersistenceServiceError> {
        let mut request = self
            .client
            .put_item()
            .table_name(fact.type_.name.clone())
            .item(
                Self::get_key_name(&fact.type_),
                AttributeValue::S(Self::get_composite_key_value(&fact)),
            );

        info!(
            "Putting fact: {:?}, key_name: {:?}, key_value: {:?}",
            fact,
            Self::get_key_name(&fact.type_),
            AttributeValue::S(Self::get_composite_key_value(&fact))
        );

        for (key, value) in fact.to_all_values_map().into_iter() {
            request = request.item(key, AttributeValue::S(value));
        }

        let _ = request.send().await?;

        Ok(())
    }

    pub async fn get_all_facts(&self, rt: &RecordTypeData) -> Result<Vec<FactData>, Error> {
        let resp = self
            .client
            .scan()
            .table_name(rt.name.clone())
            .send()
            .await?;

        let items = resp.items();

        let facts = items
            .into_iter()
            .map(|item| FactData {
                type_: rt.clone(),
                values: rt
                    .all_fields()
                    .iter()
                    .map(|field| {
                        item.get(field)
                            .and_then(|v| v.as_s().ok())
                            .map(String::from)
                            .unwrap_or_default()
                    })
                    .collect(),
            })
            .collect();

        Ok(facts)
    }

    pub async fn generate_data_files(&self) -> Result<(), PersistenceServiceError> {
        let record_types = self
            .get_all_record_types()
            .await?
            .into_iter()
            .map(|rt| rt.into())
            .collect::<Vec<_>>();

        codegen::generate_record_types_json_and_pythia_program(&record_types)?;

        for rt in record_types{
            info!("Generating Prolog file for record type: {}", rt.name);
            let facts = self.get_all_facts(&rt).await?;
            codegen::generate_fact_programs_for_record_types(&rt, facts)?;
        }
        Ok(())
    }
}

impl PersistenceService for DbService {
    async fn create_record_types_table(&self) -> Result<(), PersistenceServiceError> {
        self.create_table_if_not_exists("types", "rt_name").await
    }
}

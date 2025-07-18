use std::collections::HashMap;

use aws_sdk_dynamodb::{
    types::{
        AttributeDefinition, AttributeValue, BillingMode, KeySchemaElement, KeyType,
        ScalarAttributeType,
    },
    Client, Error,
};
use log::info;

use crate::services::persist::{PersistenceService, PersistenceServiceError};

#[derive(Clone)]
pub struct RecordTypeDesc {
    pub rt_name: String,
    pub id_fields: Vec<String>,
    pub data_fields: Vec<String>,
    pub metadata_fields: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub struct RecordTypeDescOut {
    pub rt_name: String,
    pub id_fields: Option<Vec<String>>,
    pub data_fields: Option<Vec<String>>,
    pub metadata_fields: Option<Vec<String>>,
}

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

    pub async fn get_all_record_types(&self) -> Result<Vec<RecordTypeDescOut>, Error> {
        let resp = self.client.scan().table_name("types").send().await?;

        let items = resp.items();

        let record_types: Vec<RecordTypeDescOut> = items
            .into_iter()
            .map(|item| RecordTypeDescOut {
                rt_name: item
                    .get("rt_name")
                    .and_then(|v| v.as_s().ok())
                    .map(String::from)
                    .unwrap_or_default(),
                id_fields: item.get("id_fields").and_then(|v| v.as_l().ok()).map(|l| {
                    l.iter()
                        .filter_map(|v| v.as_s().ok())
                        .map(String::from)
                        .collect()
                }),
                data_fields: item
                    .get("data_fields")
                    .and_then(|v| v.as_l().ok())
                    .map(|l| {
                        l.iter()
                            .filter_map(|v| v.as_s().ok())
                            .map(String::from)
                            .collect()
                    }),
                metadata_fields: item
                    .get("metadata_fields")
                    .and_then(|v| v.as_l().ok())
                    .map(|l| {
                        l.iter()
                            .filter_map(|v| v.as_s().ok())
                            .map(String::from)
                            .collect()
                    }),
            })
            .collect();

        Ok(record_types)
    }

    pub async fn put_record_type(&self, item: RecordTypeDesc, table: &String) -> Result<(), Error> {
        let rt_name = AttributeValue::S(item.rt_name);
        let id_fields =
            AttributeValue::L(item.id_fields.into_iter().map(AttributeValue::S).collect());
        let data_fields = AttributeValue::L(
            item.data_fields
                .into_iter()
                .map(AttributeValue::S)
                .collect(),
        );
        let metadata_fields = AttributeValue::L(
            item.metadata_fields
                .into_iter()
                .map(AttributeValue::S)
                .collect(),
        );

        let request = self
            .client
            .put_item()
            .table_name(table)
            .item("rt_name", rt_name)
            .item("id_fields", id_fields)
            .item("data_fields", data_fields)
            .item("metadata_fields", metadata_fields);

        let _ = request.send().await?;
        info!("Info added item to table {}...", table);

        Ok(())
    }

    pub async fn put_fact(
        &self,
        item: HashMap<String, String>,
        table: &String,
    ) -> Result<(), Error> {
        let mut request = self.client.put_item().table_name(table);

        for (key, value) in item {
            request = request.item(key, AttributeValue::S(value));
        }

        let _ = request.send().await?;
        info!("Info added item to table {}...", table);

        Ok(())
    }

    pub async fn get_all_facts(
        &self,
        table: &String,
    ) -> Result<Vec<HashMap<String, String>>, Error> {
        let resp = self.client.scan().table_name(table).send().await?;

        let items = resp.items();

        let facts: Vec<HashMap<String, String>> = items
            .into_iter()
            .map(|item| {
                item.iter()
                    .filter_map(|(k, v)| v.as_s().ok().map(|s| (k.clone(), s.to_string())))
                    .collect()
            })
            .collect();

        Ok(facts)
    }
}

impl PersistenceService for DbService {
    async fn create_record_types_table(&self) -> Result<(), PersistenceServiceError> {
        self.create_table_if_not_exists("types", "rt_name").await
    }
}

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
    utils::codegen::{self, KnowledgeBase, KnowledgeBaseItem},
};

use aws_sdk_dynamodb::operation::{
    create_table::CreateTableError, delete_item::DeleteItemError, get_item::GetItemError,
    put_item::PutItemError, scan::ScanError,
};

use crate::utils::codegen::CodeGenError;

#[derive(thiserror::Error, Debug)]
pub enum DbServiceError {
    #[error("DynamoDB SDK Error: {0}")]
    AwsDynamoDbError(#[from] aws_sdk_dynamodb::Error),

    #[error("DynamoDB SDK BuildError: {0}")]
    AwsDynamoDbBuildError(#[from] aws_sdk_dynamodb::error::BuildError),

    #[error("DynamoDB CreateTableError: {0}")]
    AwsDynamoDbCreateTableError(#[from] aws_sdk_dynamodb::error::SdkError<CreateTableError>),

    #[error("DynamoDB ScanError: {0}")]
    AwsDynamoDbScanError(#[from] aws_sdk_dynamodb::error::SdkError<ScanError>),

    #[error("DynamoDB GetItemError: {0}")]
    AwsDynamoDbGetItemError(#[from] aws_sdk_dynamodb::error::SdkError<GetItemError>),

    #[error("DynamoDB PutItemError: {0}")]
    AwsDynamoDbPutItemError(#[from] aws_sdk_dynamodb::error::SdkError<PutItemError>),

    #[error("DynamoDB DeleteItemError: {0}")]
    AwsDynamoDbDeleteItemError(#[from] aws_sdk_dynamodb::error::SdkError<DeleteItemError>),

    #[error("Code generation error: {0}")]
    CodeGenError(#[from] CodeGenError),

    #[error("Failed to find record type: {0}")]
    RecordTypeNotFound(String),

    #[error("Failed to find knowledge base item: {0}")]
    KnowledgeBaseItemNotFound(String),
}

#[derive(Clone)]
pub struct DbService {
    client: Client,
}

impl DbService {
    pub async fn new(region: String, endpoint: String) -> Self {
        let config = aws_config::defaults(aws_config::BehaviorVersion::latest())
            .region(aws_config::Region::new(region))
            .endpoint_url(endpoint)
            .load()
            .await;

        let dynamodb_local_config = aws_sdk_dynamodb::config::Builder::from(&config).build();

        let client = Client::from_conf(dynamodb_local_config);

        DbService { client }
    }

    pub async fn create_essential_tables_if_not_exist(&self) -> Result<(), DbServiceError> {
        self.create_table_if_not_exists("types", "name").await?;
        self.create_table_if_not_exists("knowledge_base", "file_path")
            .await?;
        Ok(())
    }

    async fn create_table_if_not_exists(
        &self,
        table: &str,
        key: &str,
    ) -> Result<(), DbServiceError> {
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

    pub async fn get_all_record_types(&self) -> Result<Vec<RecordTypeData>, DbServiceError> {
        let resp = self.client.scan().table_name("types").send().await?;
        let record_types = resp
            .items()
            .into_iter()
            .map(Self::map_item_to_record_type)
            .collect();
        Ok(record_types)
    }

    pub async fn get_record_type(&self, name: &str) -> Result<RecordTypeData, DbServiceError> {
        let request = self
            .client
            .get_item()
            .table_name("types")
            .key("name", AttributeValue::S(name.to_owned()));

        let resp = request.send().await?;

        if let Some(item) = resp.item() {
            Ok(Self::map_item_to_record_type(item))
        } else {
            Err(DbServiceError::RecordTypeNotFound(name.to_owned()))
        }
    }

    pub async fn put_record_type(&self, rt: &RecordTypeData) -> Result<(), DbServiceError> {
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

        self.create_table_if_not_exists(&rt.name, "id").await?;

        Ok(())
    }

    pub async fn delete_record_type(&self, name: &str) -> Result<(), DbServiceError> {
        let request = self
            .client
            .delete_item()
            .table_name("types")
            .key("name", AttributeValue::S(name.to_string()));

        let _ = request.send().await?;
        info!("Deleted record type {}", name);

        Ok(())
    }

    pub async fn put_fact(&self, fact: &FactData) -> Result<(), DbServiceError> {
        let uuid = uuid::Uuid::new_v4();

        let mut request = self
            .client
            .put_item()
            .table_name(fact.type_.name.clone())
            .item("id", AttributeValue::S(uuid.to_string()));

        info!("Putting fact for '{}', key: {}", fact.type_.name, uuid);

        for (key, value) in fact.to_all_values_map().into_iter() {
            request = request.item(key, AttributeValue::S(value));
        }

        let _ = request.send().await?;

        Ok(())
    }

    /// Retrieves all facts for a given record type.
    pub async fn get_all_facts(
        &self,
        rt: &RecordTypeData,
    ) -> Result<Vec<FactData>, DbServiceError> {
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
                id: item
                    .get("id")
                    .and_then(|v| v.as_s().ok())
                    .map(String::from)
                    .unwrap_or_default(),
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

    pub async fn update_knowledge_base(&self) -> Result<(), DbServiceError> {
        let rts = self.get_all_record_types().await?;

        let mut rt_facts = Vec::new();
        for rt in &rts {
            let facts = self.get_all_facts(rt).await?;
            rt_facts.push((rt.clone(), facts));
        }

        let kb = codegen::generate_knowledge_base(&rts, &rt_facts)?;

        self.update_knowledge_base_item(kb.record_type_definitions)
            .await?;
        self.update_knowledge_base_item(kb.pythia_program).await?;
        for fact_program in kb.fact_programs {
            self.update_knowledge_base_item(fact_program).await?;
        }

        Ok(())
    }

    /// Deletes a fact by its ID for a specific record type.
    pub async fn delete_fact(&self, rt: &RecordTypeData, fact_id: &str) -> Result<(), DbServiceError> {
        let request = self
            .client
            .delete_item()
            .table_name(rt.name.to_owned())
            .key("id", AttributeValue::S(fact_id.to_owned()));

        request.send().await?;
        Ok(())
    }

    async fn update_knowledge_base_item(
        &self,
        kbi: KnowledgeBaseItem,
    ) -> Result<(), DbServiceError> {
        let request = self
            .client
            .put_item()
            .table_name("knowledge_base")
            .item("file_path", AttributeValue::S(kbi.file_path))
            .item("contents", AttributeValue::S(kbi.contents));

        request.send().await?;
        Ok(())
    }

    pub async fn get_knowledge_base(&self) -> Result<KnowledgeBase, DbServiceError> {
        let rts = self.get_all_record_types().await?;

        let mut fact_programs = Vec::new();
        for rt in &rts {
            let path = codegen::get_fact_program_file_path(rt);
            fact_programs.push(self.get_knowledge_base_item(&path).await?);
        }

        let kb = KnowledgeBase {
            record_type_definitions: self.get_knowledge_base_item("data/types.json").await?,
            pythia_program: self
                .get_knowledge_base_item("data/internal/pythia.pl")
                .await?,
            fact_programs,
        };

        Ok(kb)
    }

    async fn get_knowledge_base_item(
        &self,
        file_path: &str,
    ) -> Result<KnowledgeBaseItem, DbServiceError> {
        let request = self
            .client
            .get_item()
            .table_name("knowledge_base")
            .key("file_path", AttributeValue::S(file_path.to_owned()));

        let resp = request.send().await?;

        if let Some(item) = resp.item() {
            let contents = item
                .get("contents")
                .and_then(|v| v.as_s().ok())
                .map(String::from)
                .unwrap_or_default();
            Ok(KnowledgeBaseItem {
                file_path: file_path.to_owned(),
                contents,
            })
        } else {
            Err(DbServiceError::KnowledgeBaseItemNotFound(
                file_path.to_owned(),
            ))
        }
    }

    pub async fn export_knowledge_base(&self) -> Result<(), DbServiceError> {
        let kb = self.get_knowledge_base().await?;
        codegen::export_knowledge_base(&kb)?;
        Ok(())
    }
}

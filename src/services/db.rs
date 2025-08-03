use std::collections::HashMap;

use aws_sdk_dynamodb::{
    operation::query::QueryError,
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

    #[error("DynamoDB QueryError: {0}")]
    AwsDynamoDbQueryError(#[from] aws_sdk_dynamodb::error::SdkError<QueryError>),

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
    user: String,
    client: Client,
}

const TABLE_PYTHIA: &str = "pythia";

impl DbService {
    pub async fn new(user: String) -> Self {
        let config = aws_config::defaults(aws_config::BehaviorVersion::latest())
            .load()
            .await;

        let dynamodb_local_config = aws_sdk_dynamodb::config::Builder::from(&config).build();
        DbService {
            user,
            client: Client::from_conf(dynamodb_local_config),
        }
    }

    pub async fn new_local(user: String) -> Self {
        let config = aws_config::defaults(aws_config::BehaviorVersion::latest())
            .test_credentials()
            .region(aws_config::Region::new("us-west-2"))
            .endpoint_url("http://host.docker.internal:8000")
            .load()
            .await;

        let dynamodb_local_config = aws_sdk_dynamodb::config::Builder::from(&config).build();
        DbService {
            user,
            client: Client::from_conf(dynamodb_local_config),
        }
    }

    pub fn set_user(&mut self, user: String) {
        self.user = user;
    }

    pub async fn create_table_if_not_exists(
        &self,
        table: &str,
        pk_name: &str,
        sk_name: &str,
    ) -> Result<(), DbServiceError> {
        if self.table_exists(table).await? {
            return Ok(());
        }

        let pk_def = AttributeDefinition::builder()
            .attribute_name(pk_name)
            .attribute_type(ScalarAttributeType::S)
            .build()?;
        let sk_def = AttributeDefinition::builder()
            .attribute_name(sk_name)
            .attribute_type(ScalarAttributeType::S)
            .build()?;

        let pk_schema = KeySchemaElement::builder()
            .attribute_name(pk_name)
            .key_type(KeyType::Hash)
            .build()?;
        let sk_schema = KeySchemaElement::builder()
            .attribute_name(sk_name)
            .key_type(KeyType::Range)
            .build()?;

        let response = self
            .client
            .create_table()
            .table_name(table)
            .key_schema(pk_schema)
            .key_schema(sk_schema)
            .attribute_definitions(pk_def)
            .attribute_definitions(sk_def)
            .billing_mode(BillingMode::PayPerRequest)
            .send()
            .await;

        match response {
            Ok(_) => {
                info!("Added table {} with key {}", table, pk_name);
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

    pub async fn add_user_token(&self, user: String) -> Result<(), DbServiceError> {
        let _ = self
            .client
            .put_item()
            .table_name(TABLE_PYTHIA)
            .item("pk", AttributeValue::S(format!("user#{}", user)))
            .item("sk", AttributeValue::S("token".to_owned()))
            .send()
            .await?;

        Ok(())
    }

    pub async fn user_token_exists(&self, user: String) -> Result<bool, DbServiceError> {
        let resp = self
            .client
            .get_item()
            .table_name(TABLE_PYTHIA)
            .key("pk", AttributeValue::S(format!("user#{}", user)))
            .key("sk", AttributeValue::S("token".to_owned()))
            .send()
            .await?;

        Ok(resp.item.is_some())
    }

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

    fn get_user(&self) -> String {
        format!("user#{}", self.user)
    }

    pub async fn get_all_record_types(&self) -> Result<Vec<RecordTypeData>, DbServiceError> {
        let resp = self
            .client
            .query()
            .table_name(TABLE_PYTHIA)
            .key_condition_expression("pk = :pk AND begins_with(sk, :sk_prefix)")
            .expression_attribute_values(":pk", AttributeValue::S(self.get_user()))
            .expression_attribute_values(":sk_prefix", AttributeValue::S("type#".to_owned()))
            .send()
            .await?;
        let record_types = resp
            .items()
            .into_iter()
            .map(Self::map_item_to_record_type)
            .collect();
        Ok(record_types)
    }

    pub async fn get_record_type(&self, name: &str) -> Result<RecordTypeData, DbServiceError> {
        let resp = self
            .client
            .get_item()
            .table_name(TABLE_PYTHIA)
            .key("pk", AttributeValue::S(self.get_user()))
            .key("sk", AttributeValue::S(format!("type#{}", name)))
            .send()
            .await?;

        if let Some(item) = resp.item() {
            Ok(Self::map_item_to_record_type(item))
        } else {
            Err(DbServiceError::RecordTypeNotFound(name.to_owned()))
        }
    }

    pub async fn put_record_type(&self, rt: &RecordTypeData) -> Result<(), DbServiceError> {
        let pk = AttributeValue::S(self.get_user());
        let sk = AttributeValue::S(format!("type#{}", rt.name));
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
            .table_name(TABLE_PYTHIA)
            .item("pk", pk)
            .item("sk", sk)
            .item("name", name)
            .item("id_fields", id_fields)
            .item("data_fields", data_fields)
            .item("metadata_fields", metadata_fields);

        let _ = request.send().await?;

        Ok(())
    }

    pub async fn delete_record_type(&self, name: &str) -> Result<(), DbServiceError> {
        let _ = self
            .client
            .delete_item()
            .table_name(TABLE_PYTHIA)
            .key("pk", AttributeValue::S(self.get_user()))
            .key("sk", AttributeValue::S(format!("type#{}", name)))
            .condition_expression("attribute_exists(pk) AND attribute_exists(sk)")
            .send()
            .await?;

        info!("Deleted record type {}", name);

        Ok(())
    }

    pub async fn put_fact(&self, fact: &FactData) -> Result<(), DbServiceError> {
        let uuid = uuid::Uuid::new_v4();

        let mut request = self
            .client
            .put_item()
            .table_name(TABLE_PYTHIA)
            .item("pk", AttributeValue::S(self.get_user()))
            .item(
                "sk",
                AttributeValue::S(format!("record#{}#{}", fact.type_.name, uuid)),
            )
            .item("_id", AttributeValue::S(uuid.to_string()));

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
            .query()
            .table_name(TABLE_PYTHIA)
            .key_condition_expression("pk = :pk AND begins_with(sk, :sk_prefix)")
            .expression_attribute_values(":pk", AttributeValue::S(self.get_user()))
            .expression_attribute_values(
                ":sk_prefix",
                AttributeValue::S(format!("record#{}#", rt.name)),
            )
            .send()
            .await?;

        let facts = resp
            .items()
            .into_iter()
            .map(|item| FactData {
                id: item
                    .get("_id")
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
    pub async fn delete_fact(
        &self,
        rt: &RecordTypeData,
        fact_id: &str,
    ) -> Result<(), DbServiceError> {
        let _ = self
            .client
            .delete_item()
            .table_name(TABLE_PYTHIA)
            .key("pk", AttributeValue::S(self.get_user()))
            .key(
                "sk",
                AttributeValue::S(format!("record#{}#{}", rt.name, fact_id)),
            )
            .send()
            .await?;

        Ok(())
    }

    async fn update_knowledge_base_item(
        &self,
        kbi: KnowledgeBaseItem,
    ) -> Result<(), DbServiceError> {
        let request = self
            .client
            .put_item()
            .table_name(TABLE_PYTHIA)
            .item("pk", AttributeValue::S(self.get_user()))
            .item("sk", AttributeValue::S(format!("kb#{}", kbi.file_path)))
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
        let resp = self
            .client
            .get_item()
            .table_name(TABLE_PYTHIA)
            .key("pk", AttributeValue::S(self.get_user()))
            .key(
                "sk",
                AttributeValue::S(format!("kb#{}", file_path.to_owned())),
            )
            .send()
            .await?;

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

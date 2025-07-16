use aws_sdk_dynamodb::{
    types::{AttributeDefinition, BillingMode, KeySchemaElement, KeyType, ScalarAttributeType},
    Client, Error,
};
use log::info;

#[derive(Clone)]
pub struct DbService {
    pub client: Client,
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

    pub async fn create_table_if_not_exists(&self, table: &str, key: &str) -> Result<(), Error> {
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
}

use aws_sdk_dynamodb::{
    operation::create_table::CreateTableOutput,
    types::{AttributeDefinition, BillingMode, KeySchemaElement, KeyType, ScalarAttributeType},
    Client, Error,
};

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

    pub async fn create_table(&self, table: &str, key: &str) -> Result<CreateTableOutput, Error> {
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
            Ok(out) => {
                println!("Added table {} with key {}", table, key);
                Ok(out)
            }
            Err(e) => {
                eprintln!("Got an error creating table:");
                eprintln!("{}", e);
                Err(e.into())
            }
        }
    }
}

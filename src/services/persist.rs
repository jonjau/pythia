// create record_types table
// put record types
// get all record types
// create facts tables for each record_type
// get all facts
// put facts

use aws_sdk_dynamodb::operation::create_table::CreateTableError;

/// Errors that can occur while loading/building record types from a JSON file.
#[derive(thiserror::Error, Debug)]
pub enum PersistenceServiceError {
    #[error("DynamoDB SDK Error: {0}")]
    AwsDynamoDbError(#[from] aws_sdk_dynamodb::Error),
    
    #[error("DynamoDB SDK BuildError: {0}")]
    AwsDynamoDbBuildError(#[from] aws_sdk_dynamodb::error::BuildError),

    #[error("DynamoDB CreateTableError: {0}")]
    AwsDynamoDbCreateTableError(#[from] aws_sdk_dynamodb::error::SdkError<CreateTableError, >),
}

pub trait PersistenceService {
    async fn create_record_types_table(&self) -> Result<(), PersistenceServiceError>;
}

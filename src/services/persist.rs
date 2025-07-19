// create record_types table
// put record types
// get all record types
// create facts tables for each record_type
// get all facts
// put facts

use aws_sdk_dynamodb::operation::{
    create_table::CreateTableError, delete_item::DeleteItemError, get_item::GetItemError,
    put_item::PutItemError, scan::ScanError,
};

use crate::utils::codegen::CodeGenError;

/// Errors that can occur while loading/building record types from a JSON file.
#[derive(thiserror::Error, Debug)]
pub enum PersistenceServiceError {
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
}

pub trait PersistenceService {
    async fn create_record_types_table(&self) -> Result<(), PersistenceServiceError>;
}

use serde::Deserialize;

use crate::{
    models::record_type::RecordTypeData,
    services::db::{DbService, DbServiceError},
};

#[derive(thiserror::Error, Debug)]
pub enum RecordTypeServiceError {
    #[error("Database error: {0}")]
    DbServiceError(#[from] DbServiceError),
}

#[derive(Clone)]
pub struct RecordTypeService {
    db: DbService,
}

#[derive(Deserialize)]
pub struct CreateRecordTypeFormData {
    name: String,
    id_fields: String,
    data_fields: String,
    metadata_fields: String,
}

impl RecordTypeService {
    pub fn new(db: DbService) -> Self {
        RecordTypeService { db }
    }

    fn uppercase_first_ascii_letter(s: &str) -> String {
        let mut chars = s.chars();
        match chars.next() {
            Some(c) => c.to_ascii_uppercase().to_string() + chars.as_str(),
            None => String::new(),
        }
    }

    pub async fn add_record_type(
        &self,
        f: CreateRecordTypeFormData,
    ) -> Result<RecordTypeData, RecordTypeServiceError> {
        let strip_whitespace =
            |s: String| s.chars().filter(|c| !c.is_whitespace()).collect::<String>();

        let record_type = RecordTypeData {
            name: strip_whitespace(f.name).to_ascii_lowercase(),
            id_fields: strip_whitespace(f.id_fields)
                .split(',')
                .map(Self::uppercase_first_ascii_letter)
                .collect(),
            data_fields: strip_whitespace(f.data_fields)
                .split(',')
                .map(Self::uppercase_first_ascii_letter)
                .collect(),
            metadata_fields: strip_whitespace(f.metadata_fields)
                .split(',')
                .map(Self::uppercase_first_ascii_letter)
                .collect(),
        };

        self.db.put_record_type(&record_type).await?;

        Ok(record_type)
    }

    pub async fn delete_record_type(&self, rt_name: &str) -> Result<(), RecordTypeServiceError> {
        Ok(self.db.delete_record_type(rt_name).await?)
    }

    pub async fn get_all_record_types(
        &self,
    ) -> Result<Vec<RecordTypeData>, RecordTypeServiceError> {
        Ok(self.db.get_all_record_types().await?)
    }
}

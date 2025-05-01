use std::{fs, io, path};

use askama::Template;
use serde_json::Value;

struct RecordType {
    name: String,
    id_fields: String,
    data_fields: String,
    metadata_fields: String,
}

#[derive(Template)]
#[template(path = "pythia.pl")]
struct PythiaTemplate {
    import_paths: Vec<String>,
    record_types: Vec<RecordType>,
}

#[derive(thiserror::Error, Debug)]
pub enum CodeGenError {
    #[error("Field not found in type definition: {0}")]
    FieldNotFound(String),
    #[error("Field is of invalid type: {0}")]
    InvalidFieldType(String),
    #[error("Failed to read JSON file: {0}")]
    FailedToReadJsonFile(#[from] io::Error),
    #[error("Invalid JSON format: {0}")]
    InvalidJsonFormat(#[from] serde_json::Error),
    #[error("Failed to render prolog template: {0}")]
    FailedToRender(#[from] askama::Error),
}

pub fn generate_main_prolog_program() -> Result<String, CodeGenError> {
    let json_data = fs::read_to_string(path::Path::new("data/types.json"))?;
    let objects: Vec<serde_json::Value> = serde_json::from_str(&json_data)?;

    // TODO JCJ: this can be got from the record types array
    let import_paths = objects
        .iter()
        .map(|o| {
            let n = o
                .get("name")
                .ok_or(CodeGenError::FieldNotFound("name".to_owned()))?
                .as_str()
                .ok_or(CodeGenError::InvalidFieldType("name".to_owned()))?;
            Ok(format!("'./data/{}.pl'", n))
        })
        .collect::<Result<Vec<_>, CodeGenError>>()?;

    let record_types = objects
        .iter()
        .map(|o| {
            let name = o
                .get("name")
                .ok_or(CodeGenError::FieldNotFound("name".to_owned()))?
                .as_str()
                .ok_or(CodeGenError::InvalidFieldType("name".to_owned()))?;

            let id_fields = parse_array_field(o, "id_fields")?;
            let id_fields = id_fields
                .is_empty()
                .then(|| vec!["\"NONE\""])
                .unwrap_or(id_fields);

            let data_fields = parse_array_field(o, "data_fields")?;
            let data_fields = data_fields
                .is_empty()
                .then(|| vec!["\"NONE\""])
                .unwrap_or(data_fields);

            let metadata_fields = parse_array_field(o, "metadata_fields")?;
            let metadata_fields = metadata_fields
                .is_empty()
                .then(|| vec!["\"NONE\""])
                .unwrap_or(metadata_fields);

            Ok(RecordType {
                name: name.to_string(),
                id_fields: id_fields.join(", "),
                data_fields: data_fields.join(", "),
                metadata_fields: metadata_fields.join(", "),
            })
        })
        .collect::<Result<Vec<_>, CodeGenError>>()?;

    Ok(PythiaTemplate {
        import_paths,
        record_types,
    }
    .render()?)
}

fn parse_array_field<'a>(o: &'a Value, field_name: &str) -> Result<Vec<&'a str>, CodeGenError> {
    let array = o
        .get(field_name)
        .ok_or_else(|| CodeGenError::FieldNotFound(field_name.to_owned()))?
        .as_array()
        .ok_or_else(|| CodeGenError::InvalidFieldType(field_name.to_owned()))?;

    array
        .iter()
        .map(|v| {
            v.as_str()
                .ok_or_else(|| CodeGenError::InvalidFieldType(field_name.to_owned()))
        })
        .collect()
}

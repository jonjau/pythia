use std::{
    fs,
    io::{self, Write},
    path,
};

use askama::Template;
use serde_json::Value;

#[derive(Clone)]
struct RecordType {
    name: String,
    id_fields: String,
    data_fields: String,
    metadata_fields: String,
    n_fields: usize,
}

#[derive(Template)]
#[template(path = "fact.pl")]
struct FactTemplate {
    record_type: RecordType,
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
    #[error("IO Error: {0}")]
    IoError(#[from] io::Error),
    #[error("Invalid JSON format: {0}")]
    InvalidJsonFormat(#[from] serde_json::Error),
    #[error("Failed to render prolog template: {0}")]
    FailedToRender(#[from] askama::Error),
}

pub fn generate_prolog_programs() -> Result<(), CodeGenError> {
    let record_types = read_record_types_from_json("data/types.json")?;

    let pythia_program = PythiaTemplate {
        import_paths: get_import_paths(&record_types),
        record_types: record_types.clone(),
    }
    .render()?;

    fs::write("data/internal/pythia.pl", pythia_program)?;

    let record_types_with_no_prolog_file = record_types.iter().filter(|record_type| {
        !fs::exists(get_fact_program_file_path(record_type)).is_ok_and(|e| e)
    });

    for record_type in record_types_with_no_prolog_file {
        let mut file = fs::File::create(get_fact_program_file_path(record_type))?;

        let fact_program = (FactTemplate {
            record_type: record_type.clone(),
        })
        .render()?;

        file.write_all(fact_program.as_bytes())?;
    }

    Ok(())
}

fn get_fact_program_file_path(record_type: &RecordType) -> String {
    format!("data/{}.pl", record_type.name)
}

fn read_record_types_from_json(file_path: &str) -> Result<Vec<RecordType>, CodeGenError> {
    let json_data = fs::read_to_string(path::Path::new(file_path))?;
    let objects: Vec<serde_json::Value> = serde_json::from_str(&json_data)?;

    Ok(objects
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
                n_fields: id_fields.len() + data_fields.len() + metadata_fields.len(),
            })
        })
        .collect::<Result<Vec<_>, CodeGenError>>()?)
}

fn get_import_paths(record_types: &Vec<RecordType>) -> Vec<String> {
    record_types
        .iter()
        .map(|rt| format!("'./data/{}.pl'", &rt.name))
        .collect::<Vec<_>>()
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

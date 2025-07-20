use std::{
    fs,
    io::{self, Write},
    path,
};

use askama::Template;
use serde_json::Value;

use crate::models::{fact::FactJson, record_type::RecordTypeJson};

#[derive(Template)]
#[template(path = "prolog/fact.pl")]
struct FactTemplate<'a> {
    record_type: &'a RecordTypeJson,
    facts: Vec<FactJson>,
}

#[derive(Template)]
#[template(path = "prolog/pythia.pl")]
struct PythiaTemplate {
    import_paths: Vec<String>,
    record_types: Vec<RecordTypeJson>,
}

/// Errors that can occur during Prolog code generation.
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

/// Generates and writes to Prolog files for each well-defined record type in `data/types.json`:
///
/// - `data/<record type>.pl` for the facts of each record type.
/// - `data/internal/pythia.pl` for state change predicates.
///
/// Both kinds of Prolog files are described by Askama templates.
///
/// # Errors
/// Returns an error if this fails to read/create files, could not render the Prolog template, or
/// finds a malformed record type or a non-string field definition in the JSON.
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
            record_type,
            facts: vec![],
        })
        .render()?;

        file.write_all(fact_program.as_bytes())?;
    }

    Ok(())
}

pub fn generate_fact_programs_for_record_types(
    record_type: &RecordTypeJson,
    facts: Vec<FactJson>,
) -> Result<(), CodeGenError> {
    let file_path = get_fact_program_file_path(record_type);
    let fact_program = FactTemplate { record_type, facts }.render()?;

    println!("Writing facts: {} ", fact_program);

    fs::write(file_path, fact_program)?;
    Ok(())
}

fn get_fact_program_file_path(record_type: &RecordTypeJson) -> String {
    format!("data/{}.pl", record_type.name)
}

fn read_record_types_from_json(file_path: &str) -> Result<Vec<RecordTypeJson>, CodeGenError> {
    let json_data = fs::read_to_string(path::Path::new(file_path))?;
    let objects: Vec<serde_json::Value> = serde_json::from_str(&json_data)?;

    Ok(objects
        .iter()
        .map(|o| {
            let name = o
                .get("name")
                .and_then(|v| v.as_str())
                .ok_or_else(|| CodeGenError::FieldNotFound("name".to_owned()))?;

            let id_fields = parse_array_field(o, "id_fields")?
                .into_iter()
                .map(|s| s.to_owned())
                .collect::<Vec<_>>();
            let data_fields = parse_array_field(o, "data_fields")?
                .into_iter()
                .map(|s| s.to_owned())
                .collect::<Vec<_>>();
            let metadata_fields = parse_array_field(o, "metadata_fields")?
                .into_iter()
                .map(|s| s.to_owned())
                .collect::<Vec<_>>();

            Ok(RecordTypeJson {
                name: name.to_owned(),
                id_fields,
                data_fields,
                metadata_fields,
            })
        })
        .collect::<Result<Vec<_>, CodeGenError>>()?)
}

fn get_import_paths(record_types: &Vec<RecordTypeJson>) -> Vec<String> {
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

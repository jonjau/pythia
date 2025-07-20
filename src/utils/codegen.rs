use std::{
    fs,
    io::{self, Read},
};

use askama::Template;

use crate::models::{fact::FactData, record_type::RecordTypeData};

#[derive(Template)]
#[template(path = "prolog/fact.pl")]
struct FactTemplate<'a> {
    record_type: &'a RecordTypeData,
    facts: Vec<FactData>,
}

#[derive(Template)]
#[template(path = "prolog/pythia.pl")]
struct PythiaTemplate {
    import_paths: Vec<String>,
    record_types: Vec<RecordTypeData>,
}

/// Errors that can occur during Prolog code generation.
#[derive(thiserror::Error, Debug)]
pub enum CodeGenError {
    #[error("IO Error: {0}")]
    IoError(#[from] io::Error),
    #[error("Invalid JSON format: {0}")]
    InvalidJsonFormat(#[from] serde_json::Error),
    #[error("Failed to render prolog template: {0}")]
    FailedToRender(#[from] askama::Error),
}

const TYPES_JSON_PATH: &str = "data/types.json";
const PYTHIA_PROGRAM_PATH: &str = "data/internal/pythia.pl";

pub fn load_record_types() -> Result<Vec<RecordTypeData>, CodeGenError> {
    let file = fs::File::open(TYPES_JSON_PATH)?;
    let reader = io::BufReader::new(file);
    let record_types: Vec<RecordTypeData> = serde_json::from_reader(reader)?;
    Ok(record_types)
}

pub fn load_pythia_program() -> Result<String, CodeGenError> {
    let file = fs::File::open(PYTHIA_PROGRAM_PATH)?;
    let mut reader = io::BufReader::new(file);
    let mut program = String::new();
    reader.read_to_string(&mut program)?;
    Ok(program)
}

/// Generates and writes to:
///
/// - `data/types.json` for record type defitions and main 'Pythia' prolog program for state change predicates.
/// - `data/internal/pythia.pl` for state change predicates (Askama template).
///
/// Returns an error if this fails to read/create files, could not render the Prolog template, or
/// encounters invalid JSON.
pub fn generate_record_types_json_and_pythia_program(
    record_types: &Vec<RecordTypeData>,
) -> Result<(), CodeGenError> {
    let json_data = serde_json::to_string_pretty(&record_types)?;
    fs::write(TYPES_JSON_PATH, json_data)?;

    let pythia_program = PythiaTemplate {
        import_paths: get_import_paths(&record_types),
        record_types: record_types.to_vec(),
    }
    .render()?;

    fs::write(PYTHIA_PROGRAM_PATH, pythia_program)?;

    Ok(())
}

fn get_import_paths(record_types: &Vec<RecordTypeData>) -> Vec<String> {
    record_types
        .iter()
        .map(get_fact_program_file_path)
        .collect::<Vec<_>>()
}

/// Generates and writes to:
///
/// - `data/<record type>.pl` for the facts of each record type (Askama template).
///
/// Returns an error if this fails to read/create files, or could not render the Prolog template.
pub fn generate_fact_programs(
    record_type: &RecordTypeData,
    facts: Vec<FactData>,
) -> Result<(), CodeGenError> {
    let file_path = get_fact_program_file_path(record_type);
    let fact_program = FactTemplate { record_type, facts }.render()?;

    fs::write(file_path, fact_program)?;
    Ok(())
}

fn get_fact_program_file_path(record_type: &RecordTypeData) -> String {
    format!("data/{}.pl", record_type.name)
}

use std::{
    fs,
    io::{self},
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

/// Represents a single item in the knowledge base, containing a file path and its contents.
pub struct KnowledgeBaseItem {
    pub file_path: String,
    pub contents: String,
}

/// Represents a knowledge base containing Prolog code for record type definitions,
/// the Pythia program, and facts for various record types.
pub struct KnowledgeBase {
    pub record_type_definitions: KnowledgeBaseItem,
    pub pythia_program: KnowledgeBaseItem,
    pub fact_programs: Vec<KnowledgeBaseItem>,
}

impl KnowledgeBase {
    /// Creates a new `KnowledgeBase` instance with the provided record type definitions,
    /// Pythia program, and a list of fact programs.
    pub fn new(
        record_type_definitions: KnowledgeBaseItem,
        pythia_program: KnowledgeBaseItem,
        fact_programs: Vec<KnowledgeBaseItem>,
    ) -> Self {
        Self {
            record_type_definitions,
            pythia_program,
            fact_programs,
        }
    }

    /// Returns the main Pythia program as a string, with all fact programs inlined.
    pub fn get_pythia_program_with_facts_inline(&self) -> String {
        let mut replaced = self.pythia_program.contents.clone();

        for fact_program in &self.fact_programs {
            replaced = replaced.replace(
                &format!(":- use_module('{}').", fact_program.file_path),
                &fact_program.contents,
            );
        }

        replaced
    }

    /// Returns the record type definitions JSON as a vector of `RecordTypeData`.
    pub fn get_record_types(&self) -> Result<Vec<RecordTypeData>, CodeGenError> {
        Ok(serde_json::from_str(
            &self.record_type_definitions.contents,
        )?)
    }
}

const TYPES_JSON_PATH: &str = "data/types.json";
const PYTHIA_PROGRAM_PATH: &str = "data/internal/pythia.pl";

/// Generates file_path/content pairs in the form of a knowledge base for Prolog.
///
/// - `data/types.json` for record type defitions and main 'Pythia' prolog program for state change predicates.
/// - `data/internal/pythia.pl` for state change predicates (Askama template).
/// - `data/<record type>.pl` for the facts of the given record type (Askama template).
///
/// Returns an error if this fails to read/create files, could not render the Prolog template, or
/// encounters invalid JSON.
pub fn generate_knowledge_base(
    rts: &Vec<RecordTypeData>,
    rt_facts: &Vec<(RecordTypeData, Vec<FactData>)>,
) -> Result<KnowledgeBase, CodeGenError> {
    let record_type_definitions = KnowledgeBaseItem {
        file_path: TYPES_JSON_PATH.to_owned(),
        contents: serde_json::to_string_pretty(rts)?,
    };
    let pythia_program = KnowledgeBaseItem {
        file_path: PYTHIA_PROGRAM_PATH.to_owned(),
        contents: PythiaTemplate {
            import_paths: get_import_paths(rts),
            record_types: rts.to_vec(),
        }
        .render()?,
    };

    let fact_programs = rt_facts
        .iter()
        .map(|(rt, fs)| {
            Ok(KnowledgeBaseItem {
                file_path: get_fact_program_file_path(rt),
                contents: FactTemplate {
                    record_type: rt,
                    facts: fs.to_vec(),
                }
                .render()?,
            })
        })
        .collect::<Result<Vec<_>, crate::utils::codegen::CodeGenError>>()?;

    Ok(KnowledgeBase::new(
        record_type_definitions,
        pythia_program,
        fact_programs,
    ))
}

fn get_import_paths(record_types: &Vec<RecordTypeData>) -> Vec<String> {
    record_types
        .iter()
        .map(get_fact_program_file_path)
        .collect::<Vec<_>>()
}

/// Returns the file path in the knowledge base for the Prolog program file for a given record type.
pub fn get_fact_program_file_path(record_type: &RecordTypeData) -> String {
    format!("data/{}.pl", record_type.name)
}

/// Exports the knowledge base to the file system.
/// This will create the necessary directories and write the record type definitions,
/// Pythia program, and fact programs to their respective files.
/// Returns an error if it fails to create directories or write files.
pub fn export_knowledge_base(kb: &KnowledgeBase) -> Result<(), CodeGenError> {
    fs::create_dir_all("data/internal")?;
    fs::write(
        &kb.record_type_definitions.file_path,
        &kb.record_type_definitions.contents,
    )?;
    fs::write(&kb.pythia_program.file_path, &kb.pythia_program.contents)?;

    for fact_program in &kb.fact_programs {
        fs::write(&fact_program.file_path, &fact_program.contents)?;
    }

    Ok(())
}

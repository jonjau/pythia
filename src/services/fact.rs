use crate::models::fact::{Fact, FactTerm};
use crate::models::logic_machine::LogicMachineError;
use crate::models::record_type::RecordTypeError;
use crate::services::db::{DbService, DbServiceError};
use crate::services::logic_machine::LogicMachineService;
use std::collections::HashMap;

/// Errors that can occur during getting and fetching facts from the LogicMachine.
#[derive(thiserror::Error, Debug)]
pub enum FactServiceError {
    #[error("LogicMachine error: {0}")]
    FactError(#[from] LogicMachineError),
    #[error("RecordType error: {0}")]
    RecordTypeError(#[from] RecordTypeError),
    #[error("Database error: {0}")]
    DbServiceError(#[from] DbServiceError),
}

/// Represents a simple in-memory fact table, with columns and row data.
///
/// This structure holds tabular data where each row is a list of strings,
/// and each string in `rows[i]` corresponds to the column defined in `columns[i]`.
#[derive(Default)]
pub struct FactTableData {
    /// The names of the columns in the fact table.
    ///
    /// Each entry corresponds to a field in the row vectors.
    pub columns: Vec<String>,

    /// The rows of the fact table, stored as a vector of string vectors.
    ///
    /// Each inner vector should have the same length as `columns`, where
    /// `rows[i][j]` is the value for column `columns[j]` in row `i`.
    pub rows: Vec<Vec<String>>,
}

/// A high-level wrapper around the [`LogicMachineService`] for working with fact tables.
///
/// This service provides utilities for adding and fetching facts
/// through the underlying logic engine.
///
/// Cloneable for convenience in concurrent or async contexts.
#[derive(Clone)]
pub struct FactService {
    lm: LogicMachineService,
    db: DbService,
}

impl FactService {
    /// Creates a new `FactService` from a given [`LogicMachineService`] instance.
    ///
    /// # Arguments
    ///
    /// * `lm` - The logic machine service used for fact storage and querying.
    pub fn new(lm: LogicMachineService, db: DbService) -> Self {
        FactService { lm, db }
    }

    /// Retrieves all facts for a given record type name.
    ///
    /// # Arguments
    ///
    /// * `rt_name` - The name of the record type to retrieve facts for.
    ///
    /// # Returns
    ///
    /// A `FactTableData` containing the column names and all row data for the record type.
    ///
    /// # Errors
    ///
    /// Returns an error if the record type does not exist or if fact retrieval fails.
    pub async fn get_facts(&self, rt_name: String) -> Result<FactTableData, FactServiceError> {
        let facts = self.lm.get_all_facts(rt_name.clone()).await?;

        let rt = self.lm.get_record_type(rt_name).await?;
        let columns = rt.all_fields();

        let rows = facts
            .iter()
            .map(|f| {
                f.to_all_values()
                    .into_iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        Ok(FactTableData { columns, rows })
    }

    /// Adds multiple facts to a given record type.
    ///
    /// # Arguments
    ///
    /// * `rt_name` - The name of the record type.
    /// * `named_valuess` - A list of field-value maps, one per fact to add.
    ///
    /// # Returns
    ///
    /// A list of added [`Fact`]s.
    ///
    /// # Errors
    ///
    /// Returns an error if record type lookup, or fact creation/persistence fails.
    pub async fn add_facts(
        &self,
        rt_name: &str,
        named_valuess: Vec<HashMap<String, String>>,
    ) -> Result<Vec<Fact>, FactServiceError> {
        let mut facts = Vec::new();

        for values in named_valuess {
            let rt = self.lm.get_record_type(rt_name).await?;
            let named_values = values
                .into_iter()
                .map(|(k, v)| {
                    (
                        format!("{}0_{}", rt.name.to_uppercase(), k),
                        FactTerm::String(v),
                    )
                })
                .collect::<HashMap<_, _>>();

            let f = rt.to_fact(&named_values.into())?;

            self.db.put_fact(&f.clone().into()).await?;

            facts.push(f);
        }

        Ok(facts)
    }

    /// Adds a single fact for the given record type
    ///
    /// # Arguments
    ///
    /// * `rt_name` - The name of the record type to add a fact for.
    /// * `named_values` - A mapping of field names to values as strings.
    ///
    /// # Returns
    ///
    /// The newly created [`Fact`] on success.
    ///
    /// # Errors
    ///
    /// Returns an error if record type lookup, fact creation, or database fails.
    pub async fn add_fact(
        &self,
        rt_name: &str,
        named_values: HashMap<String, String>,
    ) -> Result<Fact, FactServiceError> {
        let f = self.add_facts(rt_name, vec![named_values]).await?[0].to_owned();
        Ok(f)
    }
}

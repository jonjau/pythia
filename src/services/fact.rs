use std::collections::HashMap;
use std::fs::OpenOptions;
use std::io::{self, Write};

use crate::models::fact::{Fact, FactTerm};
use crate::models::logic_machine::LogicMachineError;
use crate::models::record_type::RecordTypeError;
use crate::services::logic_machine::LogicMachineService;

#[derive(thiserror::Error, Debug)]
pub enum FactServiceError {
    #[error("LogicMachine error: {0}")]
    FactError(#[from] LogicMachineError),
    #[error("RecordType error: {0}")]
    RecordTypeError(#[from] RecordTypeError),
    #[error("File error: {0}")]
    IoError(#[from] io::Error),
}

#[derive(Default)]
pub struct FactTableData {
    pub columns: Vec<String>,
    pub rows: Vec<Vec<String>>,
}

#[derive(Clone)]
pub struct FactService {
    lm: LogicMachineService,
}

impl FactService {
    pub fn new(lm: LogicMachineService) -> Self {
        FactService { lm }
    }

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

    async fn add_fact_to_lm(
        &self,
        rt_name: &str,
        named_values: HashMap<String, String>,
    ) -> Result<Fact, FactServiceError> {
        let rt: std::sync::Arc<crate::models::record_type::RecordType> =
            self.lm.get_record_type(rt_name).await?;

        let named_values = named_values
            .into_iter()
            .map(|(k, v)| {
                (
                    format!("{}0_{}", rt.name.to_uppercase(), k),
                    FactTerm::String(v),
                )
            })
            .collect::<HashMap<_, _>>();

        let fact = rt.to_fact(&named_values.into())?;

        let res = self.lm.add_fact(fact).await?;
        Ok(res)
    }

    pub async fn add_facts(
        &self,
        rt_name: &str,
        named_valuess: Vec<HashMap<String, String>>,
    ) -> Result<Vec<Fact>, FactServiceError> {
        let mut file = OpenOptions::new()
            .append(true)
            .create(true)
            .open(format!("data/{}.pl", &rt_name))?;

        let iso_time = chrono::Utc::now().to_rfc3339();
        writeln!(file, "\n% [{}]", iso_time)?;

        let mut facts = Vec::new();

        for values in named_valuess {
            let f = self.add_fact_to_lm(&rt_name, values).await?;
            writeln!(file, "{}.", f.to_string())?;
            facts.push(f);
        }

        Ok(facts)
    }

    pub async fn add_fact(
        &self,
        rt_name: &str,
        named_values: HashMap<String, String>,
    ) -> Result<Fact, FactServiceError> {
        let f = self.add_facts(rt_name, vec![named_values]).await?[0].clone();
        Ok(f)
    }
}

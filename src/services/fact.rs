use std::collections::HashMap;
use std::fs::OpenOptions;
use std::io::Write;

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
}

#[derive(Clone)]
pub struct FactService {
    lm: LogicMachineService,
}

impl FactService {
    pub fn new(lm: LogicMachineService) -> Self {
        FactService { lm }
    }

    pub async fn get_facts(&self, rt_name: String) -> Result<Vec<String>, FactServiceError> {
        let facts = self.lm.get_all_facts(rt_name).await?;

        Ok(facts.iter().map(|f| f.to_string()).collect())
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
            .open(format!("data/{}.pl", &rt_name))
            .unwrap();

        let iso_time = chrono::Utc::now().to_rfc3339();
        writeln!(file, "\n% [{}]", iso_time).unwrap();

        let mut facts = Vec::new();

        for values in named_valuess {
            let f = self.add_fact_to_lm(&rt_name, values).await?;
            writeln!(file, "{}.", f.to_string()).unwrap();
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

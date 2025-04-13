use std::collections::HashMap;

use crate::models::fact::FactTerm;
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

    pub async fn add_fact(
        &self,
        rt_name: &str,
        named_values: HashMap<String, String>,
    ) -> Result<Vec<String>, FactServiceError> {
        let rt = self.lm.get_record_type(rt_name).await?;

        let named_values = named_values
            .into_iter()
            .map(|(k, v)| (format!("{}0_{}", rt.name.to_uppercase(), k), FactTerm::String(v)))
            .collect::<HashMap<_, _>>();

        dbg!(&named_values);

        let fact = rt.to_fact(&named_values.into())?;

        let res = self.lm.add_fact(fact).await?;
        Ok(res.iter().map(|f| f.to_string()).collect::<Vec<_>>())
    }
}

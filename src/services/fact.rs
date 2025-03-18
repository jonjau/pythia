use crate::services::logic_machine::LogicMachineService;
use crate::models::logic_machine::LogicMachineError;

#[derive(thiserror::Error, Debug)]
pub enum FactServiceError {
    #[error("LogicMachine error: {0}")]
    FactError(#[from] LogicMachineError),
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
}
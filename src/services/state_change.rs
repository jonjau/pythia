use super::fact::FactService;

#[derive(Clone)]
pub struct StateChangeService {
    facts: FactService
}

impl StateChangeService {
    pub fn new(facts: FactService) -> Self {
        StateChangeService { facts }
    }

}
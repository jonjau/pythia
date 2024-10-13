#[derive(Clone)]
pub struct StateService {
    pub start_state: String,
    pub end_state: String,
}


impl StateService {
    pub fn new() -> Self {
        StateService {
            start_state: "".to_string(),
            end_state: "".to_string()
        }
    }
}
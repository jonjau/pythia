use std::collections::HashMap;

use crate::models::fact::{Goal, RecordType};

#[derive(Clone)]
pub struct StateService {
    pub start_state: Option<Goal>,
    pub end_state: Option<Goal>,
}

impl StateService {
    pub fn new() -> Self {
        StateService {
            start_state: None,
            end_state: None,
        }
    }

    // pub fn update_start_state(&mut self, rt: RecordType, values: HashMap<&str, &str>) {
    //     self.start_state = rt.to_goal(&values).ok();
    // }
}

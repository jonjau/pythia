use std::{collections::HashMap, sync::Arc};

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

    pub fn update_start_state(&mut self, rt: Arc<RecordType>, values: HashMap<&str, &str>) {
        // self.states = States {
        //     start_state: rt.to_goal(&values).ok(),
        //     end_state: None,
        // }
        self.start_state = rt.to_goal(&values).ok();

        // self.states.

        // let updated_states = States {
        //     start_state: rt.to_goal(&values).ok(),
        //     end_state: self.states.end_state.clone(), // Preserve other fields
        // };
    }
}

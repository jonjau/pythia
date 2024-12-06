
use std::collections::HashMap;
use crate::models::fact::RecordType;

#[derive(Debug, Clone, PartialEq)]
pub struct State {
    record_type: RecordType,
    mapped_values: HashMap<String, String>
}

impl State {
    
}

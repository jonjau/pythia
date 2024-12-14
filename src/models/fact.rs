use scryer_prolog::machine::parsed_results::{
    prolog_value_to_json_string, QueryMatch, QueryResolution,
};
use scryer_prolog::machine::Machine;
use std::fmt::{Display, Formatter};
use std::{
    clone::Clone,
    collections::HashMap,
    sync::Arc,
};

// #[derive(PartialEq, Debug)]
#[derive(thiserror::Error, Debug, PartialEq)]
pub enum RecordTypeError {
    #[error("unknown field names: {}", .0.join(", "))]
    UnknownFieldNames(Vec<String>),
    #[error("ungrounded values: {}", .0.join(", "))]
    UngroundedValues(Vec<String>),
    #[error("field names not starting with uppercase ASCII: {}", .0.join(", "))]
    InvalidFieldNames(Vec<String>),
}

// TODO JCJ: need id and data fields
#[derive(Clone, Debug, PartialEq)]
pub struct RecordType {
    pub name: String,
    pub id_fields: Vec<String>,
    pub data_fields: Vec<String>,
    pub metadata_fields: Vec<String>
}

impl RecordType {
    pub fn new(name: &str, id_fields: &[&str], data_fields: &[&str], metadata_fields: &[&str]) -> Result<Self, RecordTypeError> {
        let invalid = id_fields
            .iter()
            .chain(data_fields.iter())
            .chain(metadata_fields.iter())
            .filter(|&&field| !field.chars().next().is_some_and(|c| c.is_ascii_uppercase()))
            .map(|&f| f.to_string())
            .collect::<Vec<_>>();

        if !invalid.is_empty() {
            Err(RecordTypeError::InvalidFieldNames(invalid))
        } else {
            Ok(RecordType {
                name: name.into(),
                id_fields: id_fields.iter().cloned().map(Into::into).collect(),
                data_fields: data_fields.iter().cloned().map(Into::into).collect(),
                metadata_fields: metadata_fields.iter().cloned().map(Into::into).collect()
            })
        }
    }

    pub fn new_without_id_fields(name: &str, data_fields: &[&str]) -> Result<Self, RecordTypeError> {
        Self::new(name, &[], data_fields, &[])
    }

    pub fn all_fields(self: Arc<Self>) -> Vec<String> {
        self.id_fields.iter().chain(self.data_fields.iter()).chain(self.metadata_fields.iter()).cloned().collect()
    }

    pub fn to_most_general_goal(self: Arc<Self>) -> Goal {
        let values = self.clone().all_fields().iter().cloned().collect::<Vec<_>>();
        Goal::new(self, values)
    }

    pub fn to_goal(self: Arc<Self>, values: &HashMap<&str, &str>) -> Result<Goal, RecordTypeError> {
        let values = values
            .iter()
            .map(|(&k, &v)| (k.to_owned(), v.to_owned()))
            .collect::<HashMap<_, _>>();

        let mut unknown_values = values
            .keys()
            .filter(|&a| !self.data_fields.contains(&a.to_owned()))
            .peekable();
        if unknown_values.peek().is_some() {
            return Err(RecordTypeError::UnknownFieldNames(
                unknown_values.into_iter().cloned().collect::<Vec<_>>(),
            ));
        }

        let complete_values: Vec<String> = self
            .data_fields
            .iter()
            .map(|field| values.get(field).cloned().unwrap_or(field.to_owned()))
            .collect();
        dbg!(&complete_values);

        let g = Goal::new(self, complete_values);

        dbg!(g.query_str());

        Ok(g)
    }

    pub fn to_fact(self: Arc<Self>, values: &HashMap<&str, &str>) -> Result<Fact, RecordTypeError> {
        struct UnknownFieldName(String);
        let mut unknown = Vec::new();

        let values = values
            .iter()
            .map(|(&k, &v)| (k.to_owned(), v.to_owned()))
            .map(|(k, v)| {
                self.data_fields
                    .contains(&k)
                    .then_some((k.clone(), v))
                    .ok_or(UnknownFieldName(k))
            })
            .filter_map(|r| r.map_err(|UnknownFieldName(e)| unknown.push(e)).ok())
            .collect::<HashMap<_, _>>();

        if !unknown.is_empty() {
            return Err(RecordTypeError::UnknownFieldNames(unknown));
        }

        struct UngroundedValue(String);
        let mut ungrounded = Vec::new();
        let complete_values = self
            .data_fields
            .iter()
            .map(|field| {
                values
                    .get(field)
                    .cloned()
                    .ok_or(UngroundedValue(field.to_string()))
            })
            .filter_map(|r| r.map_err(|UngroundedValue(e)| ungrounded.push(e)).ok())
            .collect::<Vec<_>>();

        if !ungrounded.is_empty() {
            return Err(RecordTypeError::UngroundedValues(ungrounded));
        }

        Ok(Fact::new(self, complete_values))
    }
    // TODO: query() overload passing in vector of argument values in order
}

/// A Goal is a compound term which may not have all its values grounded.
/// It is meant to be run as a query to the logic machine.
#[derive(Clone, Debug, PartialEq)]
pub struct Goal {
    pub type_: Arc<RecordType>,
    pub values: Vec<String>,
}

impl Goal {
    pub fn new(type_: Arc<RecordType>, values: Vec<String>) -> Self {
        Goal {
            type_: Arc::clone(&type_),
            values,
        }
    }

    pub fn query_str(&self) -> String {
        if !self.values.is_empty() {
            format!(r#"{}({})"#, self.type_.name, self.values.join(", "))
        } else {
            format!(r#"{}"#, self.type_.name)
        }
    }

    pub fn to_values(&self) -> HashMap<String, String> {
        self.type_
            .data_fields
            .iter()
            .zip(self.values.iter())
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect::<HashMap<_, _>>()
    }
}

/// A Fact is a compound term which has all its values grounded.
/// It is meant to be asserted to the logic machine
#[derive(Debug, Clone, PartialEq)]
pub struct Fact {
    type_: Arc<RecordType>,
    values: Vec<String>,
}

impl Fact {
    pub fn new(type_: Arc<RecordType>, values: Vec<String>) -> Self {
        Fact {
            type_: Arc::clone(&type_),
            values,
        }
    }

    pub fn assertion_str(&self) -> String {
        if !self.values.is_empty() {
            format!(r#"{}({})"#, self.type_.name, self.values.join(", "))
        } else {
            format!(r#"{}"#, self.type_.name)
        }
    }

    pub fn data_fields(&self) -> Vec<String> {
        self.type_
            .data_fields
            .iter()
            .map(|field| field.to_string())
            .collect::<Vec<_>>()
    }

    pub fn type_name(&self) -> String {
        self.type_.name.clone()
    }
}

impl Display for Fact {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", self.type_.name, self.values.join(","))
    }
}

#[derive(thiserror::Error, Debug)]
pub enum LogicMachineError {
    #[error("unexpected query resolution")]
    UnexpectedQueryResolution,
    #[error("prolog error: {0}")]
    PrologError(String),
}

pub type LogicMachineResult = Result<Vec<Fact>, LogicMachineError>;
pub type RecordTypeResult = Result<Arc<RecordType>, String>;

pub struct LogicMachine {
    record_types: HashMap<String, RecordType>,
    machine: Machine,
}

impl LogicMachine {
    pub fn new<T: Into<String>>(program: T) -> LogicMachine {
        let mut machine = Machine::new_lib();
        machine.consult_module_string("module0", program.into());

        LogicMachine {
            record_types: HashMap::new(),
            machine,
        }
    }

    fn parse_to_facts<'a>(qr: QueryResolution, rt: Arc<RecordType>) -> LogicMachineResult {
        match qr {
            QueryResolution::Matches(m) => Ok(m
                .iter()
                .map(|QueryMatch { bindings: b }| {
                    Fact::new(
                        Arc::clone(&rt),
                        b.values()
                            .map(|v| prolog_value_to_json_string(v.clone()))
                            .collect::<Vec<_>>(),
                    )
                })
                .collect::<Vec<_>>()),
            _ => Err(LogicMachineError::UnexpectedQueryResolution),
        }
    }

    // Get predicate which has the given name
    pub fn get_record_type(&self, name: &str) -> Result<Arc<RecordType>, String> {
        self.record_types
            .get(name)
            .map(|record| Arc::new(record.clone()))
            .ok_or("RecordType not found".to_string())
    }

    pub fn define_types(&mut self, types: Vec<RecordType>) {
        for t in types {
            self.record_types.insert(t.name.clone(), t);
        }
    }

    pub fn add_fact(&mut self, f: Fact) -> LogicMachineResult {
        self
            .machine
            .run_query(format!(r#"assertz({})."#, f.assertion_str()))
            .map_err(LogicMachineError::PrologError)?;

        self.fetch_all(f.type_)
    }

    pub fn fetch_all(&mut self, rt: Arc<RecordType>) -> LogicMachineResult {
        self.fetch(Arc::clone(&rt).to_most_general_goal())
    }

    pub fn fetch(&mut self, g: Goal) -> LogicMachineResult {
        let qr = self
            .machine
            .run_query(format!(r#"{}."#, g.query_str()))
            .map_err(LogicMachineError::PrologError)?;

        Self::parse_to_facts(qr, Arc::clone(&g.type_))
    }
}

#[cfg(test)]
mod tests {
    // use scryer_prolog::machine::parsed_results::{
    //     self, prolog_value_to_json_string, QueryMatch, QueryResolution,
    // };
    // use std::collections::HashMap;
    // use std::sync::Arc;

    // use crate::models::fact::{Fact, RecordTypeError};

    // use super::{LogicMachine, RecordType};

    #[test]
    fn query() {
        // let mut lm = LogicMachine::new(String::from(r#"edge(0, 4)."#));
        // let edge = Arc::new(RecordType::new("edge", &["X", "Y"]).unwrap());
        // let res = lm.fetch(edge.to_goal(&HashMap::from([("X", "0")])).unwrap());
        // println!("{:?}", res);
    }

    #[test]
    fn fact() {
        // let mut lm = LogicMachine::new(String::from(
        //     r#"
        //     :- dynamic(edge/2).
        //     edge(0, 4).
        // "#,
        // ));

        // TODO: assert argument names start with uppercase ASCII letter
        // let edge = Arc::new(RecordType::new("edge", &["X", "Y"]).unwrap());

        // TODO: accept more than just strings for argument values
        // assert_eq!(
        //     Arc::clone(&edge).to_fact(&HashMap::from([("X", "0")])),
        //     Err(RecordTypeError::UngroundedValues(vec![String::from("Y")]))
        // );

        // assert_eq!(
        //     Arc::clone(&edge).to_fact(&HashMap::from([("XA", "0")])),
        //     Err(RecordTypeError::UnknownFieldNames(vec![String::from("XA")]))
        // );

        // let _ = lm.add_fact(
        //     Arc::clone(&edge)
        //         .to_fact(&HashMap::from([("X", "1"), ("Y", "7")]))
        //         .unwrap(),
        // );
        // let a = lm.fetch_all(Arc::clone(&edge));

        // dbg!(&a);
    }
}

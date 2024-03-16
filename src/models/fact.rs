use scryer_prolog::machine::{parsed_results::QueryResult, Machine};
use std::{
    clone::Clone,
    collections::{BTreeSet, HashMap},
    fmt::{self, Display},
    hash::Hash,
};

#[derive(PartialEq, Debug)]
pub enum RecordTypeError {
    UnknownAttrs(Vec<String>),
    UngroundedAttrs(Vec<String>),
}

impl Display for RecordTypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RecordTypeError::UnknownAttrs(attrs) => {
                write!(f, "unknown attributes: {}", attrs.join(", "))
            }
            RecordTypeError::UngroundedAttrs(attrs) => {
                write!(f, "ungrounded attributes: {}", attrs.join(", "))
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct RecordType {
    name: String,
    attr_names: BTreeSet<String>,
}

impl RecordType {
    pub fn new<T, U>(name: T, attr_names: &[U]) -> Self
    where
        T: Into<String>,
        U: Clone + Into<String>,
    {
        RecordType {
            name: name.into(),
            attr_names: attr_names.iter().cloned().map(Into::into).collect(),
        }
    }

    pub fn to_goal(&self, attrs: &HashMap<&str, &str>) -> Result<Goal, RecordTypeError> {
        let attrs = attrs
            .iter()
            .map(|(&k, &v)| (k.to_owned(), v.to_owned()))
            .collect::<HashMap<_, _>>();

        let mut unknown_attrs = attrs
            .keys()
            .filter(|&a| !self.attr_names.contains(&a.to_owned()))
            .peekable();
        if unknown_attrs.peek().is_some() {
            return Err(RecordTypeError::UnknownAttrs(
                unknown_attrs.into_iter().cloned().collect::<Vec<_>>(),
            ));
        }

        let complete_attrs: Vec<String> = self
            .attr_names
            .iter()
            .map(|attr_name| {
                attrs
                    .get(attr_name)
                    .cloned()
                    .unwrap_or(format!(r#"V_{}"#, attr_name))
            })
            .collect();

        Ok(Goal::new(self, complete_attrs))
    }

    pub fn to_fact(&self, attrs: &HashMap<&str, &str>) -> Result<Fact, RecordTypeError> {
        let attrs = attrs
            .iter()
            .map(|(&k, &v)| (k.to_owned(), v.to_owned()))
            .collect::<HashMap<_, _>>();

        let mut unknown_attrs = attrs
            .keys()
            .filter(|&a| !self.attr_names.contains(&a.to_owned()))
            .peekable();
        if unknown_attrs.peek().is_some() {
            return Err(RecordTypeError::UnknownAttrs(
                unknown_attrs.into_iter().cloned().collect::<Vec<_>>(),
            ));
        }

        struct UngroundedAttr(String);
        let mut ungrounded = Vec::new();
        let complete_attrs = self
            .attr_names
            .iter()
            .map(|attr_name| {
                attrs
                    .get(attr_name)
                    .cloned()
                    .ok_or(UngroundedAttr(attr_name.to_string()))
            })
            .filter_map(|r| r.map_err(|UngroundedAttr(e)| ungrounded.push(e)).ok())
            .collect::<Vec<_>>();

        if !ungrounded.is_empty() {
            return Err(RecordTypeError::UngroundedAttrs(ungrounded));
        }

        Ok(Fact::new(self, complete_attrs))
    }
    // TODO: query() overload passing in vector of argument values in order
}

#[derive(Debug, PartialEq)]
pub struct Goal<'a> {
    type_: &'a RecordType,
    attrs: Vec<String>,
}

impl<'a> Goal<'a> {
    pub fn new(type_: &'a RecordType, attrs: Vec<String>) -> Self {
        Goal { type_, attrs }
    }

    pub fn query_str(&self) -> String {
        if !self.attrs.is_empty() {
            format!(r#"{}({})"#, self.type_.name, self.attrs.join(", "))
        } else {
            format!(r#"{}"#, self.type_.name)
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Fact<'a> {
    type_: &'a RecordType,
    attrs: Vec<String>,
}

impl<'a> Fact<'a> {
    pub fn new(type_: &'a RecordType, attrs: Vec<String>) -> Self {
        Fact { type_, attrs }
    }

    pub fn assertion_str(&self) -> String {
        if !self.attrs.is_empty() {
            format!(r#"{}({})"#, self.type_.name, self.attrs.join(", "))
        } else {
            format!(r#"{}"#, self.type_.name)
        }
    }
}

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

    pub fn predicate(&self, name: &str) -> Option<&RecordType> {
        self.record_types.get(name)
    }

    pub fn define_types(&mut self, types: &mut Vec<RecordType>) {
        // self.predicates.insert(k, v)

        // TODO: assert predicates are loaded by fetching them
    }

    pub fn add_fact(&mut self, f: Fact) -> QueryResult {
        self.machine.run_query(f.assertion_str())
    }

    pub fn fetch_all(&mut self, rt: RecordType) -> QueryResult {
        self.fetch(
            rt.to_goal(&HashMap::new())
                .expect("No args must successfully return a goal"),
        )
    }

    pub fn fetch(&mut self, g: Goal) -> QueryResult {
        self.machine.run_query(format!(r#"{}."#, g.query_str()))
    }
}

impl Clone for LogicMachine {
    fn clone(&self) -> Self {
        let mut machine = Machine::new_lib();

        // self.record_types.values().map(|&t| {
        //     let facts = self.fetch_all(t);
        //     // machine.add_facts(facts)
        // });

        todo!()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::models::fact::RecordTypeError;

    use super::{LogicMachine, RecordType};

    #[test]
    fn it_works() {
        let mut lm = LogicMachine::new(String::from(r#"edge(3, 4)."#));

        let res = lm.machine.run_query(String::from("edge(3, X)."));
        println!("{:?}", res);

        let res = lm.machine.run_query(String::from("edge(X, Y)."));
        println!("{:?}", res);
    }

    #[test]
    fn query() {
        let mut lm = LogicMachine::new(String::from(r#"edge(0, 4)."#));

        // TODO: assert argument names start with uppercase ASCII letter
        let edge = RecordType::new("edge", &["X", "Y"]);

        // TODO: accept more than just strings for argument values
        let res = lm.fetch(edge.to_goal(&HashMap::from([("X", "0")])).unwrap());
        println!("{:?}", res);
    }

    #[test]
    fn fact() {
        // TODO: assert argument names start with uppercase ASCII letter
        let edge = RecordType::new("edge", &["X", "Y"]);

        // TODO: accept more than just strings for argument values
        assert_eq!(
            edge.to_fact(&HashMap::from([("X", "0")])),
            Err(RecordTypeError::UngroundedAttrs(vec![String::from("Y")]))
        );

        assert_eq!(
            edge.to_fact(&HashMap::from([("XA", "0")])),
            Err(RecordTypeError::UnknownAttrs(vec![String::from("XA")]))
        );
    }
}

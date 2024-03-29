use scryer_prolog::machine::{parsed_results::QueryResult, Machine};
use std::{
    clone::Clone,
    collections::{BTreeSet, HashMap},
    fmt::{self, Display},
};


#[derive(PartialEq, Debug)]
pub enum RecordTypeError {
    UnknownAttrNames(Vec<String>),
    UngroundedAttrs(Vec<String>),
    InvalidAttrNames(Vec<String>),
}

impl Display for RecordTypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RecordTypeError::UnknownAttrNames(attrs) => {
                write!(f, "unknown attribute names: {}", attrs.join(", "))
            }
            RecordTypeError::UngroundedAttrs(attrs) => {
                write!(f, "ungrounded attributes: {}", attrs.join(", "))
            }
            RecordTypeError::InvalidAttrNames(attrs) => {
                write!(
                    f,
                    "attribute names not starting with uppercase ASCII: {}",
                    attrs.join(", ")
                )
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
            return Err(RecordTypeError::UnknownAttrNames(
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
                    .unwrap_or(attr_name.to_owned())
            })
            .collect();

        Ok(Goal::new(self, complete_attrs))
    }

    pub fn to_fact(&self, attrs: &HashMap<&str, &str>) -> Result<Fact, RecordTypeError> {
        // if attrs.get("").is_some() {
        //     return Err(RecordTypeError::InvalidAttrNames(vec!["".to_string()]))
        // }

        struct UnknownAttrName(String);
        struct InvalidAttrName(String);
        let mut unknown = Vec::new();
        let mut invalid = Vec::new();

        let attrs = attrs
            .iter()
            .map(|(&k, &v)| (k.to_owned(), v.to_owned()))
            .map(|(k, v)| {
                self.attr_names
                    .contains(&k)
                    .then_some((k.clone(), v))
                    .ok_or(UnknownAttrName(k))
            })
            .filter_map(|r| r.map_err(|UnknownAttrName(e)| unknown.push(e)).ok())
            .map(|(k, v)| {
                k.chars()
                    .next()
                    .is_some_and(|c| c.is_ascii_uppercase())
                    .then_some((k.clone(), v))
                    .ok_or(InvalidAttrName(k))
            })
            .filter_map(|r| r.map_err(|InvalidAttrName(e)| invalid.push(e)).ok())
            .collect::<HashMap<_, _>>();

        if !unknown.is_empty() {
            return Err(RecordTypeError::UnknownAttrNames(unknown));
        }

        if !invalid.is_empty() {
            return Err(RecordTypeError::InvalidAttrNames(invalid));
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
    pub machine: Machine,
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
        // let m = self.machine;//.get_mut();
        self.machine.run_query(format!(r#"assertz({})."#, f.assertion_str()))
    }

    pub fn fetch_all(&mut self, rt: RecordType) -> QueryResult {
        self.fetch(
            rt.to_goal(&HashMap::new())
                .expect("No args must successfully return a goal"),
        )
    }

    pub fn fetch(&mut self, g: Goal) -> QueryResult {
        // let m = self.machine;//.get_mut();
        self.machine.run_query(format!(r#"{}."#, g.query_str()))
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

        // let res = lm.machine.run_query(String::from("edge(3, X)."));
        // println!("{:?}", res);

        // let res = lm.machine.run_query(String::from("edge(X, Y)."));
        // println!("{:?}", res);
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
        let mut lm = LogicMachine::new(String::from(
            r#"
            :- dynamic(edge/2).
            edge(0, 4).
        "#,
        ));

        // TODO: assert argument names start with uppercase ASCII letter
        let edge = RecordType::new("edge", &["X", "Y"]);

        // TODO: accept more than just strings for argument values
        assert_eq!(
            edge.to_fact(&HashMap::from([("X", "0")])),
            Err(RecordTypeError::UngroundedAttrs(vec![String::from("Y")]))
        );

        assert_eq!(
            edge.to_fact(&HashMap::from([("XA", "0")])),
            Err(RecordTypeError::UnknownAttrNames(vec![String::from("XA")]))
        );

        let _ = lm.add_fact(
            edge.to_fact(&HashMap::from([("X", "1"), ("Y", "7")]))
                .unwrap(),
        );
        let a = lm.fetch_all(edge);

        println!("{:?}", a);
    }
}

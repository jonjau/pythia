use core::fmt;
use scryer_prolog::machine::parsed_results::{
    prolog_value_to_json_string, QueryMatch, QueryResolution,
};
use scryer_prolog::machine::Machine;
use std::fmt::Formatter;
use std::str::FromStr;
use std::{clone::Clone, collections::HashMap, sync::Arc};

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
    pub metadata_fields: Vec<String>,
}

impl RecordType {
    pub fn new(
        name: &str,
        id_fields: &[&str],
        data_fields: &[&str],
        metadata_fields: &[&str],
    ) -> Result<Self, RecordTypeError> {
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
                metadata_fields: metadata_fields.iter().cloned().map(Into::into).collect(),
            })
        }
    }

    pub fn new_without_id_fields(
        name: &str,
        data_fields: &[&str],
    ) -> Result<Self, RecordTypeError> {
        Self::new(name, &[], data_fields, &[])
    }

    pub fn all_fields(self: Arc<Self>) -> Vec<String> {
        self.id_fields
            .iter()
            .chain(self.data_fields.iter())
            .chain(self.metadata_fields.iter())
            .cloned()
            .collect()
    }

    // pub fn to_data_record_type(self: Arc<Self>) -> RecordType {
    //     RecordType {
    //         name: Arc::clone(&self).name.clone() + "_data",
    //         id_fields: Vec::new(),
    //         data_fields: self.data_fields.clone(),
    //         metadata_fields: Vec::new()
    //     }
    // }

    pub fn to_most_general_goal(self: Arc<Self>) -> Goal {
        let values = self
            .clone()
            .all_fields()
            .iter()
            .map(|f| GoalTerm::Variable(f.to_string()))
            .collect::<Vec<_>>();
        Goal::new(self, values)
    }

    pub fn to_goal(
        self: Arc<Self>,
        data_values: &HashMap<String, GoalTerm>,
    ) -> Result<Goal, RecordTypeError> {
        let mut unknown_values = data_values
            .keys()
            .filter(|&a| !self.data_fields.contains(&a.to_owned()))
            .peekable();
        if unknown_values.peek().is_some() {
            return Err(RecordTypeError::UnknownFieldNames(
                unknown_values.into_iter().cloned().collect::<Vec<_>>(),
            ));
        }

        let id_values = self
            .id_fields
            .iter()
            .map(|field| GoalTerm::Variable(field.to_string()));

        let complete_data_values = self.data_fields.iter().map(|field| {
            data_values
                .get(field)
                .cloned()
                .unwrap_or(GoalTerm::Variable(field.to_string()))
        });

        let metadata_values = self
            .id_fields
            .iter()
            .map(|field| GoalTerm::Variable(field.to_string()));

        let all_values = id_values
            .chain(complete_data_values)
            .chain(metadata_values)
            .collect::<Vec<_>>();

        let g = Goal::new(self, all_values);
        Ok(g)
    }

    pub fn to_fact(
        self: Arc<Self>,
        all_values: &HashMap<String, FactTerm>,
    ) -> Result<Fact, RecordTypeError> {
        // let mut unknown_values = data_values
        //     .keys()
        //     .filter(|&a| !self.data_fields.contains(&a.to_owned()))
        //     .peekable();
        // if unknown_values.peek().is_some() {
        //     return Err(RecordTypeError::UnknownFieldNames(
        //         unknown_values.into_iter().cloned().collect::<Vec<_>>(),
        //     ));
        // }

        struct UngroundedValue(String);
        let mut ungrounded = Vec::new();
        let complete_values = Arc::clone(&self)
            .all_fields()
            .iter()
            .map(|field| {
                all_values
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
}

/// A Goal is a compound term which may not have all its values grounded.
/// It is meant to be run as a query to the logic machine.
/// Values that are not grounded are to be left as a Term::Variable.
#[derive(Clone, Debug, PartialEq)]
pub struct Goal {
    pub type_: Arc<RecordType>,
    pub values: Vec<GoalTerm>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum GoalTerm {
    Variable(String),
    String(String),
    List(Vec<GoalTerm>),
    SubTerm(Goal),
}

impl Goal {
    pub fn new(type_: Arc<RecordType>, values: Vec<GoalTerm>) -> Self {
        Goal {
            type_: Arc::clone(&type_),
            values,
        }
    }

    pub fn to_all_values(&self) -> Vec<GoalTerm> {
        self.values
            .iter()
            .take(self.type_.clone().all_fields().len())
            .cloned()
            .collect::<Vec<_>>()
    }

    pub fn to_data_values(&self) -> HashMap<String, GoalTerm> {
        self.type_
            .data_fields
            .iter()
            .zip(self.values.iter())
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect::<HashMap<_, _>>()
    }

    pub fn to_data_value_list(&self) -> GoalTerm {
        GoalTerm::List(
            self.values
                .iter()
                .skip(self.type_.id_fields.len())
                .take(self.type_.data_fields.len())
                .cloned()
                .collect::<Vec<_>>(),
        )
    }

    pub fn and(&self, goal2: Goal) -> Goal {
        let conjunction =
            Arc::new(RecordType::new_without_id_fields(",", &["Term1", "Term2"]).unwrap());

        conjunction
            .to_goal(&HashMap::from([
                ("Term1".to_string(), GoalTerm::SubTerm(self.clone())),
                ("Term2".to_string(), GoalTerm::SubTerm(goal2)),
            ]))
            .unwrap()
    }
}

impl fmt::Display for Goal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let values = self
            .to_all_values()
            .iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>();

        if !values.is_empty() {
            write!(f, "'{}'({})", self.type_.name, values.join(", "))
        } else {
            write!(f, "{}", self.type_.name)
        }
    }
}

impl fmt::Display for GoalTerm {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GoalTerm::Variable(var_name) => write!(f, "{}", var_name),
            GoalTerm::String(s) => write!(f, "\"{}\"", s),
            GoalTerm::List(ss) => write!(
                f,
                "[{}]",
                ss.iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            GoalTerm::SubTerm(g) => write!(f, "{}", g.to_string()),
        }
    }
}

/// A Fact is a compound term which has all its values grounded.
/// It is meant to be asserted to or by the logic machine
#[derive(Debug, Clone, PartialEq)]
pub struct Fact {
    type_: Arc<RecordType>,
    values: Vec<FactTerm>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FactTerm {
    String(String),
    Integer(i32),
    Float(f64),
    List(Vec<FactTerm>),
    SubTerm(Fact),
}

impl Fact {
    pub fn new(type_: Arc<RecordType>, values: Vec<FactTerm>) -> Self {
        Fact {
            type_: Arc::clone(&type_),
            values,
        }
    }

    pub fn to_all_values(&self) -> Vec<FactTerm> {
        self.values
            .iter()
            .take(self.type_.clone().all_fields().len())
            .cloned()
            .collect::<Vec<_>>()
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

impl fmt::Display for Fact {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let values = self
            .to_all_values()
            .iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>();

        if !values.is_empty() {
            write!(f, "'{}'({})", self.type_.name, values.join(", "))
        } else {
            write!(f, "'{}'", self.type_.name)
        }
    }
}

impl fmt::Display for FactTerm {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            FactTerm::String(s) => write!(f, "\"{}\"", s),
            FactTerm::Integer(i) => write!(f, "{}", i),
            FactTerm::Float(flt) => write!(f, "{}", flt),
            FactTerm::List(ss) => write!(
                f,
                "[{}]",
                ss.iter()
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            FactTerm::SubTerm(st) => write!(f, "{}", st),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseFactTermError;

impl FromStr for FactTerm {
    type Err = ParseFactTermError;

    fn from_str(str: &str) -> Result<Self, Self::Err> {
        let s = str.trim();

        if s.starts_with("\"") && s[1..].ends_with("\"") {
            return Ok(FactTerm::String(
                s[1..s.len() - 1].to_string(), // Remove quotes
            ));
        }

        if let Ok(int) = s.parse::<i32>() {
            return Ok(FactTerm::Integer(int));
        }

        if let Ok(float) = s.parse::<f64>() {
            return Ok(FactTerm::Float(float));
        }

        if s.starts_with("[") && s.ends_with("]") {
            let inner = &s[1..s.len() - 1];
            let inner_terms = inner
                .split(",")
                .into_iter()
                .map(|t| t.trim().parse::<FactTerm>())
                .collect::<Result<Vec<_>, ParseFactTermError>>();

            return inner_terms.and_then(|terms| Ok(FactTerm::List(terms)));
        }

        if let Some(type_name_start) = s.find("'") {
            if let Some(type_name_end) = s[type_name_start + 1..].find("'") {
                let type_name = &s[type_name_start + 1..type_name_start + 1 + type_name_end];

                if s[type_name_start + 1 + type_name_end + 1..].starts_with("(") && s.ends_with(")")
                {
                    let inner = &s[type_name_start + 1 + type_name_end + 1 + 1..s.len() - 1];

                    dbg!(&inner);

                    let inner_terms = inner
                        .split(",")
                        .into_iter()
                        .map(|t| t.trim().parse::<FactTerm>())
                        .collect::<Result<Vec<_>, ParseFactTermError>>();

                    dbg!(&inner_terms);

                    return inner_terms.and_then(|terms| {
                        let fields = (0..terms.len() - 1)
                            .map(|i| format!("{}_{}", type_name.to_uppercase(), i.to_string()))
                            .collect::<Vec<_>>();

                        let rt = RecordType::new_without_id_fields(
                            type_name,
                            &fields.iter().map(|f| f.as_str()).collect::<Vec<_>>(),
                        )
                        .unwrap();

                        Ok(FactTerm::SubTerm(Fact::new(Arc::new(rt), terms)))
                    });
                }
            }
        }

        Err(ParseFactTermError)
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
                    dbg!(&b);

                    let map = b
                        .into_iter()
                        .map(|(k, v)| {
                            let to_parse = prolog_value_to_json_string(v.clone());
                            dbg!(&to_parse);
                            (k.clone(), to_parse.parse::<FactTerm>().unwrap())
                        })
                        .collect::<HashMap<_, _>>();

                    Arc::clone(&rt).to_fact(&map).unwrap()
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
        self.machine
            .run_query(format!(r#"assertz({})."#, f.to_string()))
            .map_err(LogicMachineError::PrologError)?;

        self.fetch_all(f.type_)
    }

    pub fn fetch_all(&mut self, rt: Arc<RecordType>) -> LogicMachineResult {
        self.fetch(Arc::clone(&rt).to_most_general_goal(), Arc::clone(&rt))
    }

    pub fn fetch(&mut self, g: Goal, target_rt: Arc<RecordType>) -> LogicMachineResult {
        dbg!(g.to_string());
        let qr = self
            .machine
            .run_query(format!(r#"{}."#, g.to_string()))
            .map_err(LogicMachineError::PrologError)?;

        let a = Self::parse_to_facts(qr, Arc::clone(&target_rt));
        dbg!(&a);
        a
    }
}

#[cfg(test)]
mod tests {
    use super::{LogicMachine, RecordType};
    use crate::models::fact::{FactTerm, GoalTerm};
    use std::collections::HashMap;
    use std::sync::Arc;

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

    #[test]
    fn term_list() {
        let mut lm = LogicMachine::new(String::from(
            r#"
                :- use_module(library(clpz)).
                :- dynamic(dimlink/9).

                dimlink("Test1", "MR00000001", "ID00000001", "JH00000001", "2023-02-08", "2023-02-10", "2024-02-18 08:16:11", "D", "0"). 
                dimlink("Test1", "MR00000001", "ID00000001", "JH00000001", "2023-02-09", "2023-02-10", "2024-02-18 08:17:11", "E", "1"). 
                dimlink("Test1", "MR00000002", "ID00000002", "JH00000001", "2023-02-08", "2023-02-11", "2024-02-18 08:20:11", "D", "0").
                dimlink("Test1", "MR00000002", "ID00000002", "JH00000001", "2023-02-08", "2023-02-11", "2024-02-18 08:20:12", "D", "1"). 
                dimlink("Test1", "MR00000002", "ID00000002", "JH00000001", "2023-02-08", "2023-02-11", "2024-02-18 08:20:13", "E", "2"). 
                dimlink("Test1", "MR00000003", "ID00000002", "JH00000002", "2023-02-08", "2023-02-09", "2024-02-18 08:20:14", "O", "0"). 
                dimlink("Test1", "MR00000004", "ID00000001", "JH00000002", "2023-02-08", "2023-02-09", "2024-02-18 09:17:11", "O", "0").
                dimlink("Test1", "MR00000005", "ID00000001", "JH00000003", "2023-02-08", "2023-02-09", "2024-02-18 09:17:11", "D", "0").
                dimlink("Test1", "MR00000005", "ID00000001", "JH00000003", "2023-02-08", "2023-02-09", "2024-02-18 09:17:11", "V", "1").
            
                table("dimlink", "MgrLinkRef", ["DimIdRef", "InvHeadRef", "BegPeriod", "EndPeriod"]).
                
                record(Context, SysVersion, SeqNum, RecType, Id, [DimIdRef, InvHeadRef, BegPeriod, EndPeriod]) :-
                    dimlink(Context, Id, DimIdRef, InvHeadRef, BegPeriod, EndPeriod, SysVersion, RecType, SeqNum).

                step_change(Ctx, Id, Vals1, Vals2) :-
                    record(Ctx, _, SeqNum1, _, Id, Vals1),
                    record(Ctx, _, SeqNum2, _, Id, Vals2),
                    number_chars(Num1, SeqNum1),
                    number_chars(Num2, SeqNum2),
                    Num2 #= Num1 + 1.
        "#,
        ));

        let step_change = Arc::new(
            RecordType::new_without_id_fields("step_change", &["Ctx", "Id", "Vals1", "Vals2"])
                .unwrap(),
        );

        let g = step_change
            .to_goal(&HashMap::from([(
                "Vals1".to_string(),
                GoalTerm::List(vec![
                    GoalTerm::Variable("DimIdRef".to_string()),
                    GoalTerm::String("JH00000001".to_string()).into(),
                    GoalTerm::Variable("BegPeriod".to_string()),
                    GoalTerm::Variable("EndPeriod".to_string()),
                ])
                .into(),
            )]))
            .unwrap();

        let s = g.to_string();
        dbg!(&s);

        let res = lm.machine.run_query(format!("{}.", g.to_string()));
        // let res = lm.machine.run_query(
        //     "step_change(Ctx, Id, [DimIdRef, \"JH00000001\", BegPeriod, EndPeriod], Vals2)."
        //         .to_string(),
        // );
        dbg!(&res);

        // let res = lm.fetch(g);

        // dbg!(&res);
    }

    #[test]
    fn parse_fact() {
        // let fs = "[\"3\", 3, 3.0, 3.1, 0, -1, \"-12\", -111111111111111111111111111.1]".parse::<FactTerm>();
        let fs = "'goal'(\"3\", 3.4)".parse::<FactTerm>();

        dbg!(&fs);
    }
}

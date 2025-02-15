use core::fmt;
use nom::{
    branch::alt,
    bytes::complete::escaped,
    character::complete::{char, digit1, multispace0, none_of, one_of},
    combinator::{map, map_res, recognize},
    error::ParseError,
    multi::separated_list0,
    sequence::{delimited, separated_pair},
    IResult, Parser,
};
use scryer_prolog::machine::parsed_results::{
    prolog_value_to_json_string, QueryMatch, QueryResolution,
};
use scryer_prolog::machine::Machine;
use std::fmt::Formatter;
use std::str::FromStr;
use std::{clone::Clone, collections::HashMap, sync::Arc};

use crate::utils::tracking::IdContext;

#[derive(thiserror::Error, Debug, PartialEq)]
pub enum RecordTypeError {
    #[error("unknown field names: {}", .0.join(", "))]
    UnknownFieldNames(Vec<String>),
    #[error("ungrounded values: {}", .0.join(", "))]
    UngroundedValues(Vec<String>),
    #[error("field names not starting with uppercase ASCII: {}", .0.join(", "))]
    InvalidFieldNames(Vec<String>),
    #[error("missing goal name")]
    MissingGoalName,
    #[error("empty goal")]
    EmptyGoal,
}

#[derive(Clone, Debug, PartialEq)]
pub struct RecordType {
    term_id_ctx: IdContext,
    pub name: String,
    pub display_name: String,
    pub id_fields: Vec<String>,
    pub data_fields: Vec<String>,
    pub metadata_fields: Vec<String>,
}

pub struct RecordTypeBuilder {
    name: String,
    display_name: Option<String>,
    id_fields: Option<Vec<String>>,
    data_fields: Vec<String>,
    metadata_fields: Option<Vec<String>>,
}

impl RecordTypeBuilder {
    pub fn new(
        name: impl Into<String>,
        data_fields: impl IntoIterator<Item = impl Into<String>>,
    ) -> Self {
        Self {
            name: name.into(),
            display_name: None,
            id_fields: None,
            data_fields: data_fields.into_iter().map(Into::into).collect(),
            metadata_fields: None,
        }
    }

    pub fn display_name(mut self, display_name: impl Into<String>) -> Self {
        self.display_name = Some(display_name.into());
        self
    }

    pub fn id_fields(mut self, id_fields: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.id_fields = Some(id_fields.into_iter().map(Into::into).collect());
        self
    }

    pub fn metadata_fields(
        mut self,
        metadata_fields: impl IntoIterator<Item = impl Into<String>>,
    ) -> Self {
        self.metadata_fields = Some(metadata_fields.into_iter().map(Into::into).collect());
        self
    }

    pub fn build(self) -> Result<RecordType, RecordTypeError> {
        let display_name = self.display_name.unwrap_or_else(|| self.name.clone());
        let id_fields = self.id_fields.unwrap_or_default();
        let metadata_fields = self.metadata_fields.unwrap_or_default();

        let invalid = id_fields
            .iter()
            .chain(self.data_fields.iter())
            .chain(metadata_fields.iter())
            .filter(|&field| !field.chars().next().is_some_and(|c| c.is_ascii_uppercase()))
            .cloned()
            .collect::<Vec<_>>();

        if !invalid.is_empty() {
            Err(RecordTypeError::InvalidFieldNames(invalid))
        } else {
            Ok(RecordType {
                term_id_ctx: IdContext::new(),
                name: self.name,
                display_name,
                id_fields,
                data_fields: self.data_fields,
                metadata_fields,
            })
        }
    }
}

impl RecordType {
    pub fn all_fields(self: Arc<Self>) -> Vec<String> {
        self.id_fields
            .iter()
            .chain(self.data_fields.iter())
            .chain(self.metadata_fields.iter())
            .cloned()
            .collect()
    }

    fn get_term_id(&self, id_number: usize) -> String {
        self.display_name.clone().to_uppercase() + &id_number.to_string()
    }

    fn get_local_var(&self, id_number: usize, var_name: &str) -> GoalTerm {
        GoalTerm::Variable(format!("{}_{}", self.get_term_id(id_number), var_name))
    }

    pub fn to_most_general_goal(self: Arc<Self>) -> Goal {
        let id_number = self.term_id_ctx.next_id();
        let values = self
            .clone()
            .all_fields()
            .iter()
            .map(|f| self.get_local_var(id_number, f))
            .collect::<Vec<_>>();
        Goal::new(self.term_id_ctx.next_id(), self, values)
    }

    pub fn to_goal(self: Arc<Self>, data_values: Vec<GoalTerm>) -> Result<Goal, RecordTypeError> {
        Arc::clone(&self).to_goal_from_named_values(
            &Arc::clone(&self)
                .data_fields
                .iter()
                .zip(data_values)
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect::<HashMap<_, _>>(),
        )
    }

    pub fn to_goal_from_named_values(
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

        let id_num = self.term_id_ctx.next_id();

        let id_values = self
            .id_fields
            .iter()
            .map(|field| self.get_local_var(id_num, field));

        let complete_data_values = self.data_fields.iter().map(|field| {
            data_values
                .get(field)
                .cloned()
                .and_then(|gt| match gt {
                    // TODO: the "RT_" should involve some sort of constant (i.e. UUID) scoped to the LM
                    // need to change how we create/fetch RTs so that they all know the same LM's ID
                    GoalTerm::Variable(f) => Some(GoalTerm::Variable(format!("RT_{}", f))),
                    _ => Some(gt),
                })
                .unwrap_or(self.get_local_var(id_num, field))
        });

        let metadata_values = self
            .id_fields
            .iter()
            .map(|field| self.get_local_var(id_num, field));

        let all_values = id_values
            .chain(complete_data_values)
            .chain(metadata_values)
            .collect::<Vec<_>>();

        let g = Goal::new(id_num, self, all_values);
        Ok(g)
    }

    fn change_prefix<T: Clone>(
        map: &HashMap<String, T>,
        from_prefix: &str,
        to_prefix: &str,
    ) -> HashMap<String, T> {
        map.iter()
            .map(|(key, value)| {
                if key.starts_with(from_prefix) {
                    let new_key = format!("{}{}", to_prefix, &key[from_prefix.len()..]);
                    (new_key, value.clone())
                } else {
                    (key.clone(), value.clone())
                }
            })
            .collect()
    }

    fn filter_by_prefix<T: Clone>(map: &HashMap<String, T>, prefix: &str) -> HashMap<String, T> {
        map.iter()
            .filter(|(key, _)| key.starts_with(prefix))
            .map(|(key, value)| (key.clone(), value.clone()))
            .collect()
    }

    fn find_common_prefix<T>(map_pre: &HashMap<String, T>) -> Result<String, RecordTypeError> {
        let mut iter = map_pre.keys();
        let first_key = iter.next().ok_or(RecordTypeError::EmptyGoal)?;

        let common_prefix = iter.fold(first_key.clone(), |acc, key| {
            acc.chars()
                .zip(key.chars())
                .take_while(|(a, b)| a == b)
                .map(|(c, _)| c)
                .collect()
        });

        if common_prefix.is_empty() {
            Err(RecordTypeError::MissingGoalName)
        } else {
            Ok(common_prefix)
        }
    }

    fn strip_common_prefix<T: Clone>(
        common_prefix: String,
        map: &HashMap<String, T>,
    ) -> HashMap<String, T> {
        map.iter()
            .map(|(key, value)| (key[common_prefix.len()..].to_string(), value.clone()))
            .collect()
    }

    pub fn to_fact(
        self: Arc<Self>,
        all_values: &HashMap<String, FactTerm>,
    ) -> Result<Fact, RecordTypeError> {
        // TODO: instead of finding and stripping the common prefix,
        // we should compute the prefix from the recordtype
        // find mapped_values that start with that prefix,
        // then strip the common prefix among those mapped values

        let name = &self.display_name.clone().to_uppercase();

        let final_values =
            Self::change_prefix(&Self::filter_by_prefix(all_values, "RT_"), "RT_", "");

        let filtered = Self::filter_by_prefix(all_values, &name);

        let id_number = self.term_id_ctx.next_id();

        let goal_id = Self::find_common_prefix(&filtered)?;
        let stripped = Self::strip_common_prefix(goal_id, &filtered);
        let stripped = stripped
            .into_iter()
            .chain(final_values)
            .collect::<HashMap<_, _>>();

        struct UngroundedValue(String);
        let mut ungrounded = Vec::new();
        let complete_values = Arc::clone(&self)
            .all_fields()
            .iter()
            .map(|field| {
                stripped
                    .get(field)
                    .cloned()
                    .ok_or(UngroundedValue(field.to_string()))
            })
            .filter_map(|r| r.map_err(|UngroundedValue(e)| ungrounded.push(e)).ok())
            .collect::<Vec<_>>();

        if !ungrounded.is_empty() {
            return Err(RecordTypeError::UngroundedValues(ungrounded));
        }

        Ok(Fact::new(id_number, self, complete_values))
    }
}

/// A Goal is a compound term which may not have all its values grounded.
/// It is meant to be run as a query to the logic machine.
/// Values that are not grounded are to be left as a Term::Variable.
#[derive(Clone, Debug, PartialEq)]
pub struct Goal {
    pub id: String,
    pub type_: Arc<RecordType>,
    pub values: Vec<GoalTerm>,
}

/// a LocalVariable is local to the goal it is for
/// TODO: Variable can be scoped to the query or a particular goal that comprises it
#[derive(Clone, Debug, PartialEq)]
pub enum GoalTerm {
    Variable(String),
    String(String),
    Integer(i32),
    List(Vec<GoalTerm>),
    SubTerm(Goal),
}

impl Goal {
    fn new(id_number: usize, type_: Arc<RecordType>, values: Vec<GoalTerm>) -> Self {
        Goal {
            id: Arc::clone(&type_).get_term_id(id_number),
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

    pub fn and(self, goal2: Goal) -> Goal {
        let conjunction = Arc::new(
            RecordTypeBuilder::new(",", vec!["Term1", "Term2"])
                .display_name("comma")
                .build()
                .unwrap(),
        );

        let res = conjunction
            .to_goal_from_named_values(&HashMap::from([
                ("Term1".to_string(), GoalTerm::SubTerm(self)),
                ("Term2".to_string(), GoalTerm::SubTerm(goal2)),
            ]))
            .unwrap();

        res
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
            GoalTerm::Variable(var_name) => {
                write!(f, "{}", var_name)
            }
            GoalTerm::String(s) => write!(f, "\"{}\"", s),
            GoalTerm::Integer(i) => write!(f, "{}", i),
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
#[derive(Debug, Clone)]
pub struct Fact {
    id: String,
    type_: Arc<RecordType>,
    values: Vec<FactTerm>,
}

#[derive(Clone, Debug)]
pub enum FactTerm {
    String(String),
    Integer(i32),
    Float(f64),
    List(Vec<FactTerm>),
    SubTerm(Fact),
}

impl Fact {
    pub fn new(id_number: usize, type_: Arc<RecordType>, values: Vec<FactTerm>) -> Self {
        Fact {
            id: Arc::clone(&type_).get_term_id(id_number),
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

    fn to_data_values(&self) -> HashMap<String, FactTerm> {
        self.type_
            .data_fields
            .iter()
            .zip(self.values.iter())
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect::<HashMap<_, _>>()
    }

    pub fn get(&self, field: &str) -> Option<FactTerm> {
        self.to_data_values().get(field).cloned()
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

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
pub fn ws<'a, O, E: ParseError<&'a str>, F>(inner: F) -> impl Parser<&'a str, Output = O, Error = E>
where
    F: Parser<&'a str, Output = O, Error = E>,
{
    delimited(multispace0, inner, multispace0)
}

fn parse_fact_term(input: &str) -> IResult<&str, FactTerm> {
    alt((
        map(parse_string, FactTerm::String),
        map(parse_float, FactTerm::Float),
        map(parse_integer, FactTerm::Integer),
        map(parse_list, FactTerm::List),
        map(parse_subterm, FactTerm::SubTerm),
    ))
    .parse(input)
}

fn parse_string(input: &str) -> IResult<&str, String> {
    ws(delimited(
        char('"'),
        escaped(none_of(r#"\""#), '\\', one_of(r#""\"#)),
        char('"'),
    ))
    .parse(input)
    .map(|(rest, s)| (rest, s.to_string()))
}

fn parse_atom(input: &str) -> IResult<&str, String> {
    ws(delimited(
        char('\''),
        escaped(none_of(r#"\'"#), '\\', one_of(r#"'\"#)),
        char('\''),
    ))
    .parse(input)
    .map(|(rest, s)| (rest, s.to_string()))
}

fn parse_integer(input: &str) -> IResult<&str, i32> {
    map_res(ws(digit1), |s: &str| s.parse::<i32>()).parse(input)
}

fn parse_float(input: &str) -> IResult<&str, f64> {
    map_res(ws(recognize((digit1, char('.'), digit1))), |s: &str| {
        s.parse::<f64>()
    })
    .parse(input)
}

// Parser for List
fn parse_list(input: &str) -> IResult<&str, Vec<FactTerm>> {
    ws(delimited(
        char('['),
        separated_list0(char(','), parse_fact_term),
        char(']'),
    ))
    .parse(input)
}

// Parser for SubTerm (Fact)
fn parse_subterm(input: &str) -> IResult<&str, Fact> {
    let (input, (functor, args)) = separated_pair(
        parse_atom,
        char('('),
        separated_list0(char(','), parse_fact_term),
    )
    .parse(input)?;
    let (input, _) = char(')')(input)?;

    let rt = Arc::new(
        RecordTypeBuilder::new(
            functor.clone(),
            args.iter()
                .enumerate()
                .map(|(i, _)| format!("{}{}", functor.to_uppercase(), i))
                .collect::<Vec<_>>(),
        )
        .build()
        .unwrap(),
    );

    Ok((
        input,
        Fact {
            id: functor,
            type_: rt, // Replace with actual RecordType
            values: args,
        },
    ))
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseFactTermError;

impl FromStr for FactTerm {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_fact_term(s)
            .map(|(_, term)| term)
            .map_err(|e| format!("Parsing error: {:?}", e))
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
                    let map = b
                        .into_iter()
                        .map(|(k, v)| {
                            let to_parse = prolog_value_to_json_string(v.clone());
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
        dbg!(&g.to_string());
        let qr = self
            .machine
            .run_query(format!(r#"{}."#, g.to_string()))
            .map_err(LogicMachineError::PrologError)?;

        Self::parse_to_facts(qr, Arc::clone(&target_rt))
    }
}

#[cfg(test)]
mod tests {
    use super::LogicMachine;
    use crate::models::fact::{FactTerm, GoalTerm, RecordTypeBuilder};
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
    }

    #[test]
    fn term_list() {
        let mut lm = LogicMachine::new(String::from(
            r#"
                :- use_module(library(clpz)).
                :- use_module(library(lists)).

                :- dynamic(dimlink/9).

                dimlink("Test1", "MR00000001", "ID00000001", "JH00000001", "2023-02-08", "2023-02-10", "2024-02-18 08:16:11", "D", "0"). 
                dimlink("Test1", "MR00000001", "ID00000001", "JH00000001", "2023-02-09", "2023-02-10", "2024-02-18 08:17:11", "E", "1"). 
                dimlink("Test1", "MR00000002", "ID00000002", "JH00000001", "2023-02-08", "2023-02-11", "2024-02-18 08:20:11", "D", "0").
                dimlink("Test1", "MR00000002", "ID00000002", "JH00000001", "2023-02-08", "2023-02-11", "2024-02-18 08:20:12", "D", "1"). 
                dimlink("Test1", "MR00000002", "ID00000002", "JH00000001", "2023-02-08", "2023-02-11", "2024-02-18 08:20:13", "D", "2"). 
                dimlink("Test1", "MR00000002", "ID00000002", "JH00000002", "2023-02-09", "2023-02-11", "2024-02-18 08:20:14", "D", "3"). 
                dimlink("Test1", "MR00000002", "ID00000002", "JH00000003", "2023-02-08", "2023-02-11", "2024-02-18 08:20:15", "E", "4"). 
                dimlink("Test1", "MR00000003", "ID00000002", "JH00000002", "2023-02-08", "2023-02-09", "2024-02-18 08:20:14", "O", "0"). 
                dimlink("Test1", "MR00000004", "ID00000001", "JH00000002", "2023-02-08", "2023-02-09", "2024-02-18 09:17:11", "O", "0").
                dimlink("Test1", "MR00000005", "ID00000001", "JH00000003", "2023-02-08", "2023-02-09", "2024-02-18 09:17:11", "D", "0").
                dimlink("Test1", "MR00000005", "ID00000001", "JH00000003", "2023-02-08", "2023-02-09", "2024-02-18 09:17:11", "V", "1").

                table("dimlink", "MgrLinkRef", ["DimIdRef", "InvHeadRef", "BegPeriod", "EndPeriod"]).

                record(Context, SysVersion, SeqNum, RecType, Id, [DimIdRef, InvHeadRef, BegPeriod, EndPeriod]) :-
                    dimlink(Context, Id, DimIdRef, InvHeadRef, BegPeriod, EndPeriod, SysVersion, RecType, SeqNum).

                change_step(Ctx, Id, Vals1, Vals2) :-
                    record(Ctx, _, SeqNum1, _, Id, Vals1),
                    record(Ctx, _, SeqNum2, _, Id, Vals2),
                    number_chars(Num1, SeqNum1),
                    number_chars(Num2, SeqNum2),
                    Num2 #= Num1 + 1,
                    Vals1 \= Vals2.

                change_path(Ctx, Id, Vals, Vals, []) :-
                    record(Ctx, _, _, _, Id, Vals).

                change_path(Ctx, Id, Vals1, Vals2, [Step|Steps]) :-
                    change_step(Ctx, Id, Vals1, ValsMid),  % Enforce step exists
                    Step = [Vals1, ValsMid],    % Construct step term
                    change_path(Ctx, Id, ValsMid, Vals2, Steps).
        "#,
        ));

        // let leap_change = Arc::new(
        //     RecordTypeBuilder::new("leap_change", vec!["Ctx", "Id", "Vals1", "Vals2", "Steps"])
        //         .build()
        //         .unwrap(),
        // );

        // let g = leap_change
        //     .to_goal_from_named_values(&HashMap::from([
        //         (
        //             "Vals1".to_string(),
        //             GoalTerm::List(vec![
        //                 // GoalTerm::LocalVariable("DimIdRef".to_string()),
        //                 GoalTerm::String("JH00000001".to_string()).into(),
        //                 // GoalTerm::LocalVariable("BegPeriod".to_string()),
        //                 // GoalTerm::LocalVariable("EndPeriod".to_string()),
        //             ]),
        //         ),
        //         (
        //             "Vals2".to_string(),
        //             GoalTerm::List(vec![
        //                 // GoalTerm::LocalVariable("DimIdRef".to_string()),
        //                 GoalTerm::String("JH00000003".to_string()).into(),
        //                 // GoalTerm::LocalVariable("BegPeriod".to_string()),
        //                 // GoalTerm::LocalVariable("EndPeriod".to_string()),
        //             ]),
        //         ),
        //     ]))
        //     .unwrap();

        // let s = g.to_string();
        // dbg!(&s);

        // let res = lm.machine.run_query(format!(
        //     "','(','(','('leap_change'(LEAP_CHANGE0_Ctx, LEAP_CHANGE0_Id, RT_Vals1, RT_Vals2, RT_Steps), 'length'(RT_Steps, 2)), '='(RT_Vals1, [DIMLINK13_DimIdRef, \"JH00000001\", DIMLINK13_BegPeriod, DIMLINK13_EndPeriod])), '='(RT_Vals2, [DIMLINK14_DimIdRef, \"JH00000003\", DIMLINK14_BegPeriod, DIMLINK14_EndPeriod]))."
        // ));
        // let res = lm.machine.run_query(format!(
        //     "leap_change(Ctx, Id, Vals1, Vals2, X), length(X, 2), Vals1 = [_, \"JH00000001\",_, _], Vals2 = [_, \"JH00000003\",_, _]."
        // ));
        // let res = lm.machine.run_query(
        //     r#"Vals1 = [_, "JH00000001",_, _], Vals2 = [_, "JH00000003",_, _], length(X, 2), leap_change(Ctx, Id, Vals1, Vals2, X)."#.to_string()
        // );
        // let res = lm.machine.run_query(
        //     r#"Vals1 = ["ID00000002", "JH00000001", "2023-02-08", "2023-02-11"], X = [], leap_change(Ctx, "MR00000002", Vals1, Vals1, X)."#.to_string()
        // );
        // let res = lm.machine.run_query(
        //     r#"Vals1 = ["ID00000002", "JH00000001", "2023-02-08", "2023-02-11"], X = A, A = [], leap_change(Ctx, "MR00000002", Vals1, Vals1, X)."#.to_string()
        // );
        // let res = lm.machine.run_query(
        //     r#"leap_change(Ctx, "MR00000002", Vals1, Vals2, X), length(X, 1), Vals1 = [A, B, C, D]."#.to_string()
        // );

        let res = lm.machine.run_query(
            r#"
            ','(
                ','(
                    ','(
                        'leap_change'(LEAP_CHANGE0_Ctx, LEAP_CHANGE0_Id, RT_Vals1, RT_Vals2, RT_Steps),
                        'length'(RT_Steps, 2)
                    ),
                    '='(RT_Vals1, [DIMLINK13_DimIdRef, "JH00000001", DIMLINK13_BegPeriod, DIMLINK13_EndPeriod])
                ),
                '='(RT_Vals2, [DIMLINK14_DimIdRef, DIMLINK14_InvHeadRef, DIMLINK14_BegPeriod, DIMLINK14_EndPeriod])
            )."#.to_string()
        );

        let res = lm.machine.run_query(
            r#"
                leap_change(LEAP_CHANGE0_Ctx, LEAP_CHANGE0_Id, RT_Vals1, RT_Vals2, RT_Steps),
                length(RT_Steps, 2),
                RT_Vals1 = [DIMLINK13_DimIdRef, "JH00000001", DIMLINK13_BegPeriod, DIMLINK13_EndPeriod],
                RT_Vals2 = [DIMLINK14_DimIdRef, DIMLINK14_InvHeadRef, DIMLINK14_BegPeriod, DIMLINK14_EndPeriod].
            "#.to_string()
        );
        dbg!(&res);

        // let res = lm.fetch(g);kfd
    }

    #[test]
    fn parse_fact() {
        // let fs = "[\"3\", 3, 3.0, 3.1, 0, -1, \"-12\", -111111111111111111111111111.1]".parse::<FactTerm>();
        let fs = r#"'goal'("3"  ,  0.2)"#.parse::<FactTerm>();

        dbg!(&fs);
    }

    #[test]
    fn parse_fact2() {
        // let fs = r#"
        //     [
        //         [
        //             ["ID00000002","JH00000001","2023-02-08","2023-02-11"],
        //             ["ID00000002","JH00000002","2023-02-09","2023-02-11"]
        //         ],
        //         [
        //             ["ID00000002","JH00000002","2023-02-09","2023-02-11"],
        //             ["ID00000002","JH00000003","2023-02-08","2023-02-11"]
        //         ]
        //     ]
        // "#;

        // let fs = r#"
        //     [[["ID00000002","JH00000001","2023-02-08","2023-02-11"],["ID00000002","JH00000002","2023-02-09","2023-02-11"]],[["ID00000002","JH00000002","2023-02-09","2023-02-11"],["ID00000002","JH00000003","2023-02-08","2023-02-11"]]]
        // "#.parse::<FactTerm>();

        // let fs = r#"12343.3434"#
        //     .parse::<FactTerm>();

        // dbg!(&fs);
    }
}

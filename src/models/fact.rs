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

use std::fmt::Formatter;
use std::str::FromStr;
use std::{clone::Clone, collections::HashMap, sync::Arc};

use crate::models::record_type::{RecordType, RecordTypeBuilder};

/// A Fact is a compound term which has all its values grounded.
/// It is meant to be asserted to or by the logic machine
#[derive(Debug, Clone, PartialEq)]
pub struct Fact {
    pub type_: Arc<RecordType>,
    pub values: Vec<FactTerm>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FactTerm {
    String(String),
    Integer(i32),
    Float(f64),
    List(Vec<FactTerm>),
    SubTerm(Fact),
}

#[derive(thiserror::Error, Debug)]
#[error("field not found: {0}")]
pub struct FieldNotFound(String);

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

    fn to_data_values(&self) -> HashMap<String, FactTerm> {
        self.type_
            .data_fields
            .iter()
            .zip(self.values.iter())
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect::<HashMap<_, _>>()
    }

    pub fn get(&self, field: &str) -> Result<FactTerm, FieldNotFound> {
        self.to_data_values()
            .get(field)
            .ok_or(FieldNotFound(field.to_string()))
            .cloned()
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

#[cfg(test)]
mod tests {
    #[test]
    fn parse_fact2() {
        // let fs = r#"
        //     [
        //         [
        //             ["ID2","J1","2023-02-08","2023-02-11"],
        //             ["ID2","J2","2023-02-09","2023-02-11"]
        //         ],
        //         [
        //             ["ID2","J2","2023-02-09","2023-02-11"],
        //             ["ID2","J3","2023-02-08","2023-02-11"]
        //         ]
        //     ]
        // "#;

        // let fs = r#"
        //     [[["ID2","J1","2023-02-08","2023-02-11"],["ID2","J2","2023-02-09","2023-02-11"]],[["ID2","J2","2023-02-09","2023-02-11"],["ID2","J3","2023-02-08","2023-02-11"]]]
        // "#.parse::<FactTerm>();

        // let fs = r#"12343.3434"#
        //     .parse::<FactTerm>();

        // dbg!(&fs);
    }
}

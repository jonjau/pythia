use std::{collections::HashMap, fmt, sync::Arc};

use super::record_type::{RecordType, RecordTypeBuilder};

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
                .expect("Failed to create built-in goal ','"),
        );

        let res = conjunction
            .to_goal_from_named_values(&HashMap::from([
                ("Term1".to_string(), GoalTerm::SubTerm(self)),
                ("Term2".to_string(), GoalTerm::SubTerm(goal2)),
            ]))
            .expect("Failed to create built-in goal, ','");

        res
    }
}

impl fmt::Display for Goal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

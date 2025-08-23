use std::{collections::HashMap, fmt, sync::Arc};

use super::record_type::{RecordType, RecordTypeBuilder};

/// Represents a compound term which may not have all its values grounded.
/// It is meant to be run as a query to the logic machine.
///
/// A `Goal` is typed using a [`RecordType`] and contain a list of associated values.
/// Values that are not grounded are to be left as a Term::Variable.
/// Typically corresponds to a logic query like: `'Person'(X, 30)`.
#[derive(Clone, Debug, PartialEq)]
pub struct Goal {
    /// Identitier for the goal.
    pub id: String,

    /// The type (functor) of the goal (e.g., `"Person"` or `"has_address"`).
    pub type_: Arc<RecordType>,

    /// The ordered list of values (terms) for the goal.
    pub values: Vec<GoalTerm>,
}

/// Represents a term in a logic goal.
///
/// A term can be a variable, constant, list, or nested sub-goal.
#[derive(Clone, Debug, PartialEq)]
pub enum GoalTerm {
    /// A named variable (e.g., `"X"`), which can match any value.
    Variable(String),

    /// A string literal, displayed with double quotes.
    String(String),

    /// An integer constant.
    Integer(i32),

    /// A list of goal terms, e.g., `[X, 1, "a"]`.
    List(Vec<GoalTerm>),

    /// A nested goal, supporting compound subqueries.
    SubTerm(Goal),
}

impl Goal {
    /// Returns all values in the goal, trimmed to the number of defined fields.
    pub fn to_all_values(&self) -> Vec<GoalTerm> {
        self.values
            .iter()
            .take(self.type_.clone().all_fields().len())
            .cloned()
            .collect::<Vec<_>>()
    }

    /// Returns a list of only the data values (excluding ID and metadata fields) as a `GoalTerm::List`.
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

    /// Combines two goals using logical conjunction (`,` operator).
    ///
    /// The resulting goal has the type `','` with two sub-goals as arguments.
    ///
    /// # Panics
    ///
    /// Panics if the built-in conjunction goal cannot be constructed.
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

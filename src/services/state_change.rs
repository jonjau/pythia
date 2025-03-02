use std::{collections::HashMap, fmt, sync::Arc};

use crate::models::fact::{Fact, FactTerm, FieldNotFound};
use crate::models::goal::GoalTerm;
use crate::models::logic_machine::LogicMachineError;
use crate::models::record_type::{RecordType, RecordTypeError};
use crate::services::fact::FactService;

// TODO: to display a path:
// detailed view: show a table with the columns being the field names of the state_rt
//   one row per change_step, the rows in the table will show the state after each change
//   the change field being highlighted in a different color
// simple view: show an unordered list, each item being in this format:
//    field: before -> after
// the table of paths should be sortable and filterable.
// Maybe we should also allow showing/hiding some fields

pub struct FieldDiff {
    pub field: String,
    pub before: FactTerm,
    pub after: FactTerm,
}

impl fmt::Display for FieldDiff {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {} → {}", self.field, self.before, self.after)
    }
}

pub struct ChangeStep {
    before: Fact,
    after: Fact,
    pub diffs: Vec<FieldDiff>,
}

impl fmt::Display for ChangeStep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.diffs
                .iter()
                .map(|d| d.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

pub struct ChangePath {
    pub steps: Vec<ChangeStep>,
}

impl fmt::Display for ChangePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.steps
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(thiserror::Error, Debug)]
pub enum StateChangeError {
    #[error("Term not found")]
    FactError(#[from] LogicMachineError),
    #[error("RT error")]
    RecordTypeError(#[from] RecordTypeError),
    #[error("Field not found")]
    FieldNotFound(#[from] FieldNotFound),
    #[error("Could not match to term to a list")]
    CouldNotMatchList,
}

#[derive(Clone)]
pub struct StateChangeService {
    facts: FactService,
}

impl StateChangeService {
    pub fn new(facts: FactService) -> Self {
        StateChangeService { facts }
    }

    fn difference(f1: &Fact, f2: &Fact) -> Result<Vec<FieldDiff>, StateChangeError> {
        f1.data_fields()
            .iter()
            .map(|field_name| {
                let (term1, term2) = (f1.get(field_name)?, f2.get(field_name)?);
                Ok(if term1 != term2 {
                    Some(FieldDiff {
                        field: field_name.to_string(),
                        before: term1,
                        after: term2,
                    })
                } else {
                    None
                })
            })
            .collect::<Result<Vec<_>, _>>()
            .map(|diffs| diffs.into_iter().flatten().collect())
    }

    fn to_change_path(
        rt: &Arc<RecordType>,
        term: &FactTerm,
    ) -> Result<ChangePath, StateChangeError> {
        let change_step_terms = match term {
            FactTerm::List(terms) => terms,
            _ => return Err(StateChangeError::CouldNotMatchList),
        };

        let change_steps = change_step_terms
            .iter()
            .map(|change_step_term| Self::to_change_step(&rt, change_step_term))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(ChangePath {
            steps: change_steps,
        })
    }

    fn to_change_step(
        rt: &Arc<RecordType>,
        term: &FactTerm,
    ) -> Result<ChangeStep, StateChangeError> {
        let step = match term {
            FactTerm::List(step) if step.len() == 2 => step,
            _ => return Err(StateChangeError::CouldNotMatchList),
        };

        let (before_vals, after_vals) = match (&step[0], &step[1]) {
            (FactTerm::List(before), FactTerm::List(after)) => (before, after),
            _ => return Err(StateChangeError::CouldNotMatchList),
        };

        let before = Fact::new(rt.clone(), before_vals.clone());
        let after = Fact::new(rt.clone(), after_vals.clone());
        let diffs = Self::difference(&before, &after)?;

        Ok(ChangeStep {
            before,
            after,
            diffs,
        })
    }

    pub async fn get_paths(
        &self,
        state_rt_name: &str,
        start_state: HashMap<String, GoalTerm>,
        end_state: HashMap<String, GoalTerm>,
        num_steps: i32,
    ) -> Result<Vec<ChangePath>, StateChangeError> {
        let state_rt = self.facts.get_record_type(state_rt_name).await?;

        let change_path_rt = self.facts.get_record_type("change_path").await?;
        let change_path_goal = change_path_rt.to_goal_from_named_values(
            &[
                ("Vals1".to_string(), GoalTerm::Variable("Vals1".to_string())),
                ("Vals2".to_string(), GoalTerm::Variable("Vals2".to_string())),
                ("Steps".to_string(), GoalTerm::Variable("Steps".to_string())),
            ]
            .into(),
        )?;

        let binding_goal_rt = self.facts.get_record_type("=".to_string()).await?;
        let binding_goal1 = Arc::clone(&binding_goal_rt).to_goal(vec![
            GoalTerm::Variable("Vals1".to_string()),
            Arc::clone(&state_rt)
                .to_goal_from_named_values(&start_state)?
                .to_data_value_list(),
        ])?;
        let binding_goal2 = Arc::clone(&binding_goal_rt).to_goal(vec![
            GoalTerm::Variable("Vals2".to_string()),
            Arc::clone(&state_rt)
                .to_goal_from_named_values(&end_state)?
                .to_data_value_list(),
        ])?;

        let length_rt = self.facts.get_record_type("length").await?;
        let length_goal = length_rt.to_goal(vec![
            GoalTerm::Variable("Steps".to_string()),
            GoalTerm::Integer(num_steps),
        ])?;

        let change_paths = self
            .facts
            .get_facts(
                change_path_goal
                    .clone()
                    .and(length_goal)
                    .and(binding_goal1)
                    .and(binding_goal2),
                change_path_goal.type_,
            )
            .await?;

        Ok(change_paths
            .iter()
            .map(|path| {
                let steps_term = path.get("Steps")?;
                Self::to_change_path(&state_rt, &steps_term)
            })
            .collect::<Result<Vec<_>, _>>()?)
    }
}

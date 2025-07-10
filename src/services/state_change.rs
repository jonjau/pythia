use std::{collections::HashMap, fmt, sync::Arc};

use crate::models::fact::{Fact, FactTerm, FieldNotFound};
use crate::models::goal::GoalTerm;
use crate::models::logic_machine::LogicMachineError;
use crate::models::record_type::{RecordType, RecordTypeError};
use crate::services::logic_machine::LogicMachineService;

/// Represents the difference in a single field between two facts.
///
/// Holds the field name and the value before and after the change.
#[derive(Clone)]
pub struct FieldDiff {
    pub field_name: String,
    pub before: FactTerm,
    pub after: FactTerm,
}

impl fmt::Display for FieldDiff {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} → {}", self.before, self.after)
    }
}

/// Represents a single state transition between two facts.
///
/// Each `ChangeStep` contains the facts before and after the step,
/// and a list of optional `FieldDiff`s.
pub struct ChangeStep {
    /// The fact representing the state before the change.
    pub before: Fact,

    /// The fact representing the state after the change.
    pub after: Fact,

    /// Differences in field values between `before` and `after`.
    ///
    /// If a field did not change, its entry is `None`.
    pub diffs: Vec<Option<FieldDiff>>,
}

impl fmt::Display for ChangeStep {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.after
                .data_fields()
                .into_iter()
                .zip(self.diffs.iter())
                .filter_map(|(f, d)| d.clone().and_then(|diff| Some((f, diff))))
                .map(|(f, d)| format!("{}: {}", f, d))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

/// Represents a full path of state changes for a given context and ID.
///
/// Tracks the initial state and each transition step (diff).
pub struct ChangePath {
    /// The context this path belongs to (e.g. scenario or namespace).
    pub context: String,

    /// The identifier of the entity being tracked.
    pub id: String,

    /// The starting fact (initial state) of the path.
    pub initial_state: Fact,

    /// A series of changes leading from the initial state to the final state.
    pub steps: Vec<ChangeStep>,
}

/// Holds the stringified values of a record at the start and end of a state transition.
///
/// Used for UI or logging purposes when comparing two states.
pub struct StateValues {
    /// Field values at the beginning of the transition.
    pub start_state_values: Vec<(String, String)>,

    /// Field values at the end of the transition.
    pub end_state_values: Vec<(String, String)>,
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

/// Errors that can occur when processing state changes.
#[derive(thiserror::Error, Debug)]
pub enum StateChangeError {
    #[error("Parameter not found: {0}")]
    ParameterNotFound(String),
    #[error("Invalid int parameter found: {0}")]
    InvalidIntParameter(#[from] std::num::ParseIntError),
    #[error("LogicMachine error: {0}")]
    FactError(#[from] LogicMachineError),
    #[error("RecordType error: {0}")]
    RecordTypeError(#[from] RecordTypeError),
    #[error("Field not found in state change: {0}")]
    FieldNotFound(#[from] FieldNotFound),
    #[error("Could not match to term to a list")]
    CouldNotMatchList,
}

/// A high-level wrapper around the [`LogicMachineService`] for working with state change paths.
///
/// This service provides utilities for calculating state change paths
/// through the underlying logic engine.
///
/// Cloneable for convenience in concurrent or async contexts.
#[derive(Clone)]
pub struct StateChangeService {
    lm: LogicMachineService,
}

impl StateChangeService {
    /// Creates a new `StateChangeService` from the given logic machine.
    pub fn new(lm: LogicMachineService) -> Self {
        StateChangeService { lm }
    }

    /// Computes field-level differences between two Facts which are assumed to describe the same `RecordType`.
    fn difference(f1: &Fact, f2: &Fact) -> Result<Vec<Option<FieldDiff>>, StateChangeError> {
        f1.data_fields()
            .into_iter()
            .map(|field_name| {
                let (term1, term2) = (f1.get(&field_name)?, f2.get(&field_name)?);
                Ok(if term1 != term2 {
                    Some(FieldDiff {
                        field_name,
                        before: term1,
                        after: term2,
                    })
                } else {
                    None
                })
            })
            .collect::<Result<Vec<_>, _>>()
    }

    /// Constructs a `ChangePath` from a Fact that describes a `change_path` Prolog term for the given `RecordType`.
    fn to_change_path(rt: &Arc<RecordType>, path: &Fact) -> Result<ChangePath, StateChangeError> {
        let (ctx, id, steps_term) = (
            path.get("Ctx")?.to_string(),
            path.get("Id")?.to_string(),
            path.get("Steps")?,
        );

        let change_step_terms = match steps_term {
            FactTerm::List(terms) => terms,
            _ => return Err(StateChangeError::CouldNotMatchList),
        };

        let change_steps = change_step_terms
            .iter()
            .map(|change_step_term| Self::to_change_step(&rt, change_step_term))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(ChangePath {
            context: ctx,
            id,
            initial_state: change_steps[0].before.clone(),
            steps: change_steps,
        })
    }

    /// Constructs a `ChangeStep` from a Fact that describes a `change_step` Prolog term for the given `RecordType`.
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

    /// Populates field values from two name-value maps for the given record type.
    ///
    /// # Arguments
    ///
    /// * `state_rt_name` - Name of the record type that the states are for.
    /// * `named_values0` - Field-values mappings for the initial state.
    /// * `named_values1` - Field-value mappings for the end state.
    /// 
    /// # Returns
    /// 
    /// `StateValues` containing a vector of field-value pairs (with default values if absent in the input maps),
    /// in the order as described in the `RecordType`. 
    ///
    /// # Errors
    ///
    /// If the record type cannot be loaded.
    pub async fn populate_all_state_values(
        &self,
        state_rt_name: &str,
        named_values0: HashMap<String, String>,
        named_values1: HashMap<String, String>,
    ) -> Result<StateValues, StateChangeError> {
        let state_rt = self.lm.get_record_type(state_rt_name).await?;
        let populate_all_state_values = |named_values: &HashMap<String, String>| {
            state_rt
                .data_fields
                .iter()
                .map(|field_name| {
                    (
                        field_name.clone(),
                        named_values.get(field_name).cloned().unwrap_or_default(),
                    )
                })
                .collect::<Vec<_>>()
        };

        Ok(StateValues {
            start_state_values: populate_all_state_values(&named_values0),
            end_state_values: populate_all_state_values(&named_values1),
        })
    }

    /// Finds possible transition paths between a start and end state, with `num_steps` steps, by querying the LogicMachine.
    ///
    /// # Arguments
    ///
    /// * `state_rt_name` - Name of the record type that the states are for.
    /// * `start_state` - Field-values mappings for the initial state.
    /// * `end_state` - Field-value mappings for the end state.
    /// * `num_steps` - The number of steps in the path.
    ///
    /// # Returns
    ///
    /// A vector of `ChangePath`s if successful.
    ///
    /// # Errors
    ///
    /// - If any parameter is missing or invalid.
    /// - If the logic engine fails to resolve the facts or any intermediate prolog goals.
    pub async fn find_paths(
        &self,
        state_rt_name: &str,
        start_state: HashMap<String, String>,
        end_state: HashMap<String, String>,
        num_steps: Option<&String>,
    ) -> Result<Vec<ChangePath>, StateChangeError> {
        let state_rt = self.lm.get_record_type(state_rt_name).await?;
        let start_state = start_state
            .into_iter()
            .map(|(k, v)| (k.clone(), GoalTerm::String(v)))
            .collect::<HashMap<_, _>>();
        let end_state = end_state
            .into_iter()
            .map(|(k, v)| (k.clone(), GoalTerm::String(v)))
            .collect::<HashMap<_, _>>();
        let num_steps = num_steps
            .ok_or(StateChangeError::ParameterNotFound("num-steps".to_string()))?
            .parse::<i32>()?;

        self.find_paths_(state_rt, start_state, end_state, num_steps)
            .await
    }

    /// Finds possible transition paths between a start and end state, with `num_steps` steps, by querying the LogicMachine.
    /// Takes in String-GoalTerm mappings for the start and end states.
    async fn find_paths_(
        &self,
        state_rt: Arc<RecordType>,
        start_state: HashMap<String, GoalTerm>,
        end_state: HashMap<String, GoalTerm>,
        num_steps: i32,
    ) -> Result<Vec<ChangePath>, StateChangeError> {
        let change_path_rt = self.lm.get_record_type("change_path").await?;
        let change_path_goal = change_path_rt.to_goal_from_named_values(
            &[
                ("RType".to_string(), GoalTerm::Variable("RType".to_string())),
                ("Vals1".to_string(), GoalTerm::Variable("Vals1".to_string())),
                ("Vals2".to_string(), GoalTerm::Variable("Vals2".to_string())),
                ("Steps".to_string(), GoalTerm::Variable("Steps".to_string())),
            ]
            .into(),
        )?;

        let binding_goal_rt = self.lm.get_record_type("=".to_string()).await?;
        let binding_goal0 = Arc::clone(&binding_goal_rt).to_goal(vec![
            GoalTerm::Variable("RType".to_string()),
            GoalTerm::String(state_rt.name.clone()),
        ])?;
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

        let length_rt = self.lm.get_record_type("length").await?;
        let length_goal = length_rt.to_goal(vec![
            GoalTerm::Variable("Steps".to_string()),
            GoalTerm::Integer(num_steps),
        ])?;

        let change_paths = self
            .lm
            .get_facts(
                change_path_goal
                    .clone()
                    .and(length_goal)
                    .and(binding_goal0)
                    .and(binding_goal1)
                    .and(binding_goal2),
                change_path_goal.type_,
            )
            .await?;

        Ok(change_paths
            .iter()
            .map(|path| Self::to_change_path(&state_rt, &path))
            .collect::<Result<Vec<_>, _>>()?)
    }
}

use std::{collections::HashMap, sync::Arc};

use crate::utils::tracking::IdContext;

use crate::models::fact::{Fact, FactTerm};
use crate::models::goal::{Goal, GoalTerm};

impl Goal {
    fn new(id_number: usize, type_: Arc<RecordType>, values: Vec<GoalTerm>) -> Self {
        Goal {
            id: Arc::clone(&type_).get_term_id(id_number),
            type_: Arc::clone(&type_),
            values,
        }
    }
}

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

    pub fn get_term_id(&self, id_number: usize) -> String {
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

        Ok(Fact::new(self, complete_values))
    }
}

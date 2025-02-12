use std::{collections::HashMap, sync::Arc};

use crate::models::fact::{Fact, GoalTerm, RecordType};

use super::fact::FactService;

#[derive(Clone)]
pub struct StateChangeService {
    facts: FactService,
}

impl StateChangeService {
    pub fn new(facts: FactService) -> Self {
        StateChangeService { facts }
    }

    pub async fn get_step_changes(
        &self,
        subgoal_rt: Arc<RecordType>,
        start_state: HashMap<String, GoalTerm>,
        end_state: HashMap<String, GoalTerm>,
    ) -> Vec<Fact> {
        let step_change_rt = self.facts.get_record_type("step_change").await.unwrap();

        let step_change_goal = step_change_rt
            .to_goal_from_named_values(
                &[
                    ("Vals1".to_string(), GoalTerm::Variable("Vals1".to_string())),
                    ("Vals2".to_string(), GoalTerm::Variable("Vals2".to_string())),
                ]
                .into(),
            )
            .unwrap();

        let binding_goal_rt = self.facts.get_record_type("=".to_string()).await.unwrap();

        let binding_goal1 = Arc::clone(&binding_goal_rt)
            .to_goal(vec![
                GoalTerm::Variable("Vals1".to_string()),
                Arc::clone(&subgoal_rt)
                    .to_goal_from_named_values(&start_state)
                    .unwrap()
                    .to_data_value_list(),
            ])
            .unwrap();
        let binding_goal2 = Arc::clone(&binding_goal_rt)
            .to_goal(vec![
                GoalTerm::Variable("Vals2".to_string()),
                Arc::clone(&subgoal_rt)
                    .to_goal_from_named_values(&end_state)
                    .unwrap()
                    .to_data_value_list(),
            ])
            .unwrap();

        let result = self
            .facts
            .get_facts(
                step_change_goal.clone().and(binding_goal1).and(binding_goal2),
                step_change_goal.type_,
            )
            .await
            .unwrap();

        result
    }

    pub async fn get_leap_changes(
        &self,
        subgoal_rt: Arc<RecordType>,
        start_state: HashMap<String, GoalTerm>,
        end_state: HashMap<String, GoalTerm>,
        _num_steps: i32,
    ) -> Vec<Fact> {
        let leap_change_rt = self.facts.get_record_type("leap_change").await.unwrap();


        let leap_change_goal = leap_change_rt
            .to_goal_from_named_values(
                &[
                    ("Vals1".to_string(), GoalTerm::Variable("Vals1".to_string())),
                    ("Vals2".to_string(), GoalTerm::Variable("Vals2".to_string())),
                    ("Steps".to_string(), GoalTerm::Variable("Steps".to_string())),
                ]
                .into(),
            )
            .unwrap();

        let binding_goal_rt = self.facts.get_record_type("=".to_string()).await.unwrap();

        let binding_goal1 = Arc::clone(&binding_goal_rt)
            .to_goal(vec![
                GoalTerm::Variable("Vals1".to_string()),
                Arc::clone(&subgoal_rt)
                    .to_goal_from_named_values(&start_state)
                    .unwrap()
                    .to_data_value_list(),
            ])
            .unwrap();
        let binding_goal2 = Arc::clone(&binding_goal_rt)
            .to_goal(vec![
                GoalTerm::Variable("Vals2".to_string()),
                Arc::clone(&subgoal_rt)
                    .to_goal_from_named_values(&end_state)
                    .unwrap()
                    .to_data_value_list(),
            ])
            .unwrap();

        let length_rt = self.facts.get_record_type("length").await.unwrap();
        let length_goal = length_rt.to_goal(vec![
            GoalTerm::Variable("Steps".to_string()),
            GoalTerm::Integer(2)
        ]).unwrap();

        let result = self
            .facts
            .get_facts(
                leap_change_goal.clone().and(length_goal).and(binding_goal1).and(binding_goal2),
                leap_change_goal.type_,
            )
            .await
            .unwrap();

        result
    }
}

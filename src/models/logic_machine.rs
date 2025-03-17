use scryer_prolog::machine::{
    parsed_results::{prolog_value_to_json_string, QueryMatch, QueryResolution},
    Machine,
};
use std::{collections::HashMap, sync::Arc};

use crate::models::fact::{Fact, FactTerm};
use crate::models::goal::Goal;
use crate::models::record_type::RecordType;

#[derive(thiserror::Error, Debug)]
pub enum LogicMachineError {
    #[error("unexpected query resolution")]
    UnexpectedQueryResolution,
    #[error("prolog error: {0}")]
    PrologError(String),
    #[error("RecordType not found: {}", .0)]
    RecordTypeNotFound(String),
}

pub type LogicMachineResult<T> = Result<T, LogicMachineError>;

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

    fn parse_to_facts<'a>(
        qr: QueryResolution,
        rt: Arc<RecordType>,
    ) -> LogicMachineResult<Vec<Fact>> {
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
    pub fn get_record_type(&self, name: &str) -> LogicMachineResult<Arc<RecordType>> {
        self.record_types
            .get(name)
            .map(|record| Arc::new(record.clone()))
            .ok_or(LogicMachineError::RecordTypeNotFound(name.to_string()))
    }

    pub fn get_all_record_types(&self) -> LogicMachineResult<Vec<Arc<RecordType>>> {
        Ok(self
            .record_types
            .values()
            .map(|record| Arc::new(record.clone()))
            .collect())
    }

    pub fn define_types(&mut self, types: Vec<RecordType>) {
        for t in types {
            self.record_types.insert(t.name.clone(), t);
        }
    }

    pub fn add_fact(&mut self, f: Fact) -> LogicMachineResult<Vec<Fact>> {
        self.machine
            .run_query(format!(r#"assertz({})."#, f.to_string()))
            .map_err(LogicMachineError::PrologError)?;

        self.fetch_all(f.type_)
    }

    pub fn fetch_all(&mut self, rt: Arc<RecordType>) -> LogicMachineResult<Vec<Fact>> {
        self.fetch(Arc::clone(&rt).to_most_general_goal(), Arc::clone(&rt))
    }

    pub fn fetch(&mut self, g: Goal, target_rt: Arc<RecordType>) -> LogicMachineResult<Vec<Fact>> {
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
    use crate::models::logic_machine::LogicMachine;

    #[test]
    fn term_list() {
        let mut lm = LogicMachine::new(String::from(
            r#"
                :- use_module(library(clpz)).
                :- use_module(library(lists)).

                :- dynamic(dimlink/9).

                dimlink("Test1", "M1", "ID1", "J1", "2023-02-08", "2023-02-10", "2024-02-18 08:16:11", "D", "0"). 
                dimlink("Test1", "M1", "ID1", "J1", "2023-02-09", "2023-02-10", "2024-02-18 08:17:11", "E", "1"). 
                dimlink("Test1", "M2", "ID2", "J1", "2023-02-08", "2023-02-11", "2024-02-18 08:20:11", "D", "0").
                dimlink("Test1", "M2", "ID2", "J1", "2023-02-08", "2023-02-11", "2024-02-18 08:20:12", "D", "1"). 
                dimlink("Test1", "M2", "ID2", "J1", "2023-02-08", "2023-02-11", "2024-02-18 08:20:13", "D", "2"). 
                dimlink("Test1", "M2", "ID2", "J2", "2023-02-09", "2023-02-11", "2024-02-18 08:20:14", "D", "3"). 
                dimlink("Test1", "M2", "ID2", "J3", "2023-02-08", "2023-02-11", "2024-02-18 08:20:15", "E", "4"). 
                dimlink("Test1", "M3", "ID2", "J2", "2023-02-08", "2023-02-09", "2024-02-18 08:20:14", "O", "0"). 
                dimlink("Test1", "M4", "ID1", "J2", "2023-02-08", "2023-02-09", "2024-02-18 09:17:11", "O", "0").
                dimlink("Test1", "M5", "ID1", "J3", "2023-02-08", "2023-02-09", "2024-02-18 09:17:11", "D", "0").
                dimlink("Test1", "M5", "ID1", "J3", "2023-02-08", "2023-02-09", "2024-02-18 09:17:11", "V", "1").

                table("dimlink", "MRef", ["DRef", "IRef", "BegPeriod", "EndPeriod"]).

                record(Context, EditTime, SeqNum, RecStatus, Id, [DRef, IRef, BegPeriod, EndPeriod]) :-
                    dimlink(Context, Id, DRef, IRef, BegPeriod, EndPeriod, EditTime, RecStatus, SeqNum).

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
        //                 // GoalTerm::LocalVariable("DRef".to_string()),
        //                 GoalTerm::String("J1".to_string()).into(),
        //                 // GoalTerm::LocalVariable("BegPeriod".to_string()),
        //                 // GoalTerm::LocalVariable("EndPeriod".to_string()),
        //             ]),
        //         ),
        //         (
        //             "Vals2".to_string(),
        //             GoalTerm::List(vec![
        //                 // GoalTerm::LocalVariable("DRef".to_string()),
        //                 GoalTerm::String("J3".to_string()).into(),
        //                 // GoalTerm::LocalVariable("BegPeriod".to_string()),
        //                 // GoalTerm::LocalVariable("EndPeriod".to_string()),
        //             ]),
        //         ),
        //     ]))
        //     .unwrap();

        // let s = g.to_string();
        // dbg!(&s);

        // let res = lm.machine.run_query(format!(
        //     "','(','(','('leap_change'(LEAP_CHANGE0_Ctx, LEAP_CHANGE0_Id, RT_Vals1, RT_Vals2, RT_Steps), 'length'(RT_Steps, 2)), '='(RT_Vals1, [DIMLINK13_DRef, \"J1\", DIMLINK13_BegPeriod, DIMLINK13_EndPeriod])), '='(RT_Vals2, [DIMLINK14_DRef, \"J3\", DIMLINK14_BegPeriod, DIMLINK14_EndPeriod]))."
        // ));
        // let res = lm.machine.run_query(format!(
        //     "leap_change(Ctx, Id, Vals1, Vals2, X), length(X, 2), Vals1 = [_, \"J1\",_, _], Vals2 = [_, \"J3\",_, _]."
        // ));
        // let res = lm.machine.run_query(
        //     r#"Vals1 = [_, "J1",_, _], Vals2 = [_, "J3",_, _], length(X, 2), leap_change(Ctx, Id, Vals1, Vals2, X)."#.to_string()
        // );
        // let res = lm.machine.run_query(
        //     r#"Vals1 = ["ID2", "J1", "2023-02-08", "2023-02-11"], X = [], leap_change(Ctx, "M2", Vals1, Vals1, X)."#.to_string()
        // );
        // let res = lm.machine.run_query(
        //     r#"Vals1 = ["ID2", "J1", "2023-02-08", "2023-02-11"], X = A, A = [], leap_change(Ctx, "M2", Vals1, Vals1, X)."#.to_string()
        // );
        // let res = lm.machine.run_query(
        //     r#"leap_change(Ctx, "M2", Vals1, Vals2, X), length(X, 1), Vals1 = [A, B, C, D]."#.to_string()
        // );

        let res = lm.machine.run_query(
            r#"
            ','(
                ','(
                    ','(
                        'leap_change'(LEAP_CHANGE0_Ctx, LEAP_CHANGE0_Id, RT_Vals1, RT_Vals2, RT_Steps),
                        'length'(RT_Steps, 2)
                    ),
                    '='(RT_Vals1, [DIMLINK13_DRef, "J1", DIMLINK13_BegPeriod, DIMLINK13_EndPeriod])
                ),
                '='(RT_Vals2, [DIMLINK14_DRef, DIMLINK14_IRef, DIMLINK14_BegPeriod, DIMLINK14_EndPeriod])
            )."#.to_string()
        );

        let res = lm.machine.run_query(
            r#"
                leap_change(LEAP_CHANGE0_Ctx, LEAP_CHANGE0_Id, RT_Vals1, RT_Vals2, RT_Steps),
                length(RT_Steps, 2),
                RT_Vals1 = [DIMLINK13_DRef, "J1", DIMLINK13_BegPeriod, DIMLINK13_EndPeriod],
                RT_Vals2 = [DIMLINK14_DRef, DIMLINK14_IRef, DIMLINK14_BegPeriod, DIMLINK14_EndPeriod].
            "#.to_string()
        );
        dbg!(&res);

        // let res = lm.fetch(g);kfd
    }
}

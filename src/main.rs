use std::{collections::HashMap, sync::Arc};

use askama::Template;
use axum::{
    extract::{Path, Query, State},
    response::Redirect,
    routing::{delete, get, post},
    Json, Router,
};

use serde::{Deserialize, Serialize};

mod models;
mod services;
mod utils;

use models::fact::{GoalTerm, RecordTypeBuilder};
use services::{fact::FactService, state_change::StateChangeService};

#[derive(Clone)]
struct AppState {
    facts: FactService,
    state_changes: StateChangeService,
}

#[tokio::main]
async fn main() {
    let db = include_str!("../data/db.pl");

    let facts = FactService::new(db);

    // TODO: graceful shutdown of actor
    let state = AppState {
        facts: facts.clone(),
        state_changes: StateChangeService::new(facts.clone()),
    };

    let r = Router::new()
        .route("/", get(|| async { Redirect::permanent("/facts") }))
        .route("/facts", get(get_facts))
        .route("/facts", post(create_fact))
        .route("/all-state-changes", get(get_all_state_changes))
        .route("/state-changes", get(get_state_changes))
        .route(
            "/start-state/:fact_type/:field_name/specified",
            post(set_to_specified),
        )
        .route(
            "/start-state/:fact_type/:field_name/specified",
            delete(set_to_unspecified),
        )
        .route(
            "/end-state/:fact_type/:field_name/specified",
            post(set_to_specified_end),
        )
        .route(
            "/end-state/:fact_type/:field_name/specified",
            delete(set_to_unspecified_end),
        )
        .with_state(state);

    // run our app with hyper, listening globally on port 3000
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    axum::serve(listener, r).await.unwrap();
}

#[derive(Template)]
#[template(path = "index.html", ext = "html")]
struct FactsPage {
    fact_type: String,
    facts: Vec<String>,
    start_state_fields: Vec<String>,
}

#[derive(Deserialize)]
struct Params {
    fact_type: Option<String>,
}

async fn get_facts(query: Query<Params>, State(state): State<AppState>) -> FactsPage {
    match &query.fact_type {
        None => FactsPage {
            fact_type: "".to_string(),
            facts: vec![],
            start_state_fields: vec![],
        },
        Some(ft) => {
            let result = state.facts.get_all_facts(ft.to_string()).await.unwrap();
            let fs = result.iter().map(|f| f.to_string()).collect::<Vec<_>>();

            FactsPage {
                fact_type: result[0].type_name(),
                facts: fs,
                start_state_fields: result[0].data_fields(),
            }
        }
    }
}

#[derive(Template)]
#[template(path = "fact-table.html", ext = "html")]
struct FactsTable {
    facts: Vec<String>,
}

async fn get_all_state_changes(State(state): State<AppState>) -> FactsTable {
    let result = state
        .facts
        .get_all_facts("step_change".to_string())
        .await
        .unwrap();
    let fs = result.iter().map(|f| f.to_string()).collect::<Vec<_>>();

    FactsTable { facts: fs }
}

async fn get_state_changes(
    State(app_state): State<AppState>,
    Query(states): Query<HashMap<String, String>>,
) -> FactsTable {
    let fact_type = states.get("_fact_type").unwrap();
    let subgoal_rt = app_state
        .facts
        .get_record_type(fact_type.to_string())
        .await
        .unwrap();

    let named_values0 = states
        .iter()
        .filter_map(|(field, value)| {
            if field.starts_with("0.") {
                Some((field[2..].to_string(), GoalTerm::String(value.to_string())))
            } else {
                None
            }
        })
        .collect::<HashMap<_, _>>();

    let named_values1 = states
        .iter()
        .filter_map(|(field, value)| {
            if field.starts_with("1.") {
                Some((field[2..].to_string(), GoalTerm::String(value.to_string())))
            } else {
                None
            }
        })
        .collect::<HashMap<_, _>>();
    dbg!(&named_values1);

    let rt = app_state
        .facts
        .get_record_type("step_change".to_string())
        .await
        .unwrap();

    let m = HashMap::from([
        ("Vals1".to_string(), GoalTerm::Variable("Vals1".to_string())),
        ("Vals2".to_string(), GoalTerm::Variable("Vals2".to_string())),
    ]);
    let step_change_goal = rt.to_goal(&m).unwrap();
    dbg!(&step_change_goal);

    let binding_goal_rt =
        Arc::new(RecordTypeBuilder::new("=", vec!["A", "B"]).display_name("equal").build().unwrap());
    let binding_goal1 = Arc::clone(&binding_goal_rt)
        .to_goal(&HashMap::from([
            ("A".to_string(), GoalTerm::Variable("Vals1".to_string())),
            (
                "B".to_string(),
                Arc::clone(&subgoal_rt)
                    .to_goal(&named_values0)
                    .unwrap()
                    .to_data_value_list(),
            ),
        ]))
        .unwrap();
    let binding_goal2 = Arc::clone(&binding_goal_rt)
    .to_goal(&HashMap::from([
        ("A".to_string(), GoalTerm::Variable("Vals2".to_string())),
        (
            "B".to_string(),
            Arc::clone(&subgoal_rt)
                .to_goal(&named_values1)
                .unwrap()
                .to_data_value_list(),
        ),
    ])).unwrap();

    let result = app_state
        .facts
        .get_facts(step_change_goal.and(binding_goal1).and(binding_goal2), step_change_goal.type_)
        .await
        .unwrap();

    let fs = result.iter().map(|f| f.to_string()).collect::<Vec<_>>();

    FactsTable { facts: fs }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
struct CreateFact {
    typename: String,
    values: Vec<String>,
}

// #[debug_handler]
async fn create_fact(State(_state): State<AppState>, Json(cf): Json<CreateFact>) -> String {
    // let rt = Arc::new(RecordType::new("edge", &["X", "Y"]).unwrap());
    // let fact = rt
    //     .to_fact(&HashMap::from([("X", "3"), ("Y", "5")]))
    //     .unwrap();

    // let fields_temp = (0..cf.values.len())
    //     .map(|i| format!("X{}", i))
    //     .collect::<Vec<_>>();
    // let fields = fields_temp.iter().map(String::as_ref).collect::<Vec<_>>();
    // let _rt0 = Arc::new(RecordTypeBuilder::new(&cf.typename, fields).build().unwrap());

    // let _mapped_values = fields
    //     .iter()
    //     .zip(cf.values.iter())
    //     .map(|(&field, value)| (field, value.as_str()))
    //     .collect::<HashMap<_, _>>();

    "".to_string()
    // let fact = rt0.to_fact(&mapped_values).unwrap();

    // // let res = state.facts.add_fact(fact).await.unwrap();
    // // res.to_string()

    // let res = state.facts.add_fact(fact).await.unwrap();
    // res.len().to_string();

    // let result = state.facts.get_all_facts(cf.typename).await.unwrap();
    // result
    //     .iter()
    //     .map(|f| f.to_string())
    //     .collect::<Vec<_>>()
    //     .join(",\n")
    // String::new()
}

#[derive(Template)]
#[template(path = "set-field-to-specified.html", ext = "html")]
struct SetFieldToSpecifiedTemplate {
    fact_type: String,
    field: String,
}

async fn set_to_specified(
    Path((fact_type, field_name)): Path<(String, String)>,
) -> SetFieldToSpecifiedTemplate {
    SetFieldToSpecifiedTemplate {
        fact_type,
        field: field_name,
    }
}

#[derive(Template)]
#[template(path = "set-field-to-unspecified.html", ext = "html")]
struct SetFieldToUnspecifiedTemplate {
    fact_type: String,
    field: String,
}

async fn set_to_unspecified(
    Path((fact_type, field_name)): Path<(String, String)>,
) -> SetFieldToUnspecifiedTemplate {
    SetFieldToUnspecifiedTemplate {
        fact_type,
        field: field_name,
    }
}


#[derive(Template)]
#[template(path = "set-end-state-field-to-specified.html", ext = "html")]
struct SetEndStateFieldToSpecifiedTemplate {
    fact_type: String,
    field: String,
}

async fn set_to_specified_end(
    Path((fact_type, field_name)): Path<(String, String)>,
) -> SetEndStateFieldToSpecifiedTemplate {
    SetEndStateFieldToSpecifiedTemplate {
        fact_type,
        field: field_name,
    }
}

#[derive(Template)]
#[template(path = "set-end-state-field-to-unspecified.html", ext = "html")]
struct SetEndStateFieldToUnspecifiedTemplate {
    fact_type: String,
    field: String,
}

async fn set_to_unspecified_end(
    Path((fact_type, field_name)): Path<(String, String)>,
) -> SetEndStateFieldToUnspecifiedTemplate {
    SetEndStateFieldToUnspecifiedTemplate {
        fact_type,
        field: field_name,
    }
}

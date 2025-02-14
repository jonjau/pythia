use std::{collections::HashMap, fmt};

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

use models::fact::GoalTerm;
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
        .route("/state-changes-leap", get(get_state_changes_leap))
        .route(
            "/state/:state_id/:fact_type/:field_name/specified",
            post(set_to_specified)
            .delete(set_to_unspecified)
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
struct StateChangeTable {
    changes: Vec<StateChange>,
}

struct StateChange {
    before: String,
    after: String,
}

impl fmt::Display for StateChange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {}", self.before, self.after)
    }
}

async fn get_all_state_changes(State(state): State<AppState>) -> StateChangeTable {
    let result = state
        .facts
        .get_all_facts("step_change".to_string())
        .await
        .unwrap();
    let fs = result.iter().map(|f| f.to_string()).collect::<Vec<_>>();

    StateChangeTable { changes: vec![] }
}

async fn get_state_changes(
    State(app_state): State<AppState>,
    Query(states): Query<HashMap<String, String>>,
) -> StateChangeTable {
    let get_named_values = |prefix: &str| {
        states
            .iter()
            .filter_map(|(field, value)| {
                field
                    .starts_with(prefix)
                    .then_some((field[2..].to_string(), GoalTerm::String(value.to_string())))
            })
            .collect::<HashMap<_, _>>()
    };
    let named_values0 = get_named_values("0.");
    let named_values1 = get_named_values("1.");

    let fact_type = states.get("_fact_type").unwrap();
    let subgoal_rt = app_state
        .facts
        .get_record_type(fact_type.to_string())
        .await
        .unwrap();

    let facts = app_state
        .state_changes
        .get_step_changes(subgoal_rt, named_values0, named_values1)
        .await;

    StateChangeTable {
        changes: facts
            .iter()
            .map(|sc| StateChange {
                before: sc.get("Vals1").map(|v| v.to_string()).unwrap(),
                after: sc.get("Vals2").map(|v| v.to_string()).unwrap(),
            })
            .collect::<Vec<_>>(),
    }
}

async fn get_state_changes_leap(
    State(app_state): State<AppState>,
    Query(q): Query<HashMap<String, String>>,
) -> StateChangeTable {
    let get_named_values = |prefix: &str| {
        q
            .iter()
            .filter_map(|(field, value)| {
                field
                    .starts_with(prefix)
                    .then_some((field[prefix.len()..].to_string(), GoalTerm::String(value.to_string())))
            })
            .collect::<HashMap<_, _>>()
    };
    let named_values0 = get_named_values("start.");
    let named_values1 = get_named_values("end.");
    let n_steps = q.get("number-of-steps").unwrap().parse::<i32>().unwrap();

    let fact_type = q.get("_fact_type").unwrap();
    let subgoal_rt = app_state
        .facts
        .get_record_type(fact_type.to_string())
        .await
        .unwrap();

    let facts = app_state
        .state_changes
        .get_leap_changes(subgoal_rt, named_values0, named_values1, n_steps)
        .await;

    StateChangeTable {
        changes: facts
            .iter()
            .map(|sc| StateChange {
                before: sc.get("Vals1").map(|v| v.to_string()).unwrap(),
                after: sc.get("Steps").map(|v| v.to_string()).unwrap(),
            })
            .collect::<Vec<_>>(),
    }
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
    state_id: String,
    fact_type: String,
    field: String,
}

async fn set_to_specified(
    Path((state_id, fact_type, field_name)): Path<(String, String, String)>,
) -> SetFieldToSpecifiedTemplate {
    SetFieldToSpecifiedTemplate {
        state_id,
        fact_type,
        field: field_name,
    }
}

#[derive(Template)]
#[template(path = "set-field-to-unspecified.html", ext = "html")]
struct SetFieldToUnspecifiedTemplate {
    state_id: String,
    fact_type: String,
    field: String,
}

async fn set_to_unspecified(
    Path((state_id, fact_type, field_name)): Path<(String, String, String)>,
) -> SetFieldToUnspecifiedTemplate {
    SetFieldToUnspecifiedTemplate {
        state_id,
        fact_type,
        field: field_name,
    }
}

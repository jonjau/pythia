use std::{collections::HashMap, sync::Arc};

use askama::Template;
use axum::{
    extract::{Query, State}, response::Redirect, routing::{get, post, put}, Form, Json, Router
};

use serde::{Deserialize, Serialize};

mod models;
mod services;

use services::{fact::FactService, state::StateService};

#[derive(Clone)]
struct AppState {
    facts: FactService,
    states: StateService
}

#[tokio::main]
async fn main() {
    let db = include_str!("../data/db.pl");

    // TODO: graceful shutdown of actor
    let state = AppState {
        facts: FactService::new(db),
        states: StateService::new(),
    };

    let r = Router::new()
        .route("/", get(|| async { Redirect::permanent("/facts") }))
        .route("/facts", get(get_facts))
        .route("/facts", post(create_fact))
        .route("/all-state-changes", get(get_all_state_changes))
        .route("/state-changes", get(get_state_changes))
        .route("/start-state", put(update_start_state))

        .with_state(state);

    // run our app with hyper, listening globally on port 3000
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    axum::serve(listener, r).await.unwrap();
}

#[derive(Template)]
#[template(path = "update_start_state_result.html", ext = "html")]
struct UpdateStartStateResult {
    fact_type: String,
    start_state_fields: Vec<String>
}


async fn update_start_state(State(state): State<AppState>, Form(start_state): Form<HashMap<String, String>>) -> UpdateStartStateResult {
    dbg!(&start_state);

    let ft = start_state.get("_fact_type").cloned().unwrap_or("".to_string());

    let clean_keys = start_state.keys().filter(|&k| k != "_fact_type").cloned()
        .collect::<Vec<_>>();

    let rt = state.facts.get_record_type(ft.clone()).await;
    dbg!(rt);

    // TOOD JCJ: pass in the fact_type string and HashMap of form values to the states service
    // the states service should then interface with the logicmachine (via the factservice or otherwise)
    // stateservice.set_start_state(fact_type, values);
    // which would do something like 

    UpdateStartStateResult { fact_type: ft, start_state_fields: clean_keys }
}

struct StartState {
    fields: Vec<String>
}

#[derive(Template)]
#[template(path = "index.html", ext = "html")]
struct FactsPage {
    fact_type: String,
    facts: Vec<String>,
    start_state_fields: Vec<String>
}

#[derive(Deserialize)]
struct Params {
    fact_type: Option<String>,
}

async fn get_facts(query: Query<Params>, State(state): State<AppState>) -> FactsPage {
    match &query.fact_type {
        None => FactsPage { fact_type: "".to_string(), facts: vec![], start_state_fields: vec![] },
        Some(ft) => {
            let result = state.facts.get_all_facts(ft.to_string()).await.unwrap();
            let fs = result.iter().map(|f| f.assertion_str()).collect::<Vec<_>>();

            FactsPage {
                fact_type: result[0].type_name(),
                facts: fs,
                start_state_fields: result[0].fields()
            } 
        }
    }
}

#[derive(Template)]
#[template(path = "fact-table.html", ext = "html")]
struct FactsTable {
    facts: Vec<String>
}

async fn get_all_state_changes(State(state): State<AppState>) -> FactsTable {
    let result = state.facts.get_all_facts("step_change".to_string()).await.unwrap();
    let fs = result.iter().map(|f| f.assertion_str()).collect::<Vec<_>>();

    FactsTable { facts: fs }
}

async fn get_state_changes(State(state): State<AppState>) -> FactsTable {
    let result = state.facts.get_facts("step_change".to_string(),
        vec!["Ctx".to_string(), "\"MR00000002\"".to_string(), "V1".to_string(), "V2".to_string()]).await.unwrap();
    let fs = result.iter().map(|f| f.assertion_str()).collect::<Vec<_>>();

    FactsTable { facts: fs }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
struct CreateFact {
    typename: String,
    values: Vec<String>,
}

use models::fact::RecordType;

// #[debug_handler]
async fn create_fact(State(state): State<AppState>, Json(cf): Json<CreateFact>) -> String {
    dbg!(cf.clone());

    // let rt = Arc::new(RecordType::new("edge", &["X", "Y"]).unwrap());
    // let fact = rt
    //     .to_fact(&HashMap::from([("X", "3"), ("Y", "5")]))
    //     .unwrap();

    let fields_temp = (0..cf.values.len())
        .map(|i| format!("X{}", i))
        .collect::<Vec<_>>();
    let fields = fields_temp.iter().map(String::as_ref).collect::<Vec<_>>();
    let rt0 = Arc::new(RecordType::new(&cf.typename, &fields).unwrap());

    let mapped_values = fields
        .iter()
        .zip(cf.values.iter())
        .map(|(&field, value)| (field, value.as_str()))
        .collect::<HashMap<_, _>>();

    let fact = rt0.to_fact(&mapped_values).unwrap();

    // let res = state.facts.add_fact(fact).await.unwrap();
    // res.to_string()

    let res = state.facts.add_fact(fact).await.unwrap();
    res.len().to_string();

    let result = state.facts.get_all_facts(cf.typename).await.unwrap();
    result.iter().map(|f| f.to_string()).collect::<Vec<_>>().join(",\n")
    // String::new()
}

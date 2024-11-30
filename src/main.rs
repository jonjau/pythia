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
        .route("/facts2", get(get_all_facts_of_type))
        .route("/start-state", put(update_start_state))

        .with_state(state);

    // run our app with hyper, listening globally on port 3000
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    axum::serve(listener, r).await.unwrap();
}

#[derive(Template)]
#[template(path = "update_start_state_result.html", ext = "html")]
struct UpdateStartStateResult {

}

#[derive(Serialize, Deserialize, Clone, Debug)]
struct UpdateStartState {
    #[serde(alias = "X")]
    x: String,
    #[serde(alias = "Y")]
    y: String
}

async fn update_start_state(State(state): State<AppState>, Form(start_state): Form<UpdateStartState>) -> UpdateStartStateResult {
    dbg!(start_state);

    UpdateStartStateResult {}
}

struct StartState {
    attr_names: Vec<String>
}

#[derive(Template)]
#[template(path = "index.html", ext = "html")]
struct FactsPage {
    facts: Vec<String>,
    start_state_attr_names: Vec<String>
}

#[derive(Deserialize)]
struct Params {
    fact_type: Option<String>,
}

async fn get_facts(query: Query<Params>, State(state): State<AppState>) -> FactsPage {
    match &query.fact_type {
        None => FactsPage { facts: vec![], start_state_attr_names: vec![] },
        Some(ft) => {
            let result = state.facts.get_facts(ft.to_string()).await.unwrap();
            let fs = result.iter().map(|f| f.assertion_str()).collect::<Vec<_>>();

            FactsPage { facts: fs, start_state_attr_names: result[0].attr_names() } 
        }
    }

    // let result = state.facts.get_facts(query.fact_type.to_string()).await.unwrap();
    // let fs = result.iter().map(|f| f.assertion_str()).collect::<Vec<_>>();

    // match &query.fact_type {
    //     None => FactsPage { facts: fs, start_state_attr_names: result[0].attr_names() },
    //     Some(q) => FactsPage { facts: vec![], start_state_attr_names: result[0].attr_names() },
    // }
}

async fn get_all_facts_of_type(State(state): State<AppState>) -> FactsPage {
    let result = state.facts.get_facts("arc".to_string()).await.unwrap();
    let fs = result.iter().map(|f| f.assertion_str()).collect::<Vec<_>>();
    // let fs = result.iter().map(|f| f.assertion_str()).collect::<Vec<_>>();

    FactsPage { facts: fs, start_state_attr_names: result[0].attr_names() }
}


#[derive(Serialize, Deserialize, Clone, Debug)]
struct CreateFact {
    typename: String,
    attrs: Vec<String>,
}

use models::fact::RecordType;

// #[debug_handler]
async fn create_fact(State(state): State<AppState>, Json(cf): Json<CreateFact>) -> String {
    dbg!(cf.clone());

    // let rt = Arc::new(RecordType::new("edge", &["X", "Y"]).unwrap());
    // let fact = rt
    //     .to_fact(&HashMap::from([("X", "3"), ("Y", "5")]))
    //     .unwrap();

    let ya = (0..cf.attrs.len())
        .map(|i| format!("X{}", i))
        .collect::<Vec<_>>();
    let attrnames = ya.iter().map(String::as_ref).collect::<Vec<_>>();
    let rt0 = Arc::new(RecordType::new(&cf.typename, &attrnames).unwrap());

    let mapped_attrs = attrnames
        .iter()
        .zip(cf.attrs.iter())
        .map(|(&attrname, attr)| (attrname, attr.as_str()))
        .collect::<HashMap<_, _>>();

    let fact = rt0.to_fact(&mapped_attrs).unwrap();

    // let res = state.facts.add_fact(fact).await.unwrap();
    // res.to_string()

    let res = state.facts.add_fact(fact).await.unwrap();
    res.len().to_string();

    let result = state.facts.get_facts(cf.typename).await.unwrap();
    result.iter().map(|f| f.to_string()).collect::<Vec<_>>().join(",\n")
    // String::new()
}

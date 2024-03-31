use std::{collections::HashMap, sync::Arc};

use askama::Template;
use axum::{
    extract::{Query, State},
    response::Redirect,
    routing::{get, post},
    Json, Router,
};

use axum_macros::debug_handler;
use serde::{Deserialize, Serialize};

mod models;
mod services;

use services::fact::FactService;

#[derive(Clone)]
struct AppState {
    facts: FactService,
}

#[tokio::main]
async fn main() {
    let db = include_str!("../data/db.pl");

    // TODO: graceful shutdown of actor
    let state = AppState {
        facts: FactService::new(db),
    };

    let r = Router::new()
        .route("/", get(|| async { Redirect::permanent("/facts") }))
        .route("/facts", get(get_facts))
        .route("/facts", post(create_fact))
        .with_state(state);

    // run our app with hyper, listening globally on port 3000
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    axum::serve(listener, r).await.unwrap();
}

#[derive(Template)]
#[template(path = "index.html", ext = "html")]
struct FactsPage {
    facts: Vec<String>,
}

#[derive(Deserialize)]
struct Params {
    q: Option<String>,
}

async fn get_facts(query: Query<Params>, State(state): State<AppState>) -> FactsPage {
    let result = state.facts.get_facts("dimlink".to_string()).await.unwrap();
    let fs = result.iter().map(|f| f.assertion_str()).collect::<Vec<_>>();

    match &query.q {
        None => FactsPage { facts: fs },
        Some(q) => FactsPage { facts: vec![] },
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
struct CreateFact {
    typename: String,
    attrs: Vec<String>,
}

// async fn create_fact(State(state): State<AppState>>, Json(payload): Json<CreateFact>) -> Json<CreateFact> {
// // let mut state = state.write().await;
// state.logic_machine.add_fact(payload.fact.clone());
// Json(payload)
// }

use models::fact::Fact;
use models::fact::RecordType;

// #[debug_handler]
async fn create_fact(State(state): State<AppState>, Json(cf): Json<CreateFact>) -> String {
    dbg!(cf);

    let rt = Arc::new(RecordType::new("edge", &["X", "Y"]).unwrap());
    let fact = rt
        .to_fact(&HashMap::from([("X", "3"), ("Y", "5")]))
        .unwrap();

    // let ya = (0..cf.attrs.len()).map(|i| format!("X{}", i)).collect::<Vec<_>>();
    // let attrnames = ya.iter().map(String::as_ref).collect::<Vec<_>>();
    // let rt0 = Arc::new(RecordType::new(
    //     &cf.typename,
    //     &attrnames
    // )).unwrap();

    // let it = attrnames.iter().zip(cf.attrs.iter());


    let res = state.facts.add_fact(fact).await.unwrap();
    res.len().to_string()
    // String::new()
}

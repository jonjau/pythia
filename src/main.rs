use askama::Template;
use axum::{
    extract::{Query, State},
    response::Redirect,
    routing::{get, post},
    Json, Router,
};

use serde::{Deserialize, Serialize};

mod models;
mod services;

use services::fact::FactService;

#[derive(Clone)]
struct AppState {
    lm: FactService,
}

#[tokio::main]
async fn main() {
    let state = AppState {
        lm: FactService::new(),
    };

    let r = Router::new()
        .route("/", get(|| async { Redirect::permanent("/contacts") }))
        .route("/contacts", get(get_facts))
        // .route("/facts", post(create_fact))
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
    let state = state.lm;
    let result = state.test_query().await;

    match &query.q {
        None => FactsPage { facts: vec![] },
        Some(q) => FactsPage { facts: vec![] },
    }
}

#[derive(Serialize, Deserialize)]
struct CreateFact {
    fact: String,
}

// async fn create_fact(State(state): State<Arc<Mutex<AppState>>>, Json(payload): Json<CreateFact>) -> Json<CreateFact> {
// // let mut state = state.write().await;
// state.logic_machine.add_fact(payload.fact.clone());
// Json(payload)
// }

use std::fs;

use askama::Template;
use axum::{
    extract::State,
    routing::get, Router,
};

use routes::{fact::fact_routes, state_change::state_change_routes};

mod models;
mod services;
mod routes;
mod utils;

use services::{
    fact::FactService,
    logic_machine::LogicMachineService,
    state_change::StateChangeService,
};
use utils::codegen::generate_main_prolog_program;

#[derive(Clone)]
pub struct AppState {
    lm: LogicMachineService,
    facts: FactService,
    state_changes: StateChangeService,
}

#[tokio::main]
async fn main() {
    let p = generate_main_prolog_program().unwrap();
    fs::write("data/internal/pythia.pl", p).unwrap();

    let data =
        std::fs::read_to_string("data/internal/pythia.pl").expect("Failed to read pythia.pl");
    let lm = LogicMachineService::new(&data, "data/types.json");

    // TODO: graceful shutdown of actor
    let state = AppState {
        lm: lm.clone(),
        facts: FactService::new(lm.clone()),
        state_changes: StateChangeService::new(lm.clone()),
    };

    let r = Router::new()
        .route("/", get(get_inquiries))
        .merge(state_change_routes())
        .merge(fact_routes())
        .with_state(state);

    // run our app with hyper, listening globally on port 3000
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    axum::serve(listener, r).await.unwrap();
}

#[derive(Template)]
#[template(path = "index.html")]
struct GetInquiriesTemplate {
    record_types: Vec<String>,
}

async fn get_inquiries(State(app_state): State<AppState>) -> GetInquiriesTemplate {
    let rts = app_state.lm.get_all_record_types().await.unwrap();

    GetInquiriesTemplate {
        record_types: rts.iter().map(|rt| rt.name.clone()).collect(),
    }
}


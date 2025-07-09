use askama::Template;
use axum::{extract::State, routing::get, Router};
use tower_http::services::ServeDir;

use log::info;
use routes::{fact::fact_routes, state_change::state_change_routes};

mod models;
mod routes;
mod services;
mod utils;

use crate::utils::codegen::generate_prolog_programs;
use services::{
    fact::FactService, logic_machine::LogicMachineService, state_change::StateChangeService,
};

#[derive(Clone)]
pub struct AppState {
    lm: LogicMachineService,
    facts: FactService,
    state_changes: StateChangeService,
}

#[tokio::main]
async fn main() {
    env_logger::init();
    info!("Starting pythia...");

    generate_prolog_programs().expect("Failed to generate prolog programs");

    let program_data =
        std::fs::read_to_string("data/internal/pythia.pl").expect("Failed to read pythia.pl");
    let lm = LogicMachineService::new(&program_data, "data/types.json")
        .expect("Failed to start LogicMachine service");

    let state = AppState {
        lm: lm.clone(),
        facts: FactService::new(lm.clone()),
        state_changes: StateChangeService::new(lm.clone()),
    };

    let r = Router::new()
        .nest_service("/static", axum::routing::get_service(ServeDir::new("src/static")))
        .route("/", get(get_inquiries))
        .merge(state_change_routes())
        .merge(fact_routes())
        .with_state(state);

    // run our app with hyper, listening globally on port 3000
    let addr = "0.0.0.0:3000";
    let listener = tokio::net::TcpListener::bind(addr)
        .await
        .expect(&format!("Failed to create TCPListener on {}.", addr));
    info!("Listening on {}...", addr);
    axum::serve(listener, r)
        .await
        .expect(&format!("Failed to start axum server on {}.", addr));
}

#[derive(Template)]
#[template(path = "inquiries.html")]
struct GetInquiriesTemplate {
    record_types: Vec<String>,
}

async fn get_inquiries(State(app_state): State<AppState>) -> GetInquiriesTemplate {
    let rts = app_state
        .lm
        .get_all_record_types()
        .await
        .unwrap_or_default();

    GetInquiriesTemplate {
        record_types: rts.iter().map(|rt| rt.name.clone()).collect(),
    }
}

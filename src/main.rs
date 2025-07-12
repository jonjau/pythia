use askama::Template;
use axum::{extract::State, routing::get, Router};
use tower_http::services::ServeDir;

use log::info;
use routes::{fact::fact_routes, state_change::state_change_routes};

mod models;
mod routes;
mod services;
mod utils;

use services::{
    fact::FactService, logic_machine::LogicMachineService, state_change::StateChangeService,
};

/// Shared application state used by all handlers and services.
///
/// This includes:
/// - `LogicMachineService`: The logic engine running Prolog programs.
/// - `FactService`: Handles operations related to facts.
/// - `StateChangeService`: Handles state change operations.
#[derive(Clone)]
pub struct AppState {
    lm: LogicMachineService,
    facts: FactService,
    state_changes: StateChangeService,
}


/// Main entry point of the Pythia application.
///
/// This function:
/// - Initializes logging.
/// - Generates necessary Prolog programs at start-up.
/// - Initializes all core services.
/// - Builds the Axum router and mounts static files, routes, and handlers.
/// - Starts the HTTP server on port 3000.
#[tokio::main]
async fn main() {
    env_logger::init();
    info!("Starting pythia...");

    // Generate logic programs (Prolog code) before launching
    utils::codegen::generate_prolog_programs().expect("Failed to generate prolog programs");

    // Load and initialize logic machine with Prolog code and type definitions
    let program_data =
        std::fs::read_to_string("data/internal/pythia.pl").expect("Failed to read pythia.pl");
    let lm = LogicMachineService::new(&program_data, "data/types.json")
        .expect("Failed to start LogicMachine service");

    // Initialise Pythia application state and services
    let state = AppState {
        lm: lm.clone(),
        facts: FactService::new(lm.clone()),
        state_changes: StateChangeService::new(lm.clone()),
    };

    // Build the Axum router
    let r = Router::new()
        .nest_service("/static", axum::routing::get_service(ServeDir::new("static")))
        .route("/", get(get_inquiries))
        .merge(state_change_routes())
        .merge(fact_routes())
        .with_state(state);

    // Run the HTTP server
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

/// Handler for GET requests to `/`.
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

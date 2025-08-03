use axum::{middleware::from_fn_with_state, Router};
use tower_http::services::ServeDir;

use log::info;
use routes::{fact::fact_routes, state_change::state_change_routes};

mod middleware;
mod models;
mod routes;
mod services;
mod utils;

use services::{
    fact::FactService, logic_machine::LogicMachineService, state_change::StateChangeService,
};

use crate::{
    middleware::session::require_session,
    routes::{
        inquiry::inquiry_routes, knowledge_base::knowledge_base_routes,
        record_type::record_type_routes, session::session_routes,
    },
    services::{db::DbService, session::SessionService},
};

#[derive(Clone)]
pub struct GlobalAppState {
    sessions: SessionService,
}

/// Shared application state used by all handlers and services.
///
/// This includes:
/// - `LogicMachineService`: The logic engine running Prolog programs.
/// - `FactService`: Handles operations related to facts.
/// - `StateChangeService`: Handles state change operations.
#[derive(Clone)]
pub struct AppState {
    db: DbService,
    lm: LogicMachineService,
    facts: FactService,
    state_changes: StateChangeService,
}

/// Main entry point of the Pythia application.
#[tokio::main]
async fn main() {
    env_logger::init();

    info!("Starting Pythia...");

    let global_state = GlobalAppState {
        sessions: SessionService::new(),
    };

    let routes_sessionless = session_routes().with_state(global_state.clone());
    let routes = Router::new()
        .merge(inquiry_routes())
        .merge(state_change_routes())
        .merge(fact_routes())
        .merge(record_type_routes())
        .merge(knowledge_base_routes())
        .layer(from_fn_with_state(global_state.clone(), require_session));

    let r = Router::new()
        .nest_service(
            "/static",
            axum::routing::get_service(ServeDir::new("static")),
        )
        .merge(routes_sessionless)
        .nest("/", routes);

    // Run the HTTP server
    let addr = "0.0.0.0:3000";
    let listener = tokio::net::TcpListener::bind(addr)
        .await
        .expect(&format!("Failed to create TCPListener on {}.", addr));
    info!("Listening on {}...", addr);
    axum::serve(listener, r)
        .with_graceful_shutdown(shutdown_signal())
        .await
        .expect(&format!("Failed to start axum server on {}.", addr));
}

async fn shutdown_signal() {
    tokio::signal::ctrl_c()
        .await
        .expect("Failed to install Ctrl+C handler");
    info!("Shutting down Pythia...");
}

use askama::Template;
use axum::{extract::State, middleware::from_fn, routing::get, Router};
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
    middleware::user_token::{set_user_token, UserToken},
    routes::{knowledge_base::knowledge_base_routes, record_type::record_type_routes},
    services::db::DbService,
};

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

    // Generates knowledge base from persistence layer (i.e. DB) at start-up.
    // let db = DbService::new("admin".to_owned()).await;
    let db = DbService::new_local().await;
    db.create_table_if_not_exists("pythia", "pk", "sk")
        .await
        .expect("Failed to create essential table");
    db.update_knowledge_base()
        .await
        .expect("Failed to update knowledge base");

    // Load knowledge base in Prolog
    let lm = LogicMachineService::new(db.clone())
        .await
        .expect("Failed to start LogicMachine service");

    // Initialise Pythia application state and services
    let state = AppState {
        db: db.clone(),
        lm: lm.clone(),
        facts: FactService::new(lm.clone(), db.clone()),
        state_changes: StateChangeService::new(lm.clone()),
    };

    // Build the Axum router, mount static files, routes, and handlers
    let r = Router::new()
        .nest_service(
            "/static",
            axum::routing::get_service(ServeDir::new("static")),
        )
        .route("/", get(get_inquiries))
        .merge(state_change_routes())
        .merge(fact_routes())
        .merge(record_type_routes())
        .merge(knowledge_base_routes())
        // .route("/sessions", post(create_session))
        // .route("/me", get(me))
        .layer(from_fn(set_user_token))
        .with_state(state);

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

#[derive(Template)]
#[template(path = "inquiries.html")]
struct GetInquiriesTemplate {
    user_token: String,
    record_types: Vec<String>,
}

/// Handler for GET requests to `/`.
async fn get_inquiries(
    UserToken(user_token): UserToken,
    State(app_state): State<AppState>,
) -> GetInquiriesTemplate {
    let rts = app_state
        .lm
        .get_all_record_types()
        .await
        .unwrap_or_default();

    GetInquiriesTemplate {
        user_token,
        record_types: rts.iter().map(|rt| rt.name.clone()).collect(),
    }
}

// // #[axum::debug_handler]
// async fn create_session(
//     jar: CookieJar,
// ) -> Result<(CookieJar, Redirect), StatusCode> {
//     // if let Some(session_id) = authorize_and_create_session(auth.token()).await {
//     //     Ok((
//     //         // the updated jar must be returned for the changes
//     //         // to be included in the response
//     //         jar.add(Cookie::new("session_id", "123".to_string())),
//     //         Redirect::to("/me"),
//     //     ))
//     // } else {
//     //     Err(StatusCode::UNAUTHORIZED)
//     // }

//         Ok((
//             // the updated jar must be returned for the changes
//             // to be included in the response
//             jar.add(Cookie::new("session_id", "123".to_string())),
//             Redirect::to("/me"),
//         ))
// }

// // #[axum::debug_handler]
// async fn me(jar: CookieJar) -> Result<String, StatusCode> {
//     if let Some(session_id) = jar.get("session_id") {
//         Ok(session_id.to_string())
//     } else {
//         Err(StatusCode::UNAUTHORIZED)
//     }
// }

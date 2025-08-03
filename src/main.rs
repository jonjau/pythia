use std::collections::HashMap;

use askama::Template;
use axum::{
    extract::State, middleware::from_fn_with_state, response::Redirect, routing::get, Form, Router,
};
use axum_extra::extract::{
    cookie::{Cookie, SameSite},
    CookieJar,
};
use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine as _};
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
use uuid::Uuid;

use crate::{
    middleware::session::{require_session, UserToken},
    routes::{knowledge_base::knowledge_base_routes, record_type::record_type_routes},
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

    let routes = Router::new()
        .route(
            "/sessions",
            get(show_start_session_page).post(start_session),
        )
        .with_state(global_state.clone());

    let routes_requiring_session = Router::new()
        .route("/", get(get_inquiries))
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
        .merge(routes)
        .nest("/", routes_requiring_session);

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
#[template(path = "start-session.html")]
struct StartSessionTemplate {
    user_token: String,
}

async fn show_start_session_page() -> StartSessionTemplate {
    StartSessionTemplate {
        user_token: "".to_string(),
    }
}

async fn start_session(
    State(state): State<GlobalAppState>,
    cookies: CookieJar,
    Form(form): Form<HashMap<String, String>>,
) -> (CookieJar, Redirect) {
    const TOKEN_COOKIE_NAME: &str = "user_token";
    const COOKIE_MAX_AGE_DAYS: i64 = 5;

    let token = form
        .get("user_token")
        .and_then(|token| {
            URL_SAFE_NO_PAD
                .decode(token)
                .ok()
                .and_then(|bytes| Uuid::from_slice(&bytes).ok().map(|_| token.clone()))
        })
        .unwrap_or_else(|| URL_SAFE_NO_PAD.encode(Uuid::new_v4().as_bytes()));

    state.sessions.start_session(token.clone()).await;

    let cookie = Cookie::build((TOKEN_COOKIE_NAME, token))
        .path("/")
        .http_only(true)
        // .secure(true)
        .same_site(SameSite::Lax)
        .max_age(time::Duration::days(COOKIE_MAX_AGE_DAYS));

    (cookies.add(cookie), Redirect::to("/"))
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
    app_state: AppState,
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

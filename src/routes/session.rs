use std::collections::HashMap;

use askama::Template;
use axum::{extract::State, response::Redirect, routing::get, Form, Router};
use axum_extra::extract::{
    cookie::{Cookie, SameSite},
    CookieJar,
};
use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine as _};
use uuid::Uuid;

use crate::GlobalAppState;

/// Returns the routes for starting sessions.
pub fn session_routes() -> Router<GlobalAppState> {
    Router::new().route(
        "/sessions",
        get(show_start_session_page).post(start_session),
    )
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

    let _ = state.sessions.start_session(token.clone()).await;

    let cookie = Cookie::build((TOKEN_COOKIE_NAME, token))
        .path("/")
        .http_only(true)
        // .secure(true)
        .same_site(SameSite::Lax)
        .max_age(time::Duration::days(COOKIE_MAX_AGE_DAYS));

    (cookies.add(cookie), Redirect::to("/"))
}

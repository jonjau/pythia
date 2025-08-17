use std::collections::HashMap;

use askama::Template;
use axum::{
    extract::{Query, State},
    response::Redirect,
    routing::{get, post},
    Form, Router,
};
use axum_extra::extract::{
    cookie::{Cookie, SameSite},
    CookieJar,
};
use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine as _};
use serde::Deserialize;
use uuid::Uuid;

use crate::GlobalAppState;

/// Returns the routes for starting sessions.
pub fn session_routes() -> Router<GlobalAppState> {
    Router::new()
        .route(
            "/sessions",
            get(show_start_session_page).post(start_session),
        )
        .route("/sessions/resume", post(resume_session))
}

#[derive(Template)]
#[template(path = "start-session.html")]
struct StartSessionTemplate {
    user_token: String,
    error: bool,
}

#[derive(Debug, Deserialize)]
struct Params {
    error: Option<bool>,
}

async fn show_start_session_page(Query(p): Query<Params>) -> StartSessionTemplate {
    StartSessionTemplate {
        user_token: "".to_string(),
        error: p.error.unwrap_or_default(),
    }
}

async fn start_session(
    State(state): State<GlobalAppState>,
    cookies: CookieJar,
) -> Result<(CookieJar, Redirect), StartSessionTemplate> {
    let token = URL_SAFE_NO_PAD.encode(Uuid::new_v4().as_bytes());

    state
        .sessions
        .start_session(token.clone())
        .await
        .map_err(|_| StartSessionTemplate {
            user_token: "".to_owned(),
            error: true,
        })?;

    let cookie = create_cookie(token);

    Ok((cookies.add(cookie), Redirect::to("/")))
}

async fn resume_session(
    State(state): State<GlobalAppState>,
    cookies: CookieJar,
    Form(form): Form<HashMap<String, String>>,
) -> Result<(CookieJar, Redirect), Redirect> {
    let token = form
        .get("user_token")
        .and_then(|token| {
            URL_SAFE_NO_PAD
                .decode(token)
                .ok()
                .and_then(|bytes| Uuid::from_slice(&bytes).ok().map(|_| token.clone()))
        })
        .ok_or_else(|| Redirect::to("/sessions?error=true"))?;

    state
        .sessions
        .start_session(token.clone())
        .await
        .map_err(|_| Redirect::to("/sessions?error=true"))?;

    let cookie = create_cookie(token);

    return Ok((cookies.add(cookie), Redirect::to("/")));
}

const TOKEN_COOKIE_NAME: &str = "user_token";
const COOKIE_MAX_AGE_DAYS: i64 = 5;

fn create_cookie(token: String) -> Cookie<'static> {
    Cookie::build((TOKEN_COOKIE_NAME, token))
        .path("/")
        .http_only(true)
        // .secure(true)
        .same_site(SameSite::Lax)
        .max_age(time::Duration::days(COOKIE_MAX_AGE_DAYS))
        .expires(time::OffsetDateTime::now_utc() + time::Duration::days(COOKIE_MAX_AGE_DAYS))
        .build()
}

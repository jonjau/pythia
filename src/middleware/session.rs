use axum::{
    async_trait,
    body::Body,
    extract::{FromRequestParts, State},
    http::{request::Parts, Request},
    middleware::Next,
    response::{IntoResponse, Redirect},
};

use crate::{AppState, GlobalAppState};

const TOKEN_COOKIE_NAME: &str = "user_token";

pub async fn require_session(
    State(global): State<GlobalAppState>,
    mut req: Request<Body>,
    next: Next,
) -> impl IntoResponse {
    let token_prefix = format!("{}=", TOKEN_COOKIE_NAME);

    let token = req
        .headers()
        .get("cookie")
        .and_then(|h| h.to_str().ok())
        .and_then(|s| {
            s.split(';').find_map(|kv| {
                let kv = kv.trim();
                kv.strip_prefix(&token_prefix).map(|v| v.to_string())
            })
        });

    match token {
        Some(t) => {
            let state = global.sessions.get_session_state(&t).await;
            if let Some(state) = state {
                req.extensions_mut().insert(t);
                req.extensions_mut().insert(state);
                next.run(req).await
            } else {
                Redirect::to("/sessions").into_response()
            }
        }
        None => Redirect::to("/sessions").into_response(),
    }
}

pub struct UserToken(pub String);

#[async_trait]
impl<S> FromRequestParts<S> for UserToken
where
    S: Send + Sync,
{
    type Rejection = Redirect;

    async fn from_request_parts(parts: &mut Parts, _: &S) -> Result<Self, Self::Rejection> {
        parts
            .extensions
            .get::<String>()
            .cloned()
            .map(UserToken)
            .ok_or(Redirect::to("/sessions"))
    }
}

#[async_trait]
impl<S> FromRequestParts<S> for AppState
where
    S: Send + Sync,
{
    type Rejection = Redirect;

    async fn from_request_parts(parts: &mut Parts, _: &S) -> Result<Self, Self::Rejection> {
        parts
            .extensions
            .get::<AppState>()
            .cloned()
            .ok_or(Redirect::to("/sessions"))
    }
}

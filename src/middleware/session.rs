use axum::{
    async_trait,
    body::Body,
    extract::{FromRequestParts, State},
    http::{request::Parts, Request},
    middleware::Next,
    response::{IntoResponse, Redirect},
};
use axum_extra::extract::CookieJar;

use crate::{services::session::AppState, GlobalAppState};

const TOKEN_COOKIE_NAME: &str = "user_token";

pub async fn require_session(
    State(global): State<GlobalAppState>,
    cookies: CookieJar,
    mut req: Request<Body>,
    next: Next,
) -> impl IntoResponse {
    if let Some(cookie) = cookies.get(TOKEN_COOKIE_NAME) {
        let token = cookie.value();
        match global.sessions.get_session_state(token).await {
            Ok(Some(state)) => {
                req.extensions_mut().insert(token.to_string());
                req.extensions_mut().insert(state);
                next.run(req).await
            }
            _ => Redirect::to("/sessions").into_response(),
        }
    } else {
        Redirect::to("/sessions").into_response()
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

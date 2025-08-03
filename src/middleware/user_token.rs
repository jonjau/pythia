use axum::{
    async_trait,
    body::Body,
    extract::FromRequestParts,
    http::{request::Parts, Request},
    middleware::Next,
    response::IntoResponse,
};
use axum_extra::extract::cookie::{Cookie, CookieJar, SameSite};
use base64::{engine::general_purpose::URL_SAFE_NO_PAD, Engine as _};
use uuid::Uuid;

const TOKEN_COOKIE_NAME: &str = "user_token";
const COOKIE_MAX_AGE_DAYS: i64 = 5;

pub async fn set_user_token(
    jar: CookieJar,
    mut request: Request<Body>,
    next: Next,
) -> impl IntoResponse {

    // Generate a new user token if it doesn't exist in the cookie jar or it is in an invalid format
    // Token format is UUIDv4 encoded as base64, URL-safe, no padding
    let user_token = jar
        .get(TOKEN_COOKIE_NAME)
        .and_then(|cookie| {
            URL_SAFE_NO_PAD
                .decode(cookie.value())
                .ok()
                .and_then(|bytes| Uuid::from_slice(&bytes).ok())
                .map(|uuid| uuid.to_string())
        }).unwrap_or_else(|| URL_SAFE_NO_PAD.encode(Uuid::new_v4().as_bytes()));

    request.extensions_mut().insert(user_token.clone());

    let mut res = next.run(request).await;

    if jar.get(TOKEN_COOKIE_NAME).is_none() {
        let cookie = Cookie::build((TOKEN_COOKIE_NAME, user_token))
            .path("/")
            .http_only(true)
            .secure(true)
            .same_site(SameSite::Lax)
            .max_age(time::Duration::days(COOKIE_MAX_AGE_DAYS));

        res.headers_mut().append(
            axum::http::header::SET_COOKIE,
            cookie.to_string().parse().unwrap(),
        );
    }

    res
}

pub struct UserToken(pub String);

#[async_trait]
impl<S> FromRequestParts<S> for UserToken
where
    S: Send + Sync,
{
    type Rejection = axum::http::StatusCode;

    async fn from_request_parts(parts: &mut Parts, _: &S) -> Result<Self, Self::Rejection> {
        parts
            .extensions
            .get::<String>()
            .cloned()
            .map(UserToken)
            .ok_or(axum::http::StatusCode::UNAUTHORIZED)
    }
}

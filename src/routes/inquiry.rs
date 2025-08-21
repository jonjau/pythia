use askama::Template;
use axum::{routing::get, Router};

use crate::{middleware::session::UserToken, services::session::AppState};

/// Returns the routes for starting inquiries.
pub fn inquiry_routes() -> Router {
    Router::new().route("/", get(get_inquiries))
}

#[derive(Template)]
#[template(path = "inquiries.html")]
struct GetInquiriesTemplate {
    page: String,
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
        page: "inquiries".to_owned(),
        user_token,
        record_types: rts.iter().map(|rt| rt.name.clone()).collect(),
    }
}

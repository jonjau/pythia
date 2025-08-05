use askama::Template;
use axum::{routing::get, Router};

use crate::{
    middleware::session::UserToken, models::record_type::RecordTypeData,
    services::session::AppState,
};

/// Returns the routes for getting, creating and deleting record types
pub fn record_type_routes() -> Router {
    Router::new()
        .nest(
            "/api",
            crate::routes::api::record_type::record_type_routes(),
        )
        .route("/record-types", get(get_record_types_page))
        .route("/record-type/new", get(get_new_record_type_form))
}

#[derive(Template)]
#[template(path = "record-type/record-types.html")]
struct RecordTypesPageTemplate {
    user_token: String,
    record_types: Vec<RecordTypeData>,
}

async fn get_record_types_page(
    UserToken(user_token): UserToken,
    state: AppState,
) -> RecordTypesPageTemplate {
    let record_types: Vec<RecordTypeData> =
        state.db.get_all_record_types().await.unwrap_or_default();

    RecordTypesPageTemplate {
        user_token,
        record_types,
    }
}

#[derive(Template)]
#[template(path = "record-type/new-record-type.html")]
struct NewRecordTypeFormTemplate {}

async fn get_new_record_type_form() -> NewRecordTypeFormTemplate {
    NewRecordTypeFormTemplate {}
}

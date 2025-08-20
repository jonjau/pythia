use askama::Template;
use axum::{
    routing::{delete, get},
    Form, Router,
};

use crate::{
    middleware::session::UserToken,
    models::record_type::RecordTypeData,
    services::{record_type::CreateRecordTypeFormData, session::AppState},
};

/// Returns the routes for getting, creating and deleting record types
pub fn record_type_routes() -> Router {
    Router::new()
        .nest(
            "/api",
            crate::routes::api::record_type::record_type_routes(),
        )
        .route(
            "/record-types",
            get(get_record_types_page).post(create_record_type),
        )
        .route(
            "/record-types/new",
            get(get_new_record_type_form).delete(get_add_record_type_button),
        )
        .route("/record-types/:rt_name", delete(delete_record_type))
}

#[derive(Template)]
#[template(path = "record-type/record-types.html")]
struct RecordTypesPageTemplate {
    page: String,
    user_token: String,
    record_types: Vec<RecordTypeData>,
}

async fn get_record_types_page(
    UserToken(user_token): UserToken,
    state: AppState,
) -> RecordTypesPageTemplate {
    let record_types = state
        .record_types
        .get_all_record_types()
        .await
        .unwrap_or_default();

    RecordTypesPageTemplate {
        page: "record-types".to_owned(),
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

#[derive(Template)]
#[template(path = "record-type/add-new-record-type-button.html")]
struct AddNewRecordTypeButtonTemplate {}

async fn get_add_record_type_button() -> AddNewRecordTypeButtonTemplate {
    AddNewRecordTypeButtonTemplate {}
}

#[derive(Template)]
#[template(path = "record-type/record-type-table.html")]
struct RecordTypeTableTemplate {
    record_types: Vec<RecordTypeData>,
}

async fn create_record_type(
    state: AppState,
    Form(f): Form<CreateRecordTypeFormData>,
) -> RecordTypeTableTemplate {
    state
        .record_types
        .add_record_type(f)
        .await
        .expect("Failed to create record type");

    let record_types: Vec<RecordTypeData> =
        state.db.get_all_record_types().await.unwrap_or_default();

    // TODO JCJ: Handle errors properly

    RecordTypeTableTemplate { record_types }
}

async fn delete_record_type(
    state: AppState,
    axum::extract::Path(rt_name): axum::extract::Path<String>,
) -> RecordTypeTableTemplate {
    state
        .record_types
        .delete_record_type(&rt_name)
        .await
        .expect("Failed to delete record type");

    let record_types = state.db.get_all_record_types().await.unwrap_or_default();

    RecordTypeTableTemplate { record_types }
}

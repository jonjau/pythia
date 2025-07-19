use crate::{models::record_type::{self, RecordTypeJson}, AppState};
use axum::{
    extract::{Path, State},
    routing::{delete, get, post},
    Json, Router,
};
use log::info;
use serde_json::{json, Value};

/// Returns the routes for getting, creating and deleting record types
pub fn record_type_routes() -> Router<AppState> {
    Router::new()
        .route(
            "/api/record_types",
            get(get_record_types).post(create_record_type),
        )
        .route("/api/record_types/:name", delete(delete_record_type))
        .route("/api/reload_record_types", post(reload_record_types))
}

async fn get_record_types(State(state): State<AppState>) -> Json<Value> {
    match state.db.get_all_record_types().await {
        Ok(record_types) => Json(json!({"record_types": record_types})),
        Err(e) => {
            info!("Failed to get record types: {}", e);
            Json(json!({"error": format!("Failed to get record types: {}", e)}))
        }
    }
}

async fn create_record_type(
    State(state): State<AppState>,
    Json(record_type): Json<RecordTypeJson>,
) -> Json<Value> {
    match state
        .db
        .put_record_type(record_type.clone())
        .await
    {
        Ok(_) => Json(json!({"record_type": record_type})),
        Err(e) => {
            info!("Failed to create record type: {}", e);
            Json(json!({"error": format!("Failed to create record type: {}", e)}))
        }
    }
}

async fn delete_record_type(
    State(state): State<AppState>,
    Path(name): Path<String>,
) -> Json<Value> {
    let db = state.db.clone();
    match db.delete_record_type(&name).await {
        Ok(_) => Json(json!({"message": "Record type deleted successfully"})),
        Err(e) => {
            info!("Failed to delete record type: {}", e);
            Json(json!({"error": format!("Failed to delete record type: {}", e)}))
        }
    }
}

async fn reload_record_types(State(state): State<AppState>) -> Json<Value> {
    let res = state.db.dump_to_files("dimlink").await;
    info!("Dumped record types to files: {:?}", res);
    match state.db.get_all_record_types().await {
        Ok(record_types) => Json(json!({"record_types": record_types})),
        Err(e) => {
            info!("Failed to reload record types: {}", e);
            Json(json!({"error": format!("Failed to reload record types: {}", e)}))
        }
    }
}

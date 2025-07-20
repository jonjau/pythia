use crate::{models::record_type::RecordTypeData, AppState};
use axum::{
    extract::{Path, State},
    routing::{get, post},
    Json, Router,
};
use log::info;
use serde_json::{json, Value};

/// Returns the routes for getting, creating and deleting record types
pub fn record_type_routes() -> Router<AppState> {
    Router::new()
        .route(
            "/api/record-types",
            get(get_record_types).post(create_record_type),
        )
        .route(
            "/api/record-types/:name",
            get(get_record_type).delete(delete_record_type),
        )
        .route("/api/record-types/reload", post(reload_record_types))
}

async fn get_record_types(State(state): State<AppState>) -> Result<Json<Value>, Json<Value>> {
    match state.db.get_all_record_types().await {
        Ok(record_types) => Ok(Json(json!({"record_types": record_types}))),
        Err(e) => {
            info!("Failed to get record types: {}", e);
            Err(Json(
                json!({"error": format!("Failed to get record types: {}", e)}),
            ))
        }
    }
}

async fn get_record_type(
    State(state): State<AppState>,
    Path(name): Path<String>,
) -> Result<Json<Value>, Json<Value>> {
    match state.db.get_record_type(&name).await {
        Ok(record_type) => Ok(Json(json!({"record_type": record_type}))),
        Err(e) => {
            info!("Failed to get record type: {}", e);
            Err(Json(
                json!({"error": format!("Failed to get record type '{}': {}", name, e)}),
            ))
        }
    }
}

async fn create_record_type(
    State(state): State<AppState>,
    Json(record_type): Json<RecordTypeData>,
) -> Result<Json<Value>, Json<Value>> {
    match state.db.put_record_type(&record_type).await {
        Ok(_) => {
            info!("Created record type: {}", record_type.name);
            Ok(Json(
                json!({"message": format!("Record type '{}' created successfully", record_type.name)}),
            ))
        }
        Err(e) => {
            info!("Failed to create record type: {}", e);
            Err(Json(
                json!({"error": format!("Failed to create record type '{}': {}", record_type.name, e)}),
            ))
        }
    }
}

async fn delete_record_type(
    State(state): State<AppState>,
    Path(name): Path<String>,
) -> Result<Json<Value>, Json<Value>> {
    match state.db.delete_record_type(&name).await {
        Ok(_) => {
            info!("Deleted record type: {}", name);
            Ok(Json(
                json!({"message": format!("Record type '{}' deleted successfully", name)}),
            ))
        }
        Err(e) => {
            info!("Failed to delete record type: {}", e);
            Err(Json(
                json!({"error": format!("Failed to delete record type '{}': {}", name, e)}),
            ))
        }
    }
}

async fn reload_record_types(State(state): State<AppState>) -> Result<Json<Value>, Json<Value>> {
    // TODO JCJ: full reload in 1 function

    state
        .db
        .generate_data_files()
        .await
        .map_err(|e| Json(json!({"error": format!("Failed to generate Prolog files: {}", e)})))?;

    let program = std::fs::read_to_string("data/internal/pythia.pl").map_err(|e| {
        Json(json!({"error": format!("Failed to read internal Pythia Prolog file: {}", e)}))
    })?;

    state
        .lm
        .reload_actor(&program, "data/types.json")
        .await
        .map_err(|e| {
            Json(json!({"error": format!("Failed to reload Logic Machine actor: {}", e)}))
        })?;

    Ok(Json(
        json!({"message": "Record types reloaded successfully"}),
    ))
}

use axum::{routing::post, Json, Router};
use serde_json::{json, Value};

use crate::services::session::AppState;

/// Returns the routes for reloading and exporting the knowledge base, i.e. the set of Prolog rules and facts stored in the database, that the Logic Machine loads.
pub fn knowledge_base_routes() -> Router {
    Router::new()
        .route("/api/knowledge-base/reload", post(reload_knowledge_base))
        .route("/api/knowledge-base/export", post(export_knowledge_base))
}

async fn reload_knowledge_base(state: AppState) -> Result<Json<Value>, Json<Value>> {
    state.db.update_knowledge_base().await.map_err(|e| {
        Json(json!({"error": format!("Failed to update Prolog knowledge base: {}", e)}))
    })?;

    state
        .lm
        .reload()
        .await
        .map_err(|e| Json(json!({"error": format!("Failed to reload Logic Machine: {}", e)})))?;

    Ok(Json(
        json!({"message": "Logic Machine reloaded successfully"}),
    ))
}

async fn export_knowledge_base(state: AppState) -> Result<Json<Value>, Json<Value>> {
    state
        .db
        .export_knowledge_base()
        .await
        .map_err(|e| Json(json!({"error": format!("Failed to export knowledge base: {}", e)})))?;

    Ok(Json(
        json!({"message": "Record types reloaded successfully"}),
    ))
}

use std::collections::HashMap;

use askama::Template;
use axum::{
    extract::{Path, State},
    routing::{delete, get, post},
    Form, Json, Router,
};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

use crate::{services::fact::FactTableData, AppState};

/// Returns the routes for getting and creating facts.
pub fn fact_routes() -> Router<AppState> {
    Router::new()
        .route("/:fact_type/facts", get(get_facts).post(create_fact))
        .route("/api/:fact_type/facts", post(create_fact_json))
        .route(
            "/:fact_type/facts/new",
            get(get_new_fact_form).delete(get_add_fact_button),
        )
        .route("/:fact_type/facts/:fact_id", delete(delete_fact))
}

#[derive(Template)]
#[template(path = "fact/facts.html")]
struct GetFactsTemplate {
    fact_type: String,
    fact_table_data: FactTableData,
}

async fn get_facts(
    State(state): State<AppState>,
    Path(fact_type): Path<String>,
) -> GetFactsTemplate {
    let fact_table_data = state
        .facts
        .get_facts(fact_type.clone())
        .await
        .unwrap_or_default();

    GetFactsTemplate {
        fact_type,
        fact_table_data,
    }
}

#[derive(Template)]
#[template(path = "fact/new-fact.html")]
struct GetNewFactFormTemplate {
    fact_type: String,
    fields: Vec<String>,
}

async fn get_new_fact_form(
    State(state): State<AppState>,
    Path(rt_name): Path<String>,
) -> GetNewFactFormTemplate {
    match state.lm.get_record_type(rt_name).await {
        Ok(rt) => GetNewFactFormTemplate {
            fact_type: rt.clone().display_name.clone(),
            fields: rt.clone().all_fields(),
        },
        Err(_) => GetNewFactFormTemplate {
            fact_type: "".to_string(),
            fields: vec![],
        },
    }
}

#[derive(Template)]
#[template(path = "fact/add-new-fact-button.html")]
struct GetAddFactButtonTemplate {
    fact_type: String,
}

async fn get_add_fact_button(Path(rt_name): Path<String>) -> GetAddFactButtonTemplate {
    GetAddFactButtonTemplate { fact_type: rt_name }
}

#[derive(Template)]
#[template(path = "fact/facts-table.html")]
struct FactsTableTemplate {
    fact_type: String,
    fact_table_data: FactTableData,
}

async fn create_fact(
    State(state): State<AppState>,
    Path(rt_name): Path<String>,
    Form(f): Form<HashMap<String, String>>,
) -> FactsTableTemplate {
    state
        .facts
        .add_fact(&rt_name, f.clone())
        .await
        .expect(&format!("Failed to add fact for {}, {:?}", rt_name, f));
    let fact_table_data = state
        .facts
        .get_facts(rt_name.clone())
        .await
        .unwrap_or_default();

    FactsTableTemplate {
        fact_type: rt_name,
        fact_table_data,
    }
}

#[derive(Debug, Deserialize, Serialize)]
struct CreateFacts {
    facts: Vec<Value>,
}

async fn create_fact_json(
    State(state): State<AppState>,
    Path(rt_name): Path<String>,
    Json(payload): Json<CreateFacts>,
) -> Json<Value> {
    let mut named_valuess: Vec<HashMap<String, String>> = Vec::new();

    for fact in &payload.facts {
        match fact.as_object() {
            Some(obj) => {
                let map = obj
                    .iter()
                    .map(|(k, v)| {
                        let val = match v {
                            Value::String(s) => s,
                            _ => return (k.clone(), "".to_string()),
                        };
                        (k.clone(), val.to_string())
                    })
                    .collect::<HashMap<String, String>>();

                named_valuess.push(map);
            }
            None => {
                return Json(json!({
                    "error": "One or more facts is not a JSON object"
                }))
            }
        }
    }

    state
        .facts
        .add_facts(&rt_name, named_valuess.clone())
        .await
        .expect(&format!(
            "Failed to add facts for {}, {:?}",
            rt_name, named_valuess
        ));

    Json(
        serde_json::to_value(payload)
            .expect("Failed to deserialise payload JSON in echo response."),
    )
}

async fn delete_fact(
    State(state): State<AppState>,
    Path((rt_name, fact_id)): Path<(String, String)>,
) -> FactsTableTemplate {
    FactsTableTemplate {
        fact_type: rt_name.clone(),
        fact_table_data: state
            .facts
            .delete_fact(&rt_name, &fact_id)
            .await
            .unwrap_or_default(),
    }
}

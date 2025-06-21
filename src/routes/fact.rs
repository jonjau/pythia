use std::collections::HashMap;

use askama::Template;
use axum::{
    extract::{Path, State},
    routing::{get, post},
    Form, Json, Router,
};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

use crate::AppState;

pub fn fact_routes() -> Router<AppState> {
    Router::new()
        .route("/:fact_type/facts", get(get_facts).post(create_fact))
        .route("/api/:fact_type/facts", post(create_fact_json))
        .route(
            "/:fact_type/facts/new",
            get(get_new_fact_form).delete(get_add_fact_button),
        )
}

#[derive(Template)]
#[template(path = "facts.html")]
struct GetFactsTemplate {
    fact_type: String,
    facts: Vec<String>,
}

async fn get_facts(State(state): State<AppState>, Path(rt_name): Path<String>) -> GetFactsTemplate {
    let facts = state.facts.get_facts(rt_name.clone()).await.unwrap();

    GetFactsTemplate {
        fact_type: rt_name,
        facts,
    }
}

#[derive(Template)]
#[template(path = "new-fact.html")]
struct GetNewFactFormTemplate {
    fact_type: String,
    fields: Vec<String>,
}

async fn get_new_fact_form(
    State(state): State<AppState>,
    Path(rt_name): Path<String>,
) -> GetNewFactFormTemplate {
    let rt = state.lm.get_record_type(rt_name).await.unwrap();

    GetNewFactFormTemplate {
        fact_type: rt.clone().display_name.clone(),
        fields: rt.clone().all_fields(),
    }
}

#[derive(Template)]
#[template(path = "add-new-fact-button.html")]
struct GetAddFactButtonTemplate {
    fact_type: String,
}

async fn get_add_fact_button(Path(rt_name): Path<String>) -> GetAddFactButtonTemplate {
    GetAddFactButtonTemplate { fact_type: rt_name }
}

#[derive(Template)]
#[template(path = "facts-table.html")]
struct FactsTableTemplate {
    facts: Vec<String>,
}

async fn create_fact(
    State(state): State<AppState>,
    Path(rt_name): Path<String>,
    Form(f): Form<HashMap<String, String>>,
) -> FactsTableTemplate {
    dbg!(&f);

    state.facts.add_fact(&rt_name, f).await.unwrap();
    let facts = state.facts.get_facts(rt_name).await.unwrap();

    FactsTableTemplate { facts }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct CreateFacts {
    pub facts: Vec<Value>,
}

async fn create_fact_json(
    State(state): State<AppState>,
    Path(rt_name): Path<String>,
    Json(payload): Json<CreateFacts>,
) -> Json<Value> {
    let mut named_valuess: Vec<HashMap<String, String>> = Vec::new();

    for fact in &payload.facts {
        match fact.as_object() {
            // TODO JCJ: accept other serde value types!
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

    state.facts.add_facts(&rt_name, named_valuess).await.unwrap();

    Json(serde_json::to_value(payload).unwrap())
}

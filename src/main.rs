use std::{collections::HashMap, error::Error};

use askama::Template;
use axum::{
    extract::{Path, Query, State},
    response::Redirect,
    routing::{get, post},
    Json, Router,
};

use serde::{Deserialize, Serialize};

mod models;
mod services;
mod utils;

use models::goal::GoalTerm;
use services::{
    fact::FactService,
    state_change::{ChangePath, StateChangeService},
};

#[derive(Clone)]
struct AppState {
    facts: FactService,
    state_changes: StateChangeService,
}

#[tokio::main]
async fn main() {
    let db = include_str!("../data/db.pl");

    let facts = FactService::new(db);

    // TODO: graceful shutdown of actor
    let state = AppState {
        facts: facts.clone(),
        state_changes: StateChangeService::new(facts.clone()),
    };

    let r = Router::new()
        .route("/", get(|| async { Redirect::permanent("/facts") }))
        .route("/facts", get(get_facts))
        .route("/facts", post(create_fact))
        .route("/state-change-paths", get(get_state_change_paths))
        .route(
            "/states/:state_id/:fact_type/:field_name/specified",
            post(set_field_to_specified).delete(set_field_to_unspecified),
        )
        .with_state(state);

    // run our app with hyper, listening globally on port 3000
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    axum::serve(listener, r).await.unwrap();
}

#[derive(Template)]
#[template(path = "index.html", ext = "html")]
struct FactsPage {
    fact_type: String,
    facts: Vec<String>,
    state_fields: Vec<String>,
}

#[derive(Deserialize)]
struct Params {
    fact_type: Option<String>,
}

async fn get_facts(query: Query<Params>, State(state): State<AppState>) -> FactsPage {
    match &query.fact_type {
        None => FactsPage {
            fact_type: "".to_string(),
            facts: vec![],
            state_fields: vec![],
        },
        Some(ft) => {
            let result = state.facts.get_all_facts(ft.to_string()).await.unwrap();
            let fs = result.iter().map(|f| f.to_string()).collect::<Vec<_>>();

            FactsPage {
                fact_type: result[0].type_name(),
                facts: fs,
                state_fields: result[0].data_fields(),
            }
        }
    }
}

#[derive(Template)]
#[template(path = "change-path-table.html", ext = "html")]
struct GetStateChangePathsResponse {
    error_message: Option<String>,
    paths: Vec<ChangePath>,
}

async fn get_state_change_paths(
    State(app_state): State<AppState>,
    Query(q): Query<HashMap<String, String>>,
) -> GetStateChangePathsResponse {
    let get_named_values = |prefix: &str| {
        q.iter()
            .filter_map(|(field, value)| {
                field.starts_with(prefix).then_some((
                    field[prefix.len()..].to_string(),
                    GoalTerm::String(value.to_string()),
                ))
            })
            .collect::<HashMap<_, _>>()
    };
    let named_values0 = get_named_values("start.");
    let named_values1 = get_named_values("end.");

    let parse = |q: &HashMap<String, String>| -> Result<(i32, String), Box<dyn Error>> {
        let n_steps = q
            .get("num-steps")
            .ok_or("num-steps not found")?
            .parse::<i32>().map_err(|_| "Invalid num-steps argument")?;
        let fact_type = q
            .get("_fact-type")
            .ok_or("_fact-type not found")?
            .to_string();
        Ok((n_steps, fact_type))
    };

    let (n_steps, fact_type) = match parse(&q) {
        Ok(i) => i,
        Err(e) => {
            return GetStateChangePathsResponse {
                error_message: Some(e.to_string()),
                paths: vec![],
            }
        }
    };

    match app_state
        .state_changes
        .get_paths(&fact_type, named_values0, named_values1, n_steps)
        .await
    {
        Ok(paths) => GetStateChangePathsResponse {
            error_message: None,
            paths,
        },
        Err(e) => GetStateChangePathsResponse {
            error_message: Some(e.to_string()),
            paths: vec![],
        },
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
struct CreateFact {
    typename: String,
    values: Vec<String>,
}

// #[debug_handler]
async fn create_fact(State(_state): State<AppState>, Json(_cf): Json<CreateFact>) -> String {
    "".to_string()
}

#[derive(Template)]
#[template(path = "set-field-to-specified.html", ext = "html")]
struct SetFieldToSpecifiedTemplate {
    state_id: String,
    fact_type: String,
    field: String,
}

async fn set_field_to_specified(
    Path((state_id, fact_type, field_name)): Path<(String, String, String)>,
) -> SetFieldToSpecifiedTemplate {
    SetFieldToSpecifiedTemplate {
        state_id,
        fact_type,
        field: field_name,
    }
}

#[derive(Template)]
#[template(path = "set-field-to-unspecified.html", ext = "html")]
struct SetFieldToUnspecifiedTemplate {
    state_id: String,
    fact_type: String,
    field: String,
}

async fn set_field_to_unspecified(
    Path((state_id, fact_type, field_name)): Path<(String, String, String)>,
) -> SetFieldToUnspecifiedTemplate {
    SetFieldToUnspecifiedTemplate {
        state_id,
        fact_type,
        field: field_name,
    }
}

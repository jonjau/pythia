use std::{collections::HashMap, error::Error};

use askama::Template;
use askama_axum::IntoResponse;
use axum::{
    extract::{Path, Query, State},
    http::HeaderMap,
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
        .route("/state-changes/:state_record_type", get(get_state_changes))
        .route("/facts", post(create_fact))
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
#[template(path = "layout.html", ext = "html")]
struct StateChangesPageInput {
    fact_type: String,
    start_state_values: Vec<(String, String)>,
    end_state_values: Vec<(String, String)>,
    num_steps: i32,
}

#[derive(Template)]
#[template(path = "state-changes-output.html", ext = "html")]
struct StateChangesPageOutput {
    error_message: Option<String>,
    paths: Vec<ChangePath>,
}

enum StateChangesPage {
    Input(StateChangesPageInput),
    Output(StateChangesPageOutput),
}

impl IntoResponse for StateChangesPage {
    fn into_response(self) -> axum::response::Response {
        match self {
            StateChangesPage::Input(t) => t.into_response(),
            StateChangesPage::Output(t) => t.into_response(),
        }
    }
}

async fn get_state_changes(
    State(app_state): State<AppState>,
    headers: HeaderMap,
    Path(state_rt): Path<String>,
    Query(q): Query<HashMap<String, String>>,
) -> StateChangesPage {
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
    let n_steps = q.get("num-steps");

    let rt = app_state.facts.get_record_type(&state_rt).await.unwrap();
    let state_fields = rt.data_fields.clone();
    let start_state_values = state_fields
        .iter()
        .map(|field_name| {
            let value = named_values0
                .get(field_name)
                .map(|v| v.to_string().trim_matches('"').to_string())
                .unwrap_or("".to_string());
            (field_name.clone(), value)
        })
        .collect::<Vec<_>>();
    let end_state_values = state_fields
        .iter()
        .map(|field_name| {
            let value = named_values1
                .get(field_name)
                .map(|v| v.to_string().trim_matches('"').to_string())
                .unwrap_or("".to_string());
            (field_name.clone(), value)
        })
        .collect::<Vec<_>>();

    if !headers.contains_key("HX-Request") {
        return StateChangesPage::Input(StateChangesPageInput {
            fact_type: rt.name.clone(),
            start_state_values,
            end_state_values,
            num_steps: n_steps.map(|s| s.parse::<i32>().unwrap_or(0)).unwrap_or(0),
        });
    } else {
        let parse = |q: &HashMap<String, String>| -> Result<i32, Box<dyn Error>> {
            let n_steps = n_steps
                .ok_or("num-steps not found")?
                .parse::<i32>()
                .map_err(|_| "Invalid num-steps argument")?;
            Ok(n_steps)
        };

        let n_steps = match parse(&q) {
            Ok(i) => i,
            Err(e) => {
                return StateChangesPage::Output(StateChangesPageOutput {
                    error_message: Some(e.to_string()),
                    paths: vec![],
                });
            }
        };

        match app_state
            .state_changes
            .get_paths(&state_rt, named_values0, named_values1, n_steps)
            .await
        {
            Ok(paths) => StateChangesPage::Output(StateChangesPageOutput {
                error_message: None,
                paths,
            }),
            Err(e) => StateChangesPage::Output(StateChangesPageOutput {
                error_message: Some(e.to_string()),
                paths: vec![],
            }),
        }
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
    value: String,
}

async fn set_field_to_specified(
    Path((state_id, fact_type, field_name)): Path<(String, String, String)>,
) -> SetFieldToSpecifiedTemplate {
    SetFieldToSpecifiedTemplate {
        state_id,
        fact_type,
        field: field_name,
        value: "".to_string(),
    }
}

#[derive(Template)]
#[template(path = "set-field-to-unspecified.html", ext = "html")]
struct SetFieldToUnspecifiedTemplate {
    state_id: String,
    fact_type: String,
    field: String,
    value: String,
}

async fn set_field_to_unspecified(
    Path((state_id, fact_type, field_name)): Path<(String, String, String)>,
) -> SetFieldToUnspecifiedTemplate {
    SetFieldToUnspecifiedTemplate {
        state_id,
        fact_type,
        field: field_name,
        value: "".to_string(),
    }
}

use std::collections::HashMap;

use askama::Template;
use askama_axum::IntoResponse;
use axum::{
    extract::{Path, Query, State},
    http::HeaderMap,
    routing::{get, post},
    Json, Router,
};

use serde::{Deserialize, Serialize};

mod models;
mod services;
mod utils;

use services::{
    fact::FactService, logic_machine::LogicMachineService, state_change::{ChangePath, StateChangeService}
};

#[derive(Clone)]
struct AppState {
    lm: LogicMachineService,
    facts: FactService,
    state_changes: StateChangeService,
}

#[tokio::main]
async fn main() {
    let db = include_str!("../data/db.pl");
    let lm = LogicMachineService::new(db, "data/types.json");

    // TODO: graceful shutdown of actor
    let state = AppState {
        lm: lm.clone(),
        facts: FactService::new(lm.clone()),
        state_changes: StateChangeService::new(lm.clone()),
    };

    let r = Router::new()
        .route("/", get(get_inquiries))
        .route("/:state_record_type/state-changes", get(get_state_changes))
        .route("/facts", post(create_fact))
        .route("/:fact_type/facts", get(get_facts))
        .route("/:fact_type/facts/new", get(get_new_fact_form).delete(get_add_fact_button))
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
struct GetInquiriesTemplate {
    record_types: Vec<String>,
}

async fn get_inquiries(State(app_state): State<AppState>) -> GetInquiriesTemplate {
    let rts = app_state.lm.get_all_record_types().await.unwrap();

    GetInquiriesTemplate {
        record_types: rts.iter().map(|rt| rt.name.clone()).collect(),
    }
}

#[derive(Template)]
#[template(path = "state-changes.html", ext = "html")]
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
    let get_values_for_key_with_prefix = |prefix: &str| {
        q.iter()
            .filter_map(|(field, value)| {
                field
                    .starts_with(prefix)
                    .then_some((field[prefix.len()..].to_string(), value.to_string()))
            })
            .collect::<HashMap<_, _>>()
    };

    let named_values0 = get_values_for_key_with_prefix("start.");
    let named_values1 = get_values_for_key_with_prefix("end.");
    let num_steps = q.get("num-steps");

    if !headers.contains_key("HX-Request") {
        StateChangesPage::Input(
            match app_state
                .state_changes
                .populate_all_state_values(&state_rt, named_values0, named_values1)
                .await
            {
                Ok(values) => StateChangesPageInput {
                    fact_type: state_rt,
                    start_state_values: values.start_state_values,
                    end_state_values: values.end_state_values,
                    num_steps: num_steps
                        .map(|s| s.parse::<i32>().unwrap_or(0))
                        .unwrap_or(0),
                },
                Err(_) => StateChangesPageInput {
                    fact_type: "".to_string(),
                    start_state_values: vec![],
                    end_state_values: vec![],
                    num_steps: 0,
                },
            },
        )
    } else {
        StateChangesPage::Output(
            match app_state
                .state_changes
                .find_paths(&state_rt, named_values0, named_values1, num_steps)
                .await
            {
                Ok(paths) => StateChangesPageOutput {
                    error_message: None,
                    paths,
                },
                Err(e) => StateChangesPageOutput {
                    error_message: Some(format!("Failed to get paths: {}", e)),
                    paths: vec![],
                },
            },
        )
    }
}

#[derive(Template)]
#[template(path = "facts.html", ext = "html")]
struct GetFactsTemplate {
    fact_type: String,
    facts: Vec<String>,
}

async fn get_facts(State(state): State<AppState>, Path(rt_name): Path<String>) -> GetFactsTemplate {
    let facts = state.facts.get_facts(rt_name.clone()).await.unwrap();

    GetFactsTemplate {
        fact_type: rt_name,
        facts
    }
}

#[derive(Template)]
#[template(path = "new-fact.html", ext = "html")]
struct GetNewFactFormTemplate {
    fact_type: String,
    data_fields: Vec<String>
}

async fn get_new_fact_form(State(state): State<AppState>, Path(rt_name): Path<String>) -> GetNewFactFormTemplate {
    let rt = (*state.lm.get_record_type(rt_name).await.unwrap()).clone();
    
    GetNewFactFormTemplate {
        fact_type: rt.display_name,
        data_fields: rt.data_fields
    }
}


#[derive(Template)]
#[template(path = "add-new-fact-button.html", ext = "html")]
struct GetAddFactButtonTemplate {
    fact_type: String
}

async fn get_add_fact_button(Path(rt_name): Path<String>) -> GetAddFactButtonTemplate {
    GetAddFactButtonTemplate {
        fact_type: rt_name
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
struct CreateFact {
    typename: String,
    values: Vec<String>,
}

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

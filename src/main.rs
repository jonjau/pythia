use std::{collections::HashMap, fs, path};

use askama::Template;
use askama_axum::IntoResponse;
use axum::{
    extract::{Path, Query, State},
    http::HeaderMap,
    routing::{get, post},
    Form, Json, Router,
};

use serde::{Deserialize, Serialize};

mod models;
mod services;
mod utils;

use serde_json::{json, Value};
use services::{
    fact::FactService,
    logic_machine::LogicMachineService,
    state_change::{ChangePath, StateChangeService},
};

#[derive(Clone)]
struct AppState {
    lm: LogicMachineService,
    facts: FactService,
    state_changes: StateChangeService,
}

#[tokio::main]
async fn main() {
    let p = generate_main_prolog_program();
    fs::write("data/internal/pythia.pl", p).unwrap();

    let data =
        std::fs::read_to_string("data/internal/pythia.pl").expect("Failed to read pythia.pl");
    let lm = LogicMachineService::new(&data, "data/types.json");

    // TODO: graceful shutdown of actor
    let state = AppState {
        lm: lm.clone(),
        facts: FactService::new(lm.clone()),
        state_changes: StateChangeService::new(lm.clone()),
    };

    let r = Router::new()
        .route("/", get(get_inquiries))
        .route("/:state_record_type/state-changes", get(get_state_changes))
        .route("/:fact_type/facts", get(get_facts).post(create_fact))
        .route("/api/:fact_type/facts", post(create_fact_json))
        .route(
            "/:fact_type/facts/new",
            get(get_new_fact_form).delete(get_add_fact_button),
        )
        .route(
            "/states/:state_id/:fact_type/:field_name/specified",
            post(set_field_to_specified).delete(set_field_to_unspecified),
        )
        .with_state(state);

    // run our app with hyper, listening globally on port 3000
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    axum::serve(listener, r).await.unwrap();
}

struct RecordType {
    name: String,
    id_fields: String,
    data_fields: String,
    metadata_fields: String
}

#[derive(Template)]
#[template(path = "pythia.pl")]
struct PythiaTemplate {
    import_paths: Vec<String>,
    record_types: Vec<RecordType>,
}

fn generate_main_prolog_program() -> String {
    let json_data =
        fs::read_to_string(path::Path::new("data/types.json")).expect("Failed to read JSON file");
    let objects: Vec<serde_json::Value> =
        serde_json::from_str(&json_data).expect("Invalid JSON format");

    let import_paths = objects
        .iter()
        .filter_map(|o| {
            o.get("name")
                .and_then(|v| v.as_str())
                .and_then(|name| Some(format!("'./data/{}.pl'", name)))
        })
        .collect::<Vec<_>>();

    let record_types = objects
        .iter()
        .map(|o| {
            let name = o.get("name").and_then(|v| v.as_str()).map(|s| s.to_owned());

            let id_fields = o
                .get("id_fields")
                .and_then(|vs| vs.as_array())
                .and_then(|vs| {
                    Some(
                        vs.iter()
                            .filter_map(|v| v.as_str())
                            .map(|s| s.to_owned())
                            .collect::<Vec<_>>(),
                    )
                });

            let data_fields = o
                .get("data_fields")
                .and_then(|vs| vs.as_array())
                .and_then(|vs| {
                    Some(
                        vs.iter()
                            .filter_map(|v| v.as_str())
                            .map(|s| s.to_owned())
                            .collect::<Vec<_>>(),
                    )
                });

            let metadata_fields = o
                .get("metadata_fields")
                .and_then(|vs| vs.as_array())
                .and_then(|vs| {
                    Some(
                        vs.iter()
                            .filter_map(|v| v.as_str())
                            .map(|s| s.to_owned())
                            .collect::<Vec<_>>(),
                    )
                });

            if let (Some(name), Some(id_fields), Some(data_fields), Some(metadata_fields)) =
                (name, id_fields, data_fields, metadata_fields)
            {
                if id_fields.is_empty() || metadata_fields.is_empty() {
                    None
                } else {
                    Some(RecordType {
                        name,
                        id_fields: id_fields.join(", "),
                        data_fields: data_fields.join(", "),
                        metadata_fields: metadata_fields.join(", "),
                    })
                }
            } else {
                None
            }
        })
        .flatten()
        .collect::<Vec<_>>();

    PythiaTemplate { import_paths, record_types }.render().unwrap()
}

#[derive(Template)]
#[template(path = "index.html")]
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
#[template(path = "state-changes.html")]
struct StateChangesPageInput {
    fact_type: String,
    start_state_values: Vec<(String, String)>,
    end_state_values: Vec<(String, String)>,
    num_steps: i32,
}

#[derive(Template)]
#[template(path = "state-changes-output.html")]
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

    let facts = state.facts.add_fact(&rt_name, f).await.unwrap();

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

    for named_values in named_valuess {
        state.facts.add_fact(&rt_name, named_values).await.unwrap();
    }

    Json(serde_json::to_value(payload).unwrap())
}

#[derive(Template)]
#[template(path = "set-field-to-specified.html")]
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
#[template(path = "set-field-to-unspecified.html")]
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

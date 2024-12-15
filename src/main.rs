use std::{
    collections::{BTreeMap, HashMap},
    sync::Arc,
};

use askama::Template;
use axum::{
    extract::{Path, Query, State}, response::Redirect, routing::{delete, get, post}, Form, Json, Router
};

use serde::{Deserialize, Serialize};

mod models;
mod services;

use services::fact::FactService;

#[derive(Clone)]
struct AppState {
    facts: FactService
}

#[tokio::main]
async fn main() {
    let db = include_str!("../data/db.pl");

    // TODO: graceful shutdown of actor
    let state = AppState {
        facts: FactService::new(db)
    };

    let r = Router::new()
        .route("/", get(|| async { Redirect::permanent("/facts") }))
        .route("/facts", get(get_facts))
        .route("/facts", post(create_fact))
        .route("/all-state-changes", get(get_all_state_changes))
        .route("/state-changes", get(get_state_changes))
        .route("/start-state/:fact_type/:field_name/specified", post(set_to_specified))
        .route("/start-state/:fact_type/:field_name/specified", delete(set_to_unspecified))
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
    start_state_fields: Vec<String>,
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
            start_state_fields: vec![],
        },
        Some(ft) => {
            let result = state.facts.get_all_facts(ft.to_string()).await.unwrap();
            let fs = result.iter().map(|f| f.assertion_str()).collect::<Vec<_>>();

            FactsPage {
                fact_type: result[0].type_name(),
                facts: fs,
                start_state_fields: result[0].data_fields(),
            }
        }
    }
}

#[derive(Template)]
#[template(path = "fact-table.html", ext = "html")]
struct FactsTable {
    facts: Vec<String>,
}

async fn get_all_state_changes(State(state): State<AppState>) -> FactsTable {
    let result = state
        .facts
        .get_all_facts("step_change".to_string())
        .await
        .unwrap();
    let fs = result.iter().map(|f| f.assertion_str()).collect::<Vec<_>>();

    FactsTable { facts: fs }
}

async fn get_state_changes(
    State(app_state): State<AppState>,
    Query(states): Query<HashMap<String, String>>,
) -> FactsTable {
    let fact_type = states.get("_fact_type").unwrap();
    let subgoal_rt = app_state.facts.get_record_type(fact_type.to_string()).await.unwrap();

    let named_values = states
        .iter()
        .filter_map(|(field, value)| {
            if field.starts_with("0.") || field.starts_with("1.") {
                Some((field[2..].to_string(), Some(value.to_string())))
            } else {
                None
            }
        })
        .collect::<HashMap<_, _>>();
    dbg!(&named_values);

    let subgoal = format!("[{}]", subgoal_rt.to_goal(&named_values.iter().map(|(k, v)| (k.as_str(), v.as_deref())).collect()).unwrap().to_unescaped_query());
    dbg!(&subgoal);

    let rt = app_state
        .facts
        .get_record_type("step_change".to_string())
        .await
        .unwrap();

    let m = HashMap::from([("Vals1", Some(subgoal.as_str()))]);
    let goal = rt.to_goal(&m).unwrap();

    dbg!(&goal);

    let result = app_state
        .facts
        .get_facts("step_change".to_string(), goal.to_values())
        .await
        .unwrap();

    let fs = result.iter().map(|f| f.assertion_str()).collect::<Vec<_>>();

    FactsTable { facts: fs }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
struct CreateFact {
    typename: String,
    values: Vec<String>,
}

use models::fact::RecordType;

// #[debug_handler]
async fn create_fact(State(state): State<AppState>, Json(cf): Json<CreateFact>) -> String {
    dbg!(cf.clone());

    // let rt = Arc::new(RecordType::new("edge", &["X", "Y"]).unwrap());
    // let fact = rt
    //     .to_fact(&HashMap::from([("X", "3"), ("Y", "5")]))
    //     .unwrap();

    let fields_temp = (0..cf.values.len())
        .map(|i| format!("X{}", i))
        .collect::<Vec<_>>();
    let fields = fields_temp.iter().map(String::as_ref).collect::<Vec<_>>();
    let rt0 = Arc::new(RecordType::new_without_id_fields(&cf.typename, &fields).unwrap());

    let mapped_values = fields
        .iter()
        .zip(cf.values.iter())
        .map(|(&field, value)| (field, value.as_str()))
        .collect::<HashMap<_, _>>();

    let fact = rt0.to_fact(&mapped_values).unwrap();

    // let res = state.facts.add_fact(fact).await.unwrap();
    // res.to_string()

    let res = state.facts.add_fact(fact).await.unwrap();
    res.len().to_string();

    let result = state.facts.get_all_facts(cf.typename).await.unwrap();
    result
        .iter()
        .map(|f| f.to_string())
        .collect::<Vec<_>>()
        .join(",\n")
    // String::new()
}




#[derive(Template)]
#[template(path = "set-field-to-specified.html", ext = "html")]
struct SetFieldToSpecifiedTemplate {
    fact_type: String,
    field: String
}

async fn set_to_specified(Path((fact_type, field_name)): Path<(String, String)>) -> SetFieldToSpecifiedTemplate {
    SetFieldToSpecifiedTemplate {
        fact_type,
        field: field_name
    }
}

#[derive(Template)]
#[template(path = "set-field-to-unspecified.html", ext = "html")]
struct SetFieldToUnspecifiedTemplate {
    fact_type: String,
    field: String
}

async fn set_to_unspecified(Path((fact_type, field_name)): Path<(String, String)>) -> SetFieldToUnspecifiedTemplate {
    SetFieldToUnspecifiedTemplate {
        fact_type,
        field: field_name
    }
}

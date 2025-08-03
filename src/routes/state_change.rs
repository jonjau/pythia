use std::collections::HashMap;

use askama::Template;
use askama_axum::IntoResponse;
use axum::{
    extract::{Path, Query},
    http::HeaderMap,
    routing::{get, post},
    Router,
};

use crate::{middleware::session::UserToken, services::state_change::ChangePath, AppState};

/// Returns the routes for calculating state change paths.
pub fn state_change_routes() -> Router {
    Router::new()
        .route("/:state_record_type/state-changes", get(get_state_changes))
        .route(
            "/states/:state_id/:fact_type/:field_name/specified",
            post(set_field_to_specified).delete(set_field_to_unspecified),
        )
}

#[derive(Template)]
#[template(path = "state-change/state-changes.html")]
struct StateChangesPageInput {
    user_token: String,
    fact_type: String,
    start_state_values: Vec<(String, String)>,
    end_state_values: Vec<(String, String)>,
    num_steps: i32,
}

#[derive(Template)]
#[template(path = "state-change/state-changes-output.html")]
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
    app_state: AppState,
    headers: HeaderMap,
    Path(state_rt): Path<String>,
    Query(q): Query<HashMap<String, String>>,
    UserToken(user_token): UserToken,
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

    // If request is coming from HTMX, just return the input section, prefilled, otherwise it's a request to calculate paths
    if !headers.contains_key("HX-Request") {
        StateChangesPage::Input(
            match app_state
                .state_changes
                .populate_all_state_values(&state_rt, named_values0, named_values1)
                .await
            {
                Ok(values) => StateChangesPageInput {
                    user_token,
                    fact_type: state_rt,
                    start_state_values: values.start_state_values,
                    end_state_values: values.end_state_values,
                    num_steps: num_steps
                        .map(|s| s.parse::<i32>().unwrap_or(0))
                        .unwrap_or(0),
                },
                Err(_) => StateChangesPageInput {
                    user_token,
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
#[template(path = "state-change/set-field-to-specified.html")]
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
#[template(path = "state-change/set-field-to-unspecified.html")]
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

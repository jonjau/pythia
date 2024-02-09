use askama::Template;
use axum::{
    extract::{Query, State},
    response::Redirect,
    routing::get,
    Router,
};
use serde::Deserialize;
use std::collections::HashSet;

mod models;
use models::contact::{Contact, ContactModel};

struct ContactController {
    model: ContactModel,
}

impl ContactController {
    fn new(model: ContactModel) -> Self {
        ContactController { model }
    }
}

#[derive(Clone)]
struct AppState {
    contact_model: ContactModel,
}

#[tokio::main]
async fn main() {
    let state = AppState {
        contact_model: ContactModel::new(),
    };

    let r = Router::new()
        .route("/", get(|| async { Redirect::permanent("/contacts") }))
        .route("/contacts", get(get_contacts).with_state(state));

    // run our app with hyper, listening globally on port 3000
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    axum::serve(listener, r).await.unwrap();
}

#[derive(Template)]
#[template(path = "index.html", ext = "html")]
struct ContactsPage {
    contacts: HashSet<Contact>,
}

#[derive(Deserialize)]
struct Params {
    q: Option<String>,
}

async fn get_contacts(query: Query<Params>, State(state): State<AppState>) -> ContactsPage {
    match &query.q {
        None => ContactsPage {
            contacts: state.contact_model.find_all(),
        },
        Some(q) => ContactsPage {
            contacts: state.contact_model.find(q.to_string()),
        },
    }
}

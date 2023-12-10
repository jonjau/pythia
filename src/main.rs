use axum::{routing::get, Router, response::Redirect};
use askama::Template;

#[tokio::main]
async fn main() {
    let r = Router::new()
        .route("/", get(|| async { Redirect::permanent("/contacts") }))
        .route("/contacts", get(get_contacts));

    // run our app with hyper, listening globally on port 3000
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    axum::serve(listener, r).await.unwrap();
}

#[derive(Template)]
#[template(path = "hello.html")]
struct HelloTemplate<'a> {
    name: &'a str,
}

async fn get_contacts() -> HelloTemplate<'static>{
    HelloTemplate { name: "world" }
}
use askama::Template;
use axum::{routing::get, Router};

/// Returns the routes for the 'how to use' page.
pub fn help_routes() -> Router {
    Router::new().route("/how-to-use", get(get_how_to_use_page))
}

#[derive(Template)]
#[template(path = "how-to-use.html")]
struct HowToUseTemplate {
    page: String,
    user_token: String,
}

async fn get_how_to_use_page() -> HowToUseTemplate {
    HowToUseTemplate {
        page: "how-to-use".to_owned(),
        user_token: "".to_owned(),
    }
}

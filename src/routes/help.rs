use askama::Template;
use axum::{routing::get, Router};

/// Returns the routes for the 'how to use' page.
pub fn help_routes() -> Router {
    Router::new().route("/how-to-use", get(get_inquiries))
}

#[derive(Template)]
#[template(path = "how_to_use.html")]
struct HowToUseTemplate {
    page: String,
    user_token: String,
}

async fn get_inquiries() -> HowToUseTemplate {
    HowToUseTemplate {
        page: "how-to-use".to_owned(),
        user_token: "".to_owned()
    }
}

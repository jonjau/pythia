use std::{collections::HashMap, sync::Arc};

use tokio::sync::RwLock;

use crate::{
    services::{
        db::DbService, fact::FactService, logic_machine::LogicMachineService,
        state_change::StateChangeService,
    },
    AppState,
};

impl AppState {
    pub async fn new(user_token: String) -> Self {
        // Generates knowledge base from persistence layer (i.e. DB) at start-up.
        // let db = DbService::new("admin".to_owned()).await;
        let mut db = DbService::new_local(user_token).await;
        db.create_table_if_not_exists("pythia", "pk", "sk")
            .await
            .expect("Failed to create essential table");
        db.update_knowledge_base()
            .await
            .expect("Failed to update knowledge base");

        // Load knowledge base in Prolog
        let lm = LogicMachineService::new(db.clone())
            .await
            .expect("Failed to start LogicMachine service");

        // Initialise Pythia application state and services
        AppState {
            db: db.clone(),
            lm: lm.clone(),
            facts: FactService::new(lm.clone(), db.clone()),
            state_changes: StateChangeService::new(lm.clone()),
        }
    }
}

#[derive(Clone)]
pub struct SessionService {
    session_state_cache: Arc<RwLock<HashMap<String, AppState>>>,
}
impl SessionService {
    pub fn new() -> Self {
        SessionService {
            session_state_cache: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    pub async fn start_session(&self, user_token: String) {
        if let None = self.get_session_state(&user_token).await {
            let app_state = AppState::new(user_token.clone()).await;
            self.session_state_cache
                .write()
                .await
                .insert(user_token.clone(), app_state);
        }
    }

    pub async fn get_session_state(&self, user_token: &str) -> Option<AppState> {
        self.session_state_cache
            .read()
            .await
            .get(user_token)
            .cloned()
    }
}

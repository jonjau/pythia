use std::sync::Arc;

use quick_cache::sync::Cache;

use crate::services::{
    db::DbService, fact::FactService, logic_machine::LogicMachineService,
    state_change::StateChangeService,
};

/// Shared application state used by all handlers and services.
///
/// This includes:
/// - `LogicMachineService`: The logic engine running Prolog programs.
/// - `FactService`: Handles operations related to facts.
/// - `StateChangeService`: Handles state change operations.
#[derive(Clone)]
pub struct AppState {
    pub db: DbService,
    pub lm: LogicMachineService,
    pub facts: FactService,
    pub state_changes: StateChangeService,
}

impl AppState {
    pub async fn new(db: DbService) -> Self {
        // Generates knowledge base from persistence layer (i.e. DB) at start-up.
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

/// Errors that can occur while using the LogicMachineServiceError.
#[derive(thiserror::Error, Debug)]
pub enum SessionServiceError {
    #[error("Failed to start session: {0}")]
    StartSessionError(String),

    #[error("Failed to check if session exists: {0}")]
    CheckSessionError(String),
}

#[derive(Clone)]
pub struct SessionService {
    session_state_cache: Arc<Cache<String, AppState>>,
    db: DbService,
}

impl SessionService {
    pub fn new(db: DbService) -> Self {
        SessionService {
            session_state_cache: Arc::new(Cache::new(1000)),
            db,
        }
    }

    pub async fn start_session(&self, user_token: String) -> Result<(), SessionServiceError> {
        match self.get_session_state(&user_token).await? {
            None => {
                let mut user_db = self.db.clone();
                user_db.set_user(user_token.clone());

                let app_state = AppState::new(user_db).await;
                self.db
                    .add_user_token(user_token.clone())
                    .await
                    .map_err(|e| {
                        SessionServiceError::StartSessionError(format!(
                            "Failed to add user token: {}",
                            e
                        ))
                    })?;
                self.session_state_cache
                    .insert(user_token.clone(), app_state);

                Ok(())
            }
            _ => Ok(()),
        }
    }

    pub async fn get_session_state(
        &self,
        user_token: &str,
    ) -> Result<Option<AppState>, SessionServiceError> {
        let exists = self
            .db
            .user_token_exists(user_token.to_string())
            .await
            .map_err(|e| {
                SessionServiceError::CheckSessionError(format!(
                    "Failed to check user token existence: {}",
                    e
                ))
            })?;

        if exists {
            Ok(self.session_state_cache.get(user_token))
        } else {
            Ok(None)
        }
    }
}

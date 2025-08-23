use std::sync::Arc;

use crate::models::fact::Fact;
use crate::models::goal::Goal;
use crate::models::logic_machine::{LogicMachine, LogicMachineError, LogicMachineResult};
use crate::models::record_type::{RecordType, RecordTypeBuilder, RecordTypeError};
use crate::services::db::{DbService, DbServiceError};
use crate::utils::codegen;

use tokio::sync::{mpsc, oneshot};
use tokio::{runtime::Builder, task::LocalSet};

/// A high-level asynchronous interface for interacting with the logic machine.
///
/// It wraps an internal actor to perform fact and record type operations
/// off the main thread using message-passing and Tokio's runtime.
#[derive(Clone)]
pub struct LogicMachineService {
    lm_actor: ActorHandle,
    db: DbService,
}

/// Errors that can occur while using the LogicMachineServiceError.
#[derive(thiserror::Error, Debug)]
pub enum LogicMachineServiceError {
    #[error("Failed to open file: {0}")]
    CouldNotOpenFile(#[from] std::io::Error),
    #[error("Failed to parse JSON: {0}")]
    CouldNotParseJson(#[from] serde_json::Error),
    #[error("Failed to build record type: {0}")]
    CouldNotBuildRecordType(#[from] RecordTypeError),
    #[error("Failed to load state from DB: {0}")]
    DbServiceError(#[from] DbServiceError),
    #[error("Failed to load state from generated files: {0}")]
    CodeGenError(#[from] codegen::CodeGenError),
    #[error("Failed to reload LM state: {0}")]
    LogicMachineError(#[from] LogicMachineError),
}

impl LogicMachineService {
    /// Creates a new logic machine service loading facts from generated files.
    pub async fn new(db: DbService) -> Result<Self, LogicMachineServiceError> {
        let (program, record_types) = Self::load_lm_types_and_facts(&db).await?;

        Ok(LogicMachineService {
            lm_actor: ActorHandle::new(program, record_types),
            db,
        })
    }

    /// Reloads the logic machine's knowledge from the knowledge base stored in the database.
    pub async fn reload(&self) -> Result<(), LogicMachineServiceError> {
        let (program, record_types) = Self::load_lm_types_and_facts(&self.db).await?;

        Ok(self
            .lm_actor
            .send_query(ReloadQuery {
                program,
                record_types,
            })
            .await?)
    }

    /// Returns the main program and the record types to be used by the logic machine.
    async fn load_lm_types_and_facts(
        db: &DbService,
    ) -> Result<(String, Vec<RecordType>), LogicMachineServiceError> {
        db.update_knowledge_base().await?;
        let kb = db.get_knowledge_base().await?;
        let program = kb.get_pythia_program_with_facts_inline();
        let rts = kb
            .get_record_types()?
            .into_iter()
            .map(|rt| {
                RecordTypeBuilder::new(rt.name, rt.data_fields)
                    .id_fields(rt.id_fields)
                    .metadata_fields(rt.metadata_fields)
                    .build()
            })
            .collect::<Result<Vec<_>, RecordTypeError>>()?;

        Ok((program, rts))
    }

    /// Fetches a record type by its name.
    pub async fn get_record_type(
        &self,
        fact_type: impl Into<String>,
    ) -> Result<Arc<RecordType>, LogicMachineServiceError> {
        self.reload().await?;
        Ok(self
            .lm_actor
            .send_query(GetRecordTypeQuery {
                fact_type: fact_type.into(),
            })
            .await?)
    }

    /// Retrieves all record types known by the logic machine.
    pub async fn get_all_record_types(
        &self,
    ) -> Result<Vec<Arc<RecordType>>, LogicMachineServiceError> {
        self.reload().await?;
        Ok(self.lm_actor.send_query(GetAllRecordTypesQuery).await?)
    }

    /// Gets facts matching a goal for a specific target record type.
    pub async fn get_facts(
        &self,
        goal: Goal,
        target_rt: Arc<RecordType>,
    ) -> Result<Vec<Fact>, LogicMachineServiceError> {
        self.reload().await?;
        Ok(self
            .lm_actor
            .send_query(GetFactsQuery { goal, target_rt })
            .await?)
    }
}

struct ReloadQuery {
    program: String,
    record_types: Vec<RecordType>,
}

struct GetAllFactsQuery {
    fact_type: String,
}

struct GetFactsQuery {
    goal: Goal,
    target_rt: Arc<RecordType>,
}

struct GetRecordTypeQuery {
    fact_type: String,
}

struct GetAllRecordTypesQuery;

/// Wraps a query and a one-shot channel to receive the result.
struct Message<Query, Response> {
    query: Query,
    respond_to: oneshot::Sender<Response>,
}

/// Enum representing all supported actor messages.
enum ActorMessage {
    Reload(ReloadMessage),
    GetAllFacts(GetAllFactsMessage),
    GetFacts(GetFactsMessage),
    GetRecordType(GetRecordTypeMessage),
    GetAllRecordTypes(GetAllRecordTypesMessage),
}

type ReloadMessage = Message<ReloadQuery, LogicMachineResult<()>>;
type GetAllFactsMessage = Message<GetAllFactsQuery, LogicMachineResult<Vec<Fact>>>;
type GetFactsMessage = Message<GetFactsQuery, LogicMachineResult<Vec<Fact>>>;
type GetRecordTypeMessage = Message<GetRecordTypeQuery, LogicMachineResult<Arc<RecordType>>>;
type GetAllRecordTypesMessage =
    Message<GetAllRecordTypesQuery, LogicMachineResult<Vec<Arc<RecordType>>>>;

impl From<ReloadMessage> for ActorMessage {
    fn from(msg: ReloadMessage) -> Self {
        ActorMessage::Reload(msg)
    }
}

impl From<GetAllFactsMessage> for ActorMessage {
    fn from(msg: GetAllFactsMessage) -> Self {
        ActorMessage::GetAllFacts(msg)
    }
}

impl From<GetFactsMessage> for ActorMessage {
    fn from(msg: GetFactsMessage) -> Self {
        ActorMessage::GetFacts(msg)
    }
}

impl From<GetRecordTypeMessage> for ActorMessage {
    fn from(msg: GetRecordTypeMessage) -> Self {
        ActorMessage::GetRecordType(msg)
    }
}

impl From<GetAllRecordTypesMessage> for ActorMessage {
    fn from(msg: GetAllRecordTypesMessage) -> Self {
        ActorMessage::GetAllRecordTypes(msg)
    }
}

/// Receives messages and executes logic machine queries on a separate thread.
struct Actor {
    receiver: mpsc::Receiver<ActorMessage>,
    lm: LogicMachine,
}

impl Actor {
    fn new(
        program: &str,
        record_types: Vec<RecordType>,
        receiver: mpsc::Receiver<ActorMessage>,
    ) -> Self {
        Actor {
            receiver,
            lm: LogicMachine::new(program, record_types),
        }
    }

    fn handle_message(&mut self, msg: ActorMessage) {
        match msg {
            ActorMessage::Reload(Message { query, respond_to }) => {
                let r = Ok(self.lm.load_program(query.program, query.record_types));
                let _ = respond_to.send(r);
            }
            ActorMessage::GetAllFacts(Message { query, respond_to }) => {
                let r = self
                    .lm
                    .get_record_type(&query.fact_type)
                    .map(|rt| self.lm.fetch_all(rt.into()))
                    .unwrap_or(Ok(Vec::new()));
                let _ = respond_to.send(r);
            }
            ActorMessage::GetFacts(Message { query, respond_to }) => {
                let _ = respond_to.send(self.lm.fetch(query.goal, query.target_rt));
            }
            ActorMessage::GetRecordType(Message { query, respond_to }) => {
                let r = self.lm.get_record_type(&query.fact_type);
                let _ = respond_to.send(r);
            }
            ActorMessage::GetAllRecordTypes(Message { respond_to, .. }) => {
                let r = self.lm.get_all_record_types();
                let _ = respond_to.send(r);
            }
        }
    }
}

async fn run_actor(mut actor: Actor) {
    while let Some(msg) = actor.receiver.recv().await {
        actor.handle_message(msg);
    }
}

/// Handle for sending queries to the actor running in a separate Tokio thread.
#[derive(Clone)]
struct ActorHandle {
    sender: mpsc::Sender<ActorMessage>,
}

impl ActorHandle {
    fn new(program: String, record_types: Vec<RecordType>) -> Self {
        let (send, recv) = mpsc::channel(16);

        let rt = Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("Failed to create new thread");

        std::thread::spawn(move || {
            let actor = Actor::new(&program, record_types, recv);
            let local = LocalSet::new();

            // Spawn a task on this thread which is a loop that ends when all
            // ActorHandles have been dropped.
            local.spawn_local(run_actor(actor));

            // This will return once all senders are dropped and all
            // messages have been handled.
            rt.block_on(local);
        });

        Self { sender: send }
    }

    async fn send_query<Q, R>(&self, q: Q) -> R
    where
        ActorMessage: From<Message<Q, R>>,
    {
        let (send, recv) = oneshot::channel();

        let msg = Message {
            query: q,
            respond_to: send,
        };

        // Ignore send errors. If this send fails, so does the
        // recv.await below. There's no reason to check the
        // failure twice.
        let _ = self.sender.send(ActorMessage::from(msg)).await;
        recv.await.expect("Actor task has been killed")
    }
}

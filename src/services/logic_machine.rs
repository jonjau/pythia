use std::fs::File;
use std::io::BufReader;
use std::sync::Arc;
use std::vec;

use crate::models::fact::Fact;
use crate::models::goal::Goal;
use crate::models::logic_machine::{LogicMachine, LogicMachineResult};
use crate::models::record_type::{RecordType, RecordTypeBuilder, RecordTypeError, RecordTypeJson};

use tokio::sync::{mpsc, oneshot};
use tokio::{runtime::Builder, task::LocalSet};

#[derive(Clone)]
pub struct LogicMachineService {
    lm_actor: ActorHandle,
}

#[derive(thiserror::Error, Debug)]
enum ReadRecordTypesError {
    #[error("Failed to open file: {0}")]
    CouldNotOpenFile(#[from] std::io::Error),
    #[error("Failed to parse JSON: {0}")]
    CouldNotParseJson(#[from] serde_json::Error),
    #[error("Failed to build record type: {0}")]
    CouldNotBuildRecordType(#[from] RecordTypeError),
}

impl LogicMachineService {
    pub fn new(program: &str, record_types_file_path: &str) -> Self {
        LogicMachineService {
            lm_actor: ActorHandle::new(
                program.to_owned(),
                LogicMachineService::read_record_types_from_json(record_types_file_path).unwrap(),
            ),
        }
    }
    
    fn read_record_types_from_json(
        file_path: &str,
    ) -> Result<Vec<RecordType>, ReadRecordTypesError> {
        let reader = BufReader::new(File::open(file_path)?);
        let rts: Vec<RecordTypeJson> = serde_json::from_reader(reader)?;
        Ok(rts
            .into_iter()
            .map(|rt| {
                RecordTypeBuilder::new(rt.name, rt.data_fields)
                    .display_name(rt.display_name)
                    .id_fields(rt.id_fields)
                    .metadata_fields(rt.metadata_fields)
                    .build()
            })
            .collect::<Result<Vec<_>, RecordTypeError>>()?)
    }

    pub async fn get_record_type(
        &self,
        fact_type: impl Into<String>,
    ) -> LogicMachineResult<Arc<RecordType>> {
        self.lm_actor
            .send_query(GetRecordTypeQuery {
                fact_type: fact_type.into(),
            })
            .await
    }

    pub async fn get_all_record_types(&self) -> LogicMachineResult<Vec<Arc<RecordType>>> {
        self.lm_actor
            .send_query(GetAllRecordTypesQuery)
            .await
    }

    pub async fn get_all_facts(&self, fact_type: String) -> LogicMachineResult<Vec<Fact>> {
        self.lm_actor
            .send_query(GetAllFactsQuery { fact_type })
            .await
    }

    pub async fn get_facts(
        &self,
        goal: Goal,
        target_rt: Arc<RecordType>,
    ) -> LogicMachineResult<Vec<Fact>> {
        self.lm_actor
            .send_query(GetFactsQuery { goal, target_rt })
            .await
    }

    pub async fn add_fact(&self, fact: Fact) -> LogicMachineResult<Vec<Fact>> {
        self.lm_actor.send_query(AddFactQuery { fact }).await
    }
}

struct AddFactQuery {
    fact: Fact,
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

struct Message<Query, Response> {
    query: Query,
    respond_to: oneshot::Sender<Response>,
}

enum ActorMessage {
    AddFact(AddFactMessage),
    GetAllFacts(GetAllFactsMessage),
    GetFacts(GetFactsMessage),
    GetRecordType(GetRecordTypeMessage),
    GetAllRecordTypes(GetAllRecordTypesMessage)
}

type AddFactMessage = Message<AddFactQuery, LogicMachineResult<Vec<Fact>>>;
type GetAllFactsMessage = Message<GetAllFactsQuery, LogicMachineResult<Vec<Fact>>>;
type GetFactsMessage = Message<GetFactsQuery, LogicMachineResult<Vec<Fact>>>;
type GetRecordTypeMessage = Message<GetRecordTypeQuery, LogicMachineResult<Arc<RecordType>>>;
type GetAllRecordTypesMessage = Message<GetAllRecordTypesQuery, LogicMachineResult<Vec<Arc<RecordType>>>>;

impl From<AddFactMessage> for ActorMessage {
    fn from(msg: AddFactMessage) -> Self {
        ActorMessage::AddFact(msg)
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
        Actor { receiver, lm: LogicMachine::new(program, record_types) }
    }

    fn handle_message(&mut self, msg: ActorMessage) {
        match msg {
            ActorMessage::AddFact(Message { query, respond_to }) => {
                let r = self.lm.add_fact(query.fact);

                // The `let _ =` ignores any errors when sending.
                // This can happen if the `select!` macro is
                // to cancel waiting for the response.
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
                dbg!(&query.goal);
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

#[derive(Clone)]
struct ActorHandle {
    sender: mpsc::Sender<ActorMessage>,
}

impl ActorHandle {
    fn new(program: String, record_types: Vec<RecordType>) -> Self {
        let (send, recv) = mpsc::channel(16);

        let rt = Builder::new_current_thread().enable_all().build().unwrap();

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

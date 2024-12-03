use std::vec;

use crate::models::fact::{Fact, Goal, LogicMachineResult, RecordType};
use crate::models::{self};
use models::fact::LogicMachine;

use tokio::sync::{mpsc, oneshot};
use tokio::{runtime::Builder, task::LocalSet};

#[derive(Clone)]
pub struct FactService {
    lm_actor: ActorHandle,
}

impl FactService {
    pub fn new(program: &str) -> Self {
        FactService {
            lm_actor: ActorHandle::new(program.to_owned()),
        }
    }

    pub async fn is_valid_record_type(&self, fact_type: String, attr_names: Vec<String>) -> bool {
        self.lm_actor
            .send_query(RecordTypeQuery::IsValidRecordType {
                fact_type,
                attr_names,
            })
            .await
    }

    pub async fn get_all_facts(&self, fact_type: String) -> LogicMachineResult {
        self.lm_actor
            .send_query(FactQuery::GetAllFacts { fact_type })
            .await
    }

    pub async fn get_facts(&self, fact_type: String, attrs: Vec<String>) -> LogicMachineResult {
        self.lm_actor
            .send_query(FactQuery::GetFacts { fact_type, attrs })
            .await
    }

    pub async fn add_fact(&self, fact: Fact) -> LogicMachineResult {
        self.lm_actor.send_query(FactQuery::AddFact { fact }).await
    }
}

pub enum FactQuery {
    AddFact {
        fact: Fact,
    },
    GetAllFacts {
        fact_type: String,
    },
    GetFacts {
        fact_type: String,
        attrs: Vec<String>,
    },
}

pub enum RecordTypeQuery {
    IsValidRecordType {
        fact_type: String,
        attr_names: Vec<String>,
    },
}

struct Message<Query, Response> {
    query: Query,
    respond_to: oneshot::Sender<Response>,
}

enum ActorMessage {
    FactMessage(Message<FactQuery, LogicMachineResult>),
    RecordTypeMessage(Message<RecordTypeQuery, bool>),
}

impl From<Message<FactQuery, LogicMachineResult>> for ActorMessage {
    fn from(msg: Message<FactQuery, LogicMachineResult>) -> Self {
        ActorMessage::FactMessage(msg)
    }
}

impl From<Message<RecordTypeQuery, bool>> for ActorMessage {
    fn from(msg: Message<RecordTypeQuery, bool>) -> Self {
        ActorMessage::RecordTypeMessage(msg)
    }
}

struct Actor {
    receiver: mpsc::Receiver<ActorMessage>,
    lm: LogicMachine,
}

impl Actor {
    fn new(program: &str, receiver: mpsc::Receiver<ActorMessage>) -> Self {
        let mut lm = LogicMachine::new(program);
        lm.define_types(vec![
            RecordType::new("edge", &["X", "Y"]).unwrap(),
            RecordType::new("arc", &["X", "Y"]).unwrap(),
            RecordType::new(
                "dimlink",
                &["X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9"],
            )
            .unwrap(),
            RecordType::new("step_change", &["Ctx", "Id", "Vals1", "Vals2"]).unwrap(),
            RecordType::new("leap_change", &["Ctx", "Id", "Vals1", "Vals2"]).unwrap(),
        ]);

        Actor { receiver, lm: lm }
    }
    fn handle_message(&mut self, msg: ActorMessage) {
        match msg {
            ActorMessage::RecordTypeMessage(Message { query, respond_to }) => match query {
                RecordTypeQuery::IsValidRecordType {
                    fact_type,
                    attr_names,
                } => {
                    let r = self.lm.is_valid_record_type(
                        &fact_type,
                        attr_names.iter().map(String::as_str).collect(),
                    );
                    let _ = respond_to.send(r);
                }
            },
            ActorMessage::FactMessage(Message { query, respond_to }) => match query {
                FactQuery::AddFact { fact } => {
                    let r = self.lm.add_fact(fact);

                    // The `let _ =` ignores any errors when sending.
                    // This can happen if the `select!` macro is
                    // to cancel waiting for the response.
                    let _ = respond_to.send(r);
                }
                FactQuery::GetAllFacts { fact_type } => {
                    let r = self
                        .lm
                        .get_record_type(&fact_type)
                        .map(|rt| self.lm.fetch_all(rt))
                        .unwrap_or(Ok(Vec::new()));
                    let _ = respond_to.send(r);
                }
                FactQuery::GetFacts { fact_type, attrs } => {
                    let r = self
                        .lm
                        .get_record_type(&fact_type)
                        .map(|rt| self.lm.fetch(Goal::new(rt, attrs)))
                        .unwrap_or(Ok(Vec::new()));
                    let _ = respond_to.send(r);
                }
            },
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
    fn new(program: String) -> Self {
        let (send, recv) = mpsc::channel(16);

        let rt = Builder::new_current_thread().enable_all().build().unwrap();

        std::thread::spawn(move || {
            let actor = Actor::new(&program, recv);
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

use std::collections::HashMap;
use std::sync::Arc;
use std::vec;

use crate::models::fact::{Fact, LogicMachineResult, RecordType};
use crate::models::{self, fact};
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

    pub async fn get_facts(&self, fact_type: String) -> LogicMachineResult {
        self.lm_actor
            .send_query(LogicMachineQuery::GetFacts { fact_type })
            .await
    }

    pub async fn add_fact(&self, fact: Fact) -> LogicMachineResult {
        self.lm_actor.send_query(LogicMachineQuery::AddFact { fact }).await
    }
}

pub enum LogicMachineQuery {
    AddFact { fact: Fact },
    GetFacts { fact_type: String },
}

struct ActorMessage {
    query: LogicMachineQuery,
    respond_to: oneshot::Sender<LogicMachineResult>,
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
        ]);

        Actor { receiver, lm: lm }
    }
    fn handle_message(&mut self, ActorMessage { query, respond_to }: ActorMessage) {
        match query {
            LogicMachineQuery::AddFact { fact } => {
                // let edge = Arc::new(RecordType::new("edge", &["X", "Y"]).unwrap());
                // let r = self.lm.fetch_all(edge);

                // self.lm.add_fact(edge.to_fact(&HashMap::from([("X", "3"), ("Y", "5")])).unwrap());

                let r = self.lm.add_fact(fact);

                // The `let _ =` ignores any errors when sending.
                // This can happen if the `select!` macro is
                // to cancel waiting for the response.
                let _ = respond_to.send(r);
            }
            LogicMachineQuery::GetFacts { fact_type } => {
                let r = self
                    .lm
                    .predicate(&fact_type)
                    .map(|rt| self.lm.fetch_all(rt))
                    .unwrap_or(Ok(Vec::new()));
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

    async fn send_query(&self, q: LogicMachineQuery) -> LogicMachineResult {
        let (send, recv) = oneshot::channel();

        let msg = ActorMessage {
            query: q,
            respond_to: send,
        };

        // Ignore send errors. If this send fails, so does the
        // recv.await below. There's no reason to check the
        // failure twice.
        let _ = self.sender.send(msg).await;
        recv.await.expect("Actor task has been killed")
    }
}

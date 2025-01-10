use std::collections::HashMap;
use std::vec;

use crate::models::fact::{Fact, LogicMachineResult, RecordType, RecordTypeResult, Term};
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

    pub async fn get_record_type(&self, fact_type: String) -> RecordTypeResult {
        self.lm_actor
            .send_query(GetRecordTypeQuery { fact_type })
            .await
    }

    pub async fn get_all_facts(&self, fact_type: String) -> LogicMachineResult {
        self.lm_actor
            .send_query(GetAllFactsQuery { fact_type })
            .await
    }

    pub async fn get_facts(
        &self,
        fact_type: String,
        values: HashMap<String, Term>,
    ) -> LogicMachineResult {
        dbg!(&values);
        self.lm_actor
            .send_query(GetFactsQuery { fact_type, values })
            .await
    }

    pub async fn add_fact(&self, fact: Fact) -> LogicMachineResult {
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
    fact_type: String,
    values: HashMap<String, Term>,
}

struct GetRecordTypeQuery {
    fact_type: String,
}

struct Message<Query, Response> {
    query: Query,
    respond_to: oneshot::Sender<Response>,
}

enum ActorMessage {
    AddFact(AddFactMessage),
    GetAllFacts(GetAllFactsMessage),
    GetFacts(GetFactsMessage),
    GetRecordType(GetRecordTypeMessage),
}

type AddFactMessage = Message<AddFactQuery, LogicMachineResult>;
type GetAllFactsMessage = Message<GetAllFactsQuery, LogicMachineResult>;
type GetFactsMessage = Message<GetFactsQuery, LogicMachineResult>;
type GetRecordTypeMessage = Message<GetRecordTypeQuery, RecordTypeResult>;

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

struct Actor {
    receiver: mpsc::Receiver<ActorMessage>,
    lm: LogicMachine,
}

impl Actor {
    fn new(program: &str, receiver: mpsc::Receiver<ActorMessage>) -> Self {
        let mut lm = LogicMachine::new(program);
        lm.define_types(vec![
            RecordType::new_without_id_fields("edge", &["X", "Y"]).unwrap(),
            RecordType::new_without_id_fields("arc", &["X", "Y"]).unwrap(),
            RecordType::new(
                "dimlink",
                &["Id"],
                &["DimIdRef", "InvHeadRef", "BegPeriod", "EndPeriod"],
                &["Context", "SysVersion", "RecType", "SeqNum"],
            )
            .unwrap(),
            RecordType::new_without_id_fields("step_change", &["Ctx", "Id", "Vals1", "Vals2"])
                .unwrap(),
            RecordType::new_without_id_fields("leap_change", &["Ctx", "Id", "Vals1", "Vals2"])
                .unwrap(),
        ]);

        Actor { receiver, lm }
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
                // let rt = self.lm.get_record_type(&query.fact_type);
                println!("queryvalues: {:?}", &query.values);
                if let Ok(rt) = self.lm.get_record_type(&query.fact_type) {
                    if let Ok(g) = rt.to_goal(&query.values) {
                        let _ = respond_to.send(self.lm.fetch(g));
                    } else {
                        let _ = respond_to.send(Ok(Vec::new()));
                    }
                } else {
                    let _ = respond_to.send(Ok(Vec::new()));
                }
                // let r = self
                //     .lm
                //     .get_record_type(&query.fact_type)
                //     .map(|rt| {
                //         let a = rt.to_goal(
                //             &query
                //                 .values
                //                 .iter()
                //                 .map(|(field, value)| (field.as_str(), value.as_str()))
                //                 .collect::<HashMap<_, _>>(),
                //         );
                //         let b = a.map(|g| self.lm.fetch(g));
                //         dbg!(&b);
                //         let c = b.unwrap_or(Ok(Vec::new()));
                //         dbg!(&c);
                //         c
                //     })
                //     .unwrap_or(Ok(Vec::new()));
                // let _ = respond_to.send(Ok(Vec::new()));
            }
            ActorMessage::GetRecordType(Message { query, respond_to }) => {
                let r = self.lm.get_record_type(&query.fact_type);
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

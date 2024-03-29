use crate::models;
use models::fact::LogicMachine;

use tokio::sync::{mpsc, oneshot};
use tokio::{runtime::Builder, task::LocalSet};

use scryer_prolog::machine::parsed_results::QueryResult;

use std::sync::Arc;
use std::sync::Mutex;


pub type FactService = ActorHandle;

enum ActorMessage {
    GetUniqueId {
        respond_to: oneshot::Sender<u32>,
    },
    Query {
        respond_to: oneshot::Sender<QueryResult>,
    },
}

struct Actor {
    receiver: mpsc::Receiver<ActorMessage>,
    next_id: u32,
    lm: Arc<Mutex<LogicMachine>>,
}

impl Actor {
    fn new(receiver: mpsc::Receiver<ActorMessage>) -> Self {
        Actor {
            receiver,
            next_id: 0,
            lm: Arc::new(Mutex::new(LogicMachine::new(String::from(
                r#"edge(3, 4)."#,
            )))),
        }
    }
    fn handle_message(&mut self, msg: ActorMessage) {
        match msg {
            ActorMessage::GetUniqueId { respond_to } => {
                self.next_id += 1;

                // The `let _ =` ignores any errors when sending.
                //
                // This can happen if the `select!` macro is used
                // to cancel waiting for the response.
                let _ = respond_to.send(self.next_id);
            }
            ActorMessage::Query { respond_to } => {
                let mut m = self.lm.lock().unwrap();

                let res = m.machine.run_query(r#"edge(3, 4)."#.to_owned());

                let _ = respond_to.send(res);
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
pub struct ActorHandle {
    sender: mpsc::Sender<ActorMessage>,
}

impl ActorHandle {
    pub fn new() -> Self {
        let (send, recv) = mpsc::channel(8);

        let rt = Builder::new_current_thread().enable_all().build().unwrap();

        std::thread::spawn(move || {
            let actor = Actor::new(recv);
            let local = LocalSet::new();

            // Spawn a task on this thread which is a loop that ends when all
            // ActorHandles have been dropped.
            local.spawn_local(run_actor(actor));

            // This will return once all senders are dropped and all
            // spawned tasks have returned.
            rt.block_on(local);
        });

        Self { sender: send }
    }

    pub async fn get_unique_id(&self) -> u32 {
        let (send, recv) = oneshot::channel();
        let msg = ActorMessage::GetUniqueId { respond_to: send };

        // Ignore send errors. If this send fails, so does the
        // recv.await below. There's no reason to check the
        // failure twice.
        let _ = self.sender.send(msg).await;
        recv.await.expect("Actor task has been killed")
    }

    pub async fn test_query(&self) -> QueryResult {
        let (send, recv) = oneshot::channel();
        let msg = ActorMessage::Query { respond_to: send };

        let _ = self.sender.send(msg).await;
        recv.await.expect("Actor task has been killed")
    }
}

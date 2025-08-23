use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

/// Global tracker that manages instance counting
#[derive(Debug)]
struct GlobalTracker {
    counter: AtomicUsize,
}

impl GlobalTracker {
    fn next_id(&self) -> usize {
        self.counter.fetch_add(1, Ordering::Relaxed)
    }
}

impl PartialEq for GlobalTracker {
    fn eq(&self, _other: &Self) -> bool {
        // Since the counter itself is not relevant for PartialEq
        // of the GlobalTracker, we can simply return true.
        true
    }
}

/// Tracking context to manage global tracking
#[derive(Clone, Debug, PartialEq)]
pub struct IdContext {
    tracker: Arc<GlobalTracker>,
}

impl IdContext {
    pub fn new() -> Self {
        Self {
            tracker: Arc::new(GlobalTracker {
                counter: AtomicUsize::new(0),
            }),
        }
    }

    pub fn next_id(&self) -> usize {
        self.tracker.next_id()
    }
}

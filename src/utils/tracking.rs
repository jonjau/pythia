use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

// Trait to abstract instance tracking
trait InstanceTracked {
    fn instance_id(&self) -> usize;
}

// Global tracker that manages instance counting
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

// Wrapper to add instance tracking
pub struct Tracked<T> {
    inner: T,
    instance_id: usize,
}

// Tracking context to manage global tracking
#[derive(Clone, Debug, PartialEq)]
pub struct IdContext {
    tracker: Arc<GlobalTracker>,
}

impl IdContext {
    pub fn new() -> Self {
        Self {
            tracker: Arc::new(GlobalTracker { 
                counter: AtomicUsize::new(0) 
            }),
        }
    }

    pub fn next_id(&self) -> usize {
        self.tracker.next_id()
    }

    pub fn track<T>(&self, value: T) -> Tracked<T> {
        let instance_id = self.tracker.next_id();
        Tracked { 
            inner: value, 
            instance_id 
        }
    }
}

// Implement the trait for Tracked
impl<T> InstanceTracked for Tracked<T> {
    fn instance_id(&self) -> usize {
        self.instance_id
    }
}
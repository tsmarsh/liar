//! Task representation for the async runtime.
//!
//! A Task wraps a Pollable (pointer to liar struct) and its poll function.

use crate::waker::Waker;
use std::ptr;

/// Poll function signature - called by executor to drive the task.
/// Takes the pollable struct pointer and a waker, returns Poll result.
pub type PollFn = extern "C" fn(*mut u8, *mut Waker) -> Poll;

/// Result of polling a task.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Poll {
    /// Task completed with a result (pointer to value).
    Ready(*mut u8),
    /// Task is not ready, will be woken when I/O completes.
    Pending,
}

impl Poll {
    /// Check if the poll result is Ready.
    pub fn is_ready(&self) -> bool {
        matches!(self, Poll::Ready(_))
    }

    /// Check if the poll result is Pending.
    pub fn is_pending(&self) -> bool {
        matches!(self, Poll::Pending)
    }
}

/// State of a task in the executor.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TaskState {
    /// Task is waiting for I/O or other event.
    Pending,
    /// Task is ready to be polled.
    Runnable,
    /// Task has completed.
    Complete,
}

/// A task in the async runtime.
///
/// Tasks are created from liar Pollable structs and managed by the executor.
#[repr(C)]
pub struct Task {
    /// Pointer to the liar Pollable struct.
    pub pollable: *mut u8,
    /// Function to poll the task.
    pub poll_fn: PollFn,
    /// Next task in the intrusive linked list (for run queue).
    pub next: *mut Task,
    /// Current state of the task.
    pub state: TaskState,
    /// Result pointer (set when task completes).
    pub result: *mut u8,
}

impl Task {
    /// Create a new task from a pollable and poll function.
    pub fn new(pollable: *mut u8, poll_fn: PollFn) -> Box<Self> {
        Box::new(Task {
            pollable,
            poll_fn,
            next: ptr::null_mut(),
            state: TaskState::Runnable,
            result: ptr::null_mut(),
        })
    }

    /// Poll the task with the given waker.
    ///
    /// # Safety
    /// The pollable pointer must be valid and the poll_fn must be callable.
    pub unsafe fn poll(&mut self, waker: &mut Waker) -> Poll {
        (self.poll_fn)(self.pollable, waker as *mut Waker)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    extern "C" fn immediate_poll(_pollable: *mut u8, _waker: *mut Waker) -> Poll {
        Poll::Ready(42 as *mut u8)
    }

    #[test]
    fn test_task_creation() {
        let task = Task::new(ptr::null_mut(), immediate_poll);
        assert_eq!(task.state, TaskState::Runnable);
        assert!(task.next.is_null());
    }

    #[test]
    fn test_poll_ready() {
        let mut task = Task::new(ptr::null_mut(), immediate_poll);
        let mut waker = Waker::null();
        unsafe {
            let result = task.poll(&mut waker);
            assert!(result.is_ready());
            if let Poll::Ready(ptr) = result {
                assert_eq!(ptr as usize, 42);
            }
        }
    }
}

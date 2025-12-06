//! Task executor - polls tasks and manages the run queue.

use crate::reactor::{create_reactor, PlatformReactor, Reactor};
use crate::task::{Poll, PollFn, Task, TaskState};
use crate::waker::Waker;
use std::cell::RefCell;
use std::collections::VecDeque;

/// The task executor.
///
/// Manages a queue of runnable tasks and polls them to completion.
/// Uses the platform reactor to wait for I/O events.
pub struct Executor {
    /// Tasks ready to be polled.
    run_queue: VecDeque<*mut Task>,
    /// The I/O reactor.
    reactor: PlatformReactor,
    /// Next task ID for waker creation.
    next_task_id: usize,
}

impl Executor {
    /// Create a new executor.
    pub fn new() -> std::io::Result<Self> {
        Ok(Executor {
            run_queue: VecDeque::new(),
            reactor: create_reactor()?,
            next_task_id: 1,
        })
    }

    /// Spawn a new task.
    ///
    /// Returns a pointer to the task, which can be passed to block_on.
    pub fn spawn(&mut self, pollable: *mut u8, poll_fn: PollFn) -> *mut Task {
        let task = Task::new(pollable, poll_fn);
        let task_ptr = Box::into_raw(task);
        self.run_queue.push_back(task_ptr);
        task_ptr
    }

    /// Run until the given task completes.
    ///
    /// Returns the result pointer from the completed task.
    ///
    /// # Safety
    /// The root_task pointer must be valid and point to a task created by spawn.
    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    pub fn block_on(&mut self, root_task: *mut Task) -> *mut u8 {
        // Ensure root task is in the run queue
        if !self.run_queue.contains(&root_task) {
            self.run_queue.push_back(root_task);
        }

        loop {
            // Poll all runnable tasks
            while let Some(task_ptr) = self.run_queue.pop_front() {
                let task = unsafe { &mut *task_ptr };

                // Create a waker for this task
                let task_id = self.next_task_id;
                self.next_task_id += 1;
                let mut waker = Waker::new(task_id);

                // Poll the task
                let poll_result = unsafe { task.poll(&mut waker) };

                match poll_result {
                    Poll::Ready(result) => {
                        task.state = TaskState::Complete;
                        task.result = result;

                        if task_ptr == root_task {
                            // Root task completed, return result
                            return result;
                        }

                        // Non-root task completed - free it
                        unsafe {
                            drop(Box::from_raw(task_ptr));
                        }
                    }
                    Poll::Pending => {
                        task.state = TaskState::Pending;
                        // Task will be re-queued when its waker is triggered
                        // For now, we'll check if waker was triggered immediately
                        if waker.was_woken() {
                            self.run_queue.push_back(task_ptr);
                        }
                        // Otherwise, the reactor will wake it via I/O
                    }
                }
            }

            // Check if root task is complete
            let root = unsafe { &*root_task };
            if root.state == TaskState::Complete {
                return root.result;
            }

            // No runnable tasks - wait for I/O events
            // Use a short timeout so we can check wakers
            if let Err(e) = self.reactor.wait(Some(10)) {
                eprintln!("Reactor wait error: {}", e);
            }

            // Check for any woken tasks and re-queue them
            // Note: In a full implementation, we'd have a shared wake queue
            // that the reactor populates. For now, we poll periodically.
        }
    }

    /// Get a mutable reference to the reactor.
    pub fn reactor(&mut self) -> &mut PlatformReactor {
        &mut self.reactor
    }

    /// Check if there are runnable tasks.
    pub fn has_runnable_tasks(&self) -> bool {
        !self.run_queue.is_empty()
    }

    /// Add a task to the run queue (used by wakers).
    ///
    /// # Safety
    /// The task pointer must be valid.
    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    pub fn wake_task(&mut self, task: *mut Task) {
        if !self.run_queue.contains(&task) {
            unsafe {
                (*task).state = TaskState::Runnable;
            }
            self.run_queue.push_back(task);
        }
    }
}

impl Drop for Executor {
    fn drop(&mut self) {
        // Free any remaining tasks
        for task_ptr in self.run_queue.drain(..) {
            unsafe {
                drop(Box::from_raw(task_ptr));
            }
        }
    }
}

// Thread-local executor for use from FFI
thread_local! {
    static EXECUTOR: RefCell<Option<Executor>> = const { RefCell::new(None) };
}

/// Initialize the thread-local executor.
pub fn init_executor() -> std::io::Result<()> {
    EXECUTOR.with(|exec| {
        let mut exec = exec.borrow_mut();
        if exec.is_none() {
            *exec = Some(Executor::new()?);
        }
        Ok(())
    })
}

/// Get access to the thread-local executor.
pub fn with_executor<F, R>(f: F) -> R
where
    F: FnOnce(&mut Executor) -> R,
{
    EXECUTOR.with(|exec| {
        let mut exec = exec.borrow_mut();
        let exec = exec.as_mut().expect("Executor not initialized");
        f(exec)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ptr;

    // A poll function that immediately returns Ready
    extern "C" fn immediate_poll(_pollable: *mut u8, _waker: *mut Waker) -> Poll {
        Poll::Ready(42 as *mut u8)
    }

    // A poll function that returns the pollable value
    extern "C" fn return_pollable(pollable: *mut u8, _waker: *mut Waker) -> Poll {
        Poll::Ready(pollable)
    }

    // State for counting polls
    #[repr(C)]
    struct CountingPollable {
        count: i32,
        target: i32,
    }

    // A poll function that returns Pending until count reaches target
    extern "C" fn counting_poll(pollable: *mut u8, waker: *mut Waker) -> Poll {
        let state = unsafe { &mut *(pollable as *mut CountingPollable) };
        state.count += 1;
        if state.count >= state.target {
            Poll::Ready(state.count as *mut u8)
        } else {
            // Wake ourselves to be polled again
            unsafe {
                if !waker.is_null() {
                    (*waker).wake();
                }
            }
            Poll::Pending
        }
    }

    #[test]
    fn test_executor_creation() {
        let executor = Executor::new().expect("Failed to create executor");
        drop(executor);
    }

    #[test]
    fn test_spawn_immediate_task() {
        let mut executor = Executor::new().expect("Failed to create executor");
        let task = executor.spawn(ptr::null_mut(), immediate_poll);
        let result = executor.block_on(task);
        assert_eq!(result as usize, 42);
    }

    #[test]
    fn test_spawn_with_pollable() {
        let mut executor = Executor::new().expect("Failed to create executor");
        let value = 123usize;
        let task = executor.spawn(value as *mut u8, return_pollable);
        let result = executor.block_on(task);
        assert_eq!(result as usize, 123);
    }

    #[test]
    fn test_counting_task() {
        let mut executor = Executor::new().expect("Failed to create executor");
        let mut state = CountingPollable {
            count: 0,
            target: 5,
        };
        let task = executor.spawn(&mut state as *mut _ as *mut u8, counting_poll);
        let result = executor.block_on(task);
        assert_eq!(result as usize, 5);
        assert_eq!(state.count, 5);
    }
}

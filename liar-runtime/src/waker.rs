//! Waker implementation for the async runtime.
//!
//! Wakers are used to notify the executor when a task is ready to be polled.
//! They are passed to I/O operations and stored by the reactor.

use std::ptr;
use std::sync::atomic::{AtomicBool, Ordering};

/// A waker that can signal a task is ready to be polled.
///
/// Wakers are cloneable and thread-safe. When wake() is called,
/// the associated task is added to the executor's run queue.
#[repr(C)]
pub struct Waker {
    /// Shared state for this waker.
    inner: *mut WakerInner,
}

/// Inner state shared between waker clones.
struct WakerInner {
    /// Has this waker been triggered?
    woken: AtomicBool,
    /// Task ID for debugging.
    task_id: usize,
    /// Reference count.
    ref_count: std::sync::atomic::AtomicUsize,
}

impl Waker {
    /// Create a new waker for a task.
    pub fn new(task_id: usize) -> Self {
        let inner = Box::into_raw(Box::new(WakerInner {
            woken: AtomicBool::new(false),
            task_id,
            ref_count: std::sync::atomic::AtomicUsize::new(1),
        }));
        Waker { inner }
    }

    /// Create a null waker (for testing immediate futures).
    pub fn null() -> Self {
        Waker {
            inner: ptr::null_mut(),
        }
    }

    /// Wake the associated task.
    ///
    /// This signals to the executor that the task should be polled again.
    pub fn wake(&self) {
        if !self.inner.is_null() {
            unsafe {
                (*self.inner).woken.store(true, Ordering::Release);
            }
        }
    }

    /// Check if the waker has been triggered.
    pub fn was_woken(&self) -> bool {
        if self.inner.is_null() {
            false
        } else {
            unsafe { (*self.inner).woken.load(Ordering::Acquire) }
        }
    }

    /// Get the task ID associated with this waker.
    pub fn task_id(&self) -> usize {
        if self.inner.is_null() {
            0
        } else {
            unsafe { (*self.inner).task_id }
        }
    }

    /// Reset the woken state.
    pub fn reset(&self) {
        if !self.inner.is_null() {
            unsafe {
                (*self.inner).woken.store(false, Ordering::Release);
            }
        }
    }
}

impl Clone for Waker {
    fn clone(&self) -> Self {
        if !self.inner.is_null() {
            unsafe {
                (*self.inner).ref_count.fetch_add(1, Ordering::Relaxed);
            }
        }
        Waker { inner: self.inner }
    }
}

impl Drop for Waker {
    fn drop(&mut self) {
        if !self.inner.is_null() {
            unsafe {
                if (*self.inner).ref_count.fetch_sub(1, Ordering::Release) == 1 {
                    std::sync::atomic::fence(Ordering::Acquire);
                    drop(Box::from_raw(self.inner));
                }
            }
        }
    }
}

// Waker is Send + Sync because WakerInner uses atomics
unsafe impl Send for Waker {}
unsafe impl Sync for Waker {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_waker_creation() {
        let waker = Waker::new(42);
        assert_eq!(waker.task_id(), 42);
        assert!(!waker.was_woken());
    }

    #[test]
    fn test_waker_wake() {
        let waker = Waker::new(1);
        assert!(!waker.was_woken());
        waker.wake();
        assert!(waker.was_woken());
    }

    #[test]
    fn test_waker_clone() {
        let waker1 = Waker::new(1);
        let waker2 = waker1.clone();
        waker1.wake();
        assert!(waker2.was_woken());
    }

    #[test]
    fn test_null_waker() {
        let waker = Waker::null();
        assert_eq!(waker.task_id(), 0);
        assert!(!waker.was_woken());
        waker.wake(); // Should not crash
    }
}

//! I/O Reactor - waits for file descriptor events and wakes tasks.
//!
//! Platform-specific implementations:
//! - Linux: epoll
//! - macOS/BSD: kqueue

use crate::waker::Waker;
use std::io;

#[cfg(target_os = "linux")]
mod epoll;
#[cfg(target_os = "linux")]
pub use epoll::EpollReactor as PlatformReactor;

#[cfg(any(target_os = "macos", target_os = "freebsd", target_os = "openbsd"))]
mod kqueue;
#[cfg(any(target_os = "macos", target_os = "freebsd", target_os = "openbsd"))]
pub use kqueue::KqueueReactor as PlatformReactor;

/// Interest in a file descriptor event.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Interest {
    /// Interested in read readiness.
    Read,
    /// Interested in write readiness.
    Write,
    /// Interested in both read and write.
    Both,
}

/// Trait for platform-specific I/O event notification.
pub trait Reactor: Send {
    /// Register interest in a file descriptor.
    ///
    /// When the fd becomes ready for the specified operation,
    /// the waker will be triggered.
    fn register(&mut self, fd: i32, interest: Interest, waker: Waker) -> io::Result<()>;

    /// Update the interest for an already registered fd.
    fn modify(&mut self, fd: i32, interest: Interest, waker: Waker) -> io::Result<()>;

    /// Remove interest in a file descriptor.
    fn deregister(&mut self, fd: i32) -> io::Result<()>;

    /// Wait for events and wake the corresponding tasks.
    ///
    /// If timeout_ms is None, wait indefinitely.
    /// If timeout_ms is Some(0), poll without blocking.
    /// Otherwise wait up to the specified milliseconds.
    ///
    /// Returns the number of events that occurred.
    fn wait(&mut self, timeout_ms: Option<i32>) -> io::Result<usize>;
}

/// Create a new platform-specific reactor.
pub fn create_reactor() -> io::Result<PlatformReactor> {
    PlatformReactor::new()
}

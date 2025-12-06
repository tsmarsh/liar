//! liar-runtime: Async runtime for the liar language
//!
//! Provides:
//! - Task executor with poll-based scheduling
//! - Platform-specific I/O reactor (epoll on Linux, kqueue on macOS)
//! - FFI interface for liar to spawn and await tasks
//!
//! # Architecture
//!
//! ```text
//! liar code
//!     |
//!     v
//! FFI (liar_spawn, liar_block_on, liar_io_*)
//!     |
//!     v
//! Executor (polls tasks, manages run queue)
//!     |
//!     v
//! Reactor (epoll/kqueue, wakes tasks on I/O ready)
//! ```

pub mod executor;
pub mod ffi;
pub mod io;
pub mod reactor;
pub mod task;
pub mod waker;

pub use executor::Executor;
pub use task::{Poll, Task, TaskState};
pub use waker::Waker;

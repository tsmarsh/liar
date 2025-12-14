//! FFI interface for liar to call the runtime.
//!
//! These functions are exposed with C ABI and can be called from
//! liar-generated code.
//!
//! # Safety
//! All functions that take raw pointers perform null checks and use
//! unsafe blocks appropriately. The caller is responsible for ensuring
//! pointers are valid.

#![allow(clippy::not_unsafe_ptr_arg_deref)]

use crate::executor::{init_executor, with_executor};
use crate::io::{AcceptFuture, ConnectFuture, ReadFuture, WriteFuture};
use crate::task::{Poll, PollFn, Task};
use crate::waker::Waker;

/// Initialize the runtime.
///
/// Must be called before any other runtime functions.
/// Returns 0 on success, -1 on error.
#[no_mangle]
pub extern "C" fn liar_runtime_init() -> i32 {
    match init_executor() {
        Ok(()) => 0,
        Err(_) => -1,
    }
}

/// Spawn a new task.
///
/// - pollable: Pointer to the liar Pollable struct.
/// - poll_fn: Function pointer to poll the task.
///
/// Returns a pointer to the Task, or null on error.
#[no_mangle]
pub extern "C" fn liar_spawn(pollable: *mut u8, poll_fn: PollFn) -> *mut Task {
    with_executor(|exec| exec.spawn(pollable, poll_fn))
}

/// Block until a task completes.
///
/// - task: Pointer to the task to wait for.
///
/// Returns the result pointer from the completed task.
#[no_mangle]
pub extern "C" fn liar_block_on(task: *mut Task) -> *mut u8 {
    with_executor(|exec| exec.block_on(task))
}

/// Create an async read operation.
///
/// - fd: File descriptor to read from.
/// - buf: Buffer to read into.
/// - len: Maximum bytes to read.
///
/// Returns a pointer to the ReadFuture.
#[no_mangle]
pub extern "C" fn liar_io_read(fd: i64, buf: *mut u8, len: i64) -> *mut ReadFuture {
    Box::into_raw(ReadFuture::new(fd as i32, buf, len as usize))
}

/// Sentinel value for Poll::Pending.
/// Using i64::MIN which is clearly distinguishable from valid byte counts.
pub const POLL_PENDING: i64 = i64::MIN;

/// Poll a read operation.
///
/// - future: Pointer to the ReadFuture.
/// - waker: Pointer to the waker to use.
///
/// Returns:
/// - `>= 0`: Ready with that many bytes read
/// - `-1`: Error
/// - `POLL_PENDING` (i64::MIN): Pending
#[no_mangle]
pub extern "C" fn liar_io_read_poll(future: *mut ReadFuture, waker: *mut Waker) -> i64 {
    if future.is_null() {
        return -1;
    }
    let future = unsafe { &mut *future };
    let waker = if waker.is_null() {
        Waker::null()
    } else {
        unsafe { (*waker).clone() }
    };
    match future.poll(&waker) {
        Poll::Ready(ptr) => ptr as i64,
        Poll::Pending => POLL_PENDING,
    }
}

/// Free a read future.
#[no_mangle]
pub extern "C" fn liar_io_read_free(future: *mut ReadFuture) {
    if !future.is_null() {
        unsafe {
            drop(Box::from_raw(future));
        }
    }
}

/// Create an async write operation.
///
/// - fd: File descriptor to write to.
/// - buf: Buffer to write from.
/// - len: Number of bytes to write.
///
/// Returns a pointer to the WriteFuture.
#[no_mangle]
pub extern "C" fn liar_io_write(fd: i64, buf: *const u8, len: i64) -> *mut WriteFuture {
    Box::into_raw(WriteFuture::new(fd as i32, buf, len as usize))
}

/// Poll a write operation.
///
/// Returns:
/// - `>= 0`: Ready with that many bytes written
/// - `-1`: Error
/// - `POLL_PENDING` (i64::MIN): Pending
#[no_mangle]
pub extern "C" fn liar_io_write_poll(future: *mut WriteFuture, waker: *mut Waker) -> i64 {
    if future.is_null() {
        return -1;
    }
    let future = unsafe { &mut *future };
    let waker = if waker.is_null() {
        Waker::null()
    } else {
        unsafe { (*waker).clone() }
    };
    match future.poll(&waker) {
        Poll::Ready(ptr) => ptr as i64,
        Poll::Pending => POLL_PENDING,
    }
}

/// Free a write future.
#[no_mangle]
pub extern "C" fn liar_io_write_free(future: *mut WriteFuture) {
    if !future.is_null() {
        unsafe {
            drop(Box::from_raw(future));
        }
    }
}

/// Create an async accept operation.
///
/// - fd: Listening socket file descriptor.
///
/// Returns a pointer to the AcceptFuture.
#[no_mangle]
pub extern "C" fn liar_io_accept(fd: i64) -> *mut AcceptFuture {
    Box::into_raw(AcceptFuture::new(fd as i32))
}

/// Poll an accept operation.
///
/// Returns:
/// - `>= 0`: Ready with client fd
/// - `-1`: Error
/// - `POLL_PENDING` (i64::MIN): Pending
#[no_mangle]
pub extern "C" fn liar_io_accept_poll(future: *mut AcceptFuture, waker: *mut Waker) -> i64 {
    if future.is_null() {
        return -1;
    }
    let future = unsafe { &mut *future };
    let waker = if waker.is_null() {
        Waker::null()
    } else {
        unsafe { (*waker).clone() }
    };
    match future.poll(&waker) {
        Poll::Ready(ptr) => ptr as i64,
        Poll::Pending => POLL_PENDING,
    }
}

/// Free an accept future.
#[no_mangle]
pub extern "C" fn liar_io_accept_free(future: *mut AcceptFuture) {
    if !future.is_null() {
        unsafe {
            drop(Box::from_raw(future));
        }
    }
}

/// Create an async connect operation.
///
/// - fd: Socket file descriptor.
/// - addr: Pointer to sockaddr structure.
/// - addr_len: Size of the sockaddr structure.
///
/// Returns a pointer to the ConnectFuture.
#[no_mangle]
pub extern "C" fn liar_io_connect(fd: i64, addr: *const u8, addr_len: i64) -> *mut ConnectFuture {
    Box::into_raw(ConnectFuture::new(fd as i32, addr, addr_len as u32))
}

/// Poll a connect operation.
///
/// Returns:
/// - `0`: Ready, connected successfully
/// - `-1`: Error
/// - `POLL_PENDING` (i64::MIN): Pending
#[no_mangle]
pub extern "C" fn liar_io_connect_poll(future: *mut ConnectFuture, waker: *mut Waker) -> i64 {
    if future.is_null() {
        return -1;
    }
    let future = unsafe { &mut *future };
    let waker = if waker.is_null() {
        Waker::null()
    } else {
        unsafe { (*waker).clone() }
    };
    match future.poll(&waker) {
        Poll::Ready(ptr) => ptr as i64,
        Poll::Pending => POLL_PENDING,
    }
}

/// Free a connect future.
#[no_mangle]
pub extern "C" fn liar_io_connect_free(future: *mut ConnectFuture) {
    if !future.is_null() {
        unsafe {
            drop(Box::from_raw(future));
        }
    }
}

/// Register a file descriptor for read readiness.
///
/// When the fd becomes readable, the waker will be triggered.
#[no_mangle]
pub extern "C" fn liar_reactor_register_read(fd: i64, waker: *mut Waker) -> i32 {
    use crate::reactor::{Interest, Reactor};

    if waker.is_null() {
        return -1;
    }

    with_executor(|exec| {
        let waker = unsafe { (*waker).clone() };
        match exec.reactor().register(fd as i32, Interest::Read, waker) {
            Ok(()) => 0,
            Err(_) => -1,
        }
    })
}

/// Register a file descriptor for write readiness.
#[no_mangle]
pub extern "C" fn liar_reactor_register_write(fd: i64, waker: *mut Waker) -> i32 {
    use crate::reactor::{Interest, Reactor};

    if waker.is_null() {
        return -1;
    }

    with_executor(|exec| {
        let waker = unsafe { (*waker).clone() };
        match exec.reactor().register(fd as i32, Interest::Write, waker) {
            Ok(()) => 0,
            Err(_) => -1,
        }
    })
}

/// Deregister a file descriptor from the reactor.
#[no_mangle]
pub extern "C" fn liar_reactor_deregister(fd: i64) -> i32 {
    use crate::reactor::Reactor;

    with_executor(|exec| match exec.reactor().deregister(fd as i32) {
        Ok(()) => 0,
        Err(_) => -1,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ptr;

    extern "C" fn immediate_poll(_pollable: *mut u8, _waker: *mut Waker) -> Poll {
        Poll::Ready(42 as *mut u8)
    }

    #[test]
    fn test_ffi_init_and_spawn() {
        assert_eq!(liar_runtime_init(), 0);

        let task = liar_spawn(ptr::null_mut(), immediate_poll);
        assert!(!task.is_null());

        let result = liar_block_on(task);
        assert_eq!(result as usize, 42);
    }

    #[test]
    fn test_ffi_io_read() {
        // Create a pipe
        let mut fds = [0i32; 2];
        unsafe {
            assert_eq!(libc::pipe(fds.as_mut_ptr()), 0);
            libc::write(fds[1], b"test".as_ptr() as *const _, 4);
        }

        let mut buf = [0u8; 10];
        let future = liar_io_read(fds[0] as i64, buf.as_mut_ptr(), buf.len() as i64);

        let result = liar_io_read_poll(future, ptr::null_mut());
        // result is now i64, not Poll enum
        assert!(result >= 0); // Ready with bytes
        assert_eq!(result, 4);

        liar_io_read_free(future);

        unsafe {
            libc::close(fds[0]);
            libc::close(fds[1]);
        }
    }
}

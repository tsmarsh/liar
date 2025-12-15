//! kqueue-based reactor for macOS and BSD.

use super::{Interest, Reactor};
use crate::waker::Waker;
use std::collections::HashMap;
use std::io;
use std::os::unix::io::RawFd;

/// kqueue reactor for macOS/BSD systems.
pub struct KqueueReactor {
    /// The kqueue file descriptor.
    kq: RawFd,
    /// Buffer for kevent results.
    events: Vec<libc::kevent>,
    /// Map from fd to waker for registered interests.
    registered: HashMap<RawFd, Waker>,
}

// SAFETY: KqueueReactor is Send because:
// - kqueue file descriptors are thread-safe at the OS level
// - The kevent structs we store only use null pointers for udata
// - The HashMap<RawFd, Waker> is Send (Waker is Send)
unsafe impl Send for KqueueReactor {}

impl KqueueReactor {
    /// Create a new kqueue reactor.
    pub fn new() -> io::Result<Self> {
        let kq = unsafe { libc::kqueue() };
        if kq < 0 {
            return Err(io::Error::last_os_error());
        }

        // Set close-on-exec
        unsafe {
            let flags = libc::fcntl(kq, libc::F_GETFD);
            libc::fcntl(kq, libc::F_SETFD, flags | libc::FD_CLOEXEC);
        }

        Ok(KqueueReactor {
            kq,
            events: vec![
                libc::kevent {
                    ident: 0,
                    filter: 0,
                    flags: 0,
                    fflags: 0,
                    data: 0,
                    udata: std::ptr::null_mut(),
                };
                64
            ],
            registered: HashMap::new(),
        })
    }
}

impl Drop for KqueueReactor {
    fn drop(&mut self) {
        unsafe {
            libc::close(self.kq);
        }
    }
}

impl Reactor for KqueueReactor {
    fn register(&mut self, fd: i32, interest: Interest, waker: Waker) -> io::Result<()> {
        let mut changes = Vec::new();

        if matches!(interest, Interest::Read | Interest::Both) {
            changes.push(libc::kevent {
                ident: fd as usize,
                filter: libc::EVFILT_READ,
                flags: libc::EV_ADD | libc::EV_CLEAR,
                fflags: 0,
                data: 0,
                udata: std::ptr::null_mut(),
            });
        }

        if matches!(interest, Interest::Write | Interest::Both) {
            changes.push(libc::kevent {
                ident: fd as usize,
                filter: libc::EVFILT_WRITE,
                flags: libc::EV_ADD | libc::EV_CLEAR,
                fflags: 0,
                data: 0,
                udata: std::ptr::null_mut(),
            });
        }

        let result = unsafe {
            libc::kevent(
                self.kq,
                changes.as_ptr(),
                changes.len() as i32,
                std::ptr::null_mut(),
                0,
                std::ptr::null(),
            )
        };

        if result < 0 {
            return Err(io::Error::last_os_error());
        }

        self.registered.insert(fd, waker);
        Ok(())
    }

    fn modify(&mut self, fd: i32, interest: Interest, waker: Waker) -> io::Result<()> {
        // kqueue doesn't need explicit modify - just re-register
        self.register(fd, interest, waker)
    }

    fn deregister(&mut self, fd: i32) -> io::Result<()> {
        let changes = [
            libc::kevent {
                ident: fd as usize,
                filter: libc::EVFILT_READ,
                flags: libc::EV_DELETE,
                fflags: 0,
                data: 0,
                udata: std::ptr::null_mut(),
            },
            libc::kevent {
                ident: fd as usize,
                filter: libc::EVFILT_WRITE,
                flags: libc::EV_DELETE,
                fflags: 0,
                data: 0,
                udata: std::ptr::null_mut(),
            },
        ];

        // Ignore errors - the fd might only have been registered for one direction
        unsafe {
            libc::kevent(
                self.kq,
                changes.as_ptr(),
                2,
                std::ptr::null_mut(),
                0,
                std::ptr::null(),
            );
        }

        self.registered.remove(&fd);
        Ok(())
    }

    fn wait(&mut self, timeout_ms: Option<i32>) -> io::Result<usize> {
        let timeout = timeout_ms.map(|ms| libc::timespec {
            tv_sec: (ms / 1000) as i64,
            tv_nsec: ((ms % 1000) * 1_000_000) as i64,
        });

        let n = unsafe {
            libc::kevent(
                self.kq,
                std::ptr::null(),
                0,
                self.events.as_mut_ptr(),
                self.events.len() as i32,
                timeout.as_ref().map_or(std::ptr::null(), |t| t),
            )
        };

        if n < 0 {
            let err = io::Error::last_os_error();
            if err.kind() == io::ErrorKind::Interrupted {
                return Ok(0);
            }
            return Err(err);
        }

        // Wake tasks for ready fds
        for i in 0..n as usize {
            let fd = self.events[i].ident as RawFd;
            if let Some(waker) = self.registered.get(&fd) {
                waker.wake();
            }
        }

        Ok(n as usize)
    }
}

#[cfg(test)]
#[cfg(any(target_os = "macos", target_os = "freebsd", target_os = "openbsd"))]
mod tests {
    use super::*;

    #[test]
    fn test_kqueue_creation() {
        let reactor = KqueueReactor::new().expect("Failed to create kqueue reactor");
        drop(reactor);
    }

    #[test]
    fn test_kqueue_pipe_read() {
        let mut reactor = KqueueReactor::new().expect("Failed to create kqueue reactor");

        // Create a pipe
        let mut fds = [0i32; 2];
        unsafe {
            assert_eq!(libc::pipe(fds.as_mut_ptr()), 0);
        }
        let (read_fd, write_fd) = (fds[0], fds[1]);

        // Register read interest
        let waker = Waker::new(1);
        reactor
            .register(read_fd, Interest::Read, waker.clone())
            .expect("Failed to register");

        // Write to the pipe
        unsafe {
            libc::write(write_fd, b"hello".as_ptr() as *const _, 5);
        }

        // Wait should return with an event
        let n = reactor.wait(Some(100)).expect("Wait failed");
        assert!(n > 0);
        assert!(waker.was_woken());

        // Cleanup
        reactor.deregister(read_fd).ok();
        unsafe {
            libc::close(read_fd);
            libc::close(write_fd);
        }
    }
}

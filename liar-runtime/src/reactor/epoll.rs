//! epoll-based reactor for Linux.

use super::{Interest, Reactor};
use crate::waker::Waker;
use std::collections::HashMap;
use std::io;
use std::os::unix::io::RawFd;

/// epoll reactor for Linux systems.
pub struct EpollReactor {
    /// The epoll file descriptor.
    epoll_fd: RawFd,
    /// Buffer for epoll_wait events.
    events: Vec<libc::epoll_event>,
    /// Map from fd to waker for registered interests.
    registered: HashMap<RawFd, Waker>,
}

impl EpollReactor {
    /// Create a new epoll reactor.
    pub fn new() -> io::Result<Self> {
        let epoll_fd = unsafe { libc::epoll_create1(libc::EPOLL_CLOEXEC) };
        if epoll_fd < 0 {
            return Err(io::Error::last_os_error());
        }

        Ok(EpollReactor {
            epoll_fd,
            events: vec![libc::epoll_event { events: 0, u64: 0 }; 64],
            registered: HashMap::new(),
        })
    }

    fn interest_to_events(interest: Interest) -> u32 {
        match interest {
            Interest::Read => libc::EPOLLIN as u32,
            Interest::Write => libc::EPOLLOUT as u32,
            Interest::Both => (libc::EPOLLIN | libc::EPOLLOUT) as u32,
        }
    }
}

impl Drop for EpollReactor {
    fn drop(&mut self) {
        unsafe {
            libc::close(self.epoll_fd);
        }
    }
}

impl Reactor for EpollReactor {
    fn register(&mut self, fd: i32, interest: Interest, waker: Waker) -> io::Result<()> {
        let mut event = libc::epoll_event {
            events: Self::interest_to_events(interest) | libc::EPOLLET as u32,
            u64: fd as u64,
        };

        let result = unsafe { libc::epoll_ctl(self.epoll_fd, libc::EPOLL_CTL_ADD, fd, &mut event) };

        if result < 0 {
            return Err(io::Error::last_os_error());
        }

        self.registered.insert(fd, waker);
        Ok(())
    }

    fn modify(&mut self, fd: i32, interest: Interest, waker: Waker) -> io::Result<()> {
        let mut event = libc::epoll_event {
            events: Self::interest_to_events(interest) | libc::EPOLLET as u32,
            u64: fd as u64,
        };

        let result = unsafe { libc::epoll_ctl(self.epoll_fd, libc::EPOLL_CTL_MOD, fd, &mut event) };

        if result < 0 {
            return Err(io::Error::last_os_error());
        }

        self.registered.insert(fd, waker);
        Ok(())
    }

    fn deregister(&mut self, fd: i32) -> io::Result<()> {
        let result = unsafe {
            libc::epoll_ctl(self.epoll_fd, libc::EPOLL_CTL_DEL, fd, std::ptr::null_mut())
        };

        if result < 0 {
            return Err(io::Error::last_os_error());
        }

        self.registered.remove(&fd);
        Ok(())
    }

    fn wait(&mut self, timeout_ms: Option<i32>) -> io::Result<usize> {
        let timeout = timeout_ms.unwrap_or(-1);

        let n = unsafe {
            libc::epoll_wait(
                self.epoll_fd,
                self.events.as_mut_ptr(),
                self.events.len() as i32,
                timeout,
            )
        };

        if n < 0 {
            let err = io::Error::last_os_error();
            // EINTR is not an error, just no events
            if err.kind() == io::ErrorKind::Interrupted {
                return Ok(0);
            }
            return Err(err);
        }

        // Wake tasks for ready fds
        for i in 0..n as usize {
            let fd = self.events[i].u64 as RawFd;
            if let Some(waker) = self.registered.get(&fd) {
                waker.wake();
            }
        }

        Ok(n as usize)
    }
}

#[cfg(test)]
#[cfg(target_os = "linux")]
mod tests {
    use super::*;

    #[test]
    fn test_epoll_creation() {
        let reactor = EpollReactor::new().expect("Failed to create epoll reactor");
        drop(reactor);
    }

    #[test]
    fn test_epoll_pipe_read() {
        let mut reactor = EpollReactor::new().expect("Failed to create epoll reactor");

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

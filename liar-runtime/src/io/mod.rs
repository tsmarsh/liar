//! Async I/O operations.
//!
//! Provides non-blocking file and network I/O that integrates
//! with the reactor and executor.

use crate::task::Poll;
use crate::waker::Waker;
use std::io;
use std::os::unix::io::RawFd;

/// State of an I/O operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IoState {
    /// Operation not started.
    Init,
    /// Waiting for fd to be ready.
    Waiting,
    /// Operation completed.
    Done,
}

/// An async read operation.
#[repr(C)]
pub struct ReadFuture {
    fd: RawFd,
    buf: *mut u8,
    len: usize,
    bytes_read: isize,
    state: IoState,
}

impl ReadFuture {
    /// Create a new read future.
    pub fn new(fd: RawFd, buf: *mut u8, len: usize) -> Box<Self> {
        Box::new(ReadFuture {
            fd,
            buf,
            len,
            bytes_read: 0,
            state: IoState::Init,
        })
    }

    /// Poll this read operation.
    ///
    /// Returns Ready with bytes read, or Pending if not ready.
    pub fn poll(&mut self, _waker: &Waker) -> Poll {
        match self.state {
            IoState::Init => {
                // Try to read immediately (non-blocking)
                self.try_read()
            }
            IoState::Waiting => {
                // Check if we can read now
                self.try_read()
            }
            IoState::Done => Poll::Ready(self.bytes_read as *mut u8),
        }
    }

    fn try_read(&mut self) -> Poll {
        // Set non-blocking
        unsafe {
            let flags = libc::fcntl(self.fd, libc::F_GETFL);
            libc::fcntl(self.fd, libc::F_SETFL, flags | libc::O_NONBLOCK);
        }

        let result = unsafe { libc::read(self.fd, self.buf as *mut _, self.len) };

        if result >= 0 {
            self.bytes_read = result;
            self.state = IoState::Done;
            Poll::Ready(result as *mut u8)
        } else {
            let err = io::Error::last_os_error();
            if err.kind() == io::ErrorKind::WouldBlock {
                self.state = IoState::Waiting;
                Poll::Pending
            } else {
                // Real error - return negative value
                self.bytes_read = -1;
                self.state = IoState::Done;
                Poll::Ready(-1isize as *mut u8)
            }
        }
    }
}

/// An async write operation.
#[repr(C)]
pub struct WriteFuture {
    fd: RawFd,
    buf: *const u8,
    len: usize,
    bytes_written: isize,
    state: IoState,
}

impl WriteFuture {
    /// Create a new write future.
    pub fn new(fd: RawFd, buf: *const u8, len: usize) -> Box<Self> {
        Box::new(WriteFuture {
            fd,
            buf,
            len,
            bytes_written: 0,
            state: IoState::Init,
        })
    }

    /// Poll this write operation.
    pub fn poll(&mut self, _waker: &Waker) -> Poll {
        match self.state {
            IoState::Init | IoState::Waiting => self.try_write(),
            IoState::Done => Poll::Ready(self.bytes_written as *mut u8),
        }
    }

    fn try_write(&mut self) -> Poll {
        // Set non-blocking
        unsafe {
            let flags = libc::fcntl(self.fd, libc::F_GETFL);
            libc::fcntl(self.fd, libc::F_SETFL, flags | libc::O_NONBLOCK);
        }

        let result = unsafe { libc::write(self.fd, self.buf as *const _, self.len) };

        if result >= 0 {
            self.bytes_written = result;
            self.state = IoState::Done;
            Poll::Ready(result as *mut u8)
        } else {
            let err = io::Error::last_os_error();
            if err.kind() == io::ErrorKind::WouldBlock {
                self.state = IoState::Waiting;
                Poll::Pending
            } else {
                self.bytes_written = -1;
                self.state = IoState::Done;
                Poll::Ready(-1isize as *mut u8)
            }
        }
    }
}

/// An async accept operation.
#[repr(C)]
pub struct AcceptFuture {
    fd: RawFd,
    client_fd: i32,
    state: IoState,
}

impl AcceptFuture {
    /// Create a new accept future for a listening socket.
    pub fn new(fd: RawFd) -> Box<Self> {
        Box::new(AcceptFuture {
            fd,
            client_fd: -1,
            state: IoState::Init,
        })
    }

    /// Poll this accept operation.
    ///
    /// Returns Ready with client fd, or Pending if no connection available.
    pub fn poll(&mut self, _waker: &Waker) -> Poll {
        match self.state {
            IoState::Init | IoState::Waiting => self.try_accept(),
            IoState::Done => Poll::Ready(self.client_fd as *mut u8),
        }
    }

    fn try_accept(&mut self) -> Poll {
        // Set non-blocking
        unsafe {
            let flags = libc::fcntl(self.fd, libc::F_GETFL);
            libc::fcntl(self.fd, libc::F_SETFL, flags | libc::O_NONBLOCK);
        }

        let result = unsafe { libc::accept(self.fd, std::ptr::null_mut(), std::ptr::null_mut()) };

        if result >= 0 {
            self.client_fd = result;
            self.state = IoState::Done;
            Poll::Ready(result as *mut u8)
        } else {
            let err = io::Error::last_os_error();
            if err.kind() == io::ErrorKind::WouldBlock {
                self.state = IoState::Waiting;
                Poll::Pending
            } else {
                self.client_fd = -1;
                self.state = IoState::Done;
                Poll::Ready(-1isize as *mut u8)
            }
        }
    }
}

/// An async connect operation.
#[repr(C)]
pub struct ConnectFuture {
    fd: RawFd,
    addr: *const libc::sockaddr,
    addr_len: libc::socklen_t,
    result: i32,
    state: IoState,
}

impl ConnectFuture {
    /// Create a new connect future.
    pub fn new(fd: RawFd, addr: *const u8, addr_len: u32) -> Box<Self> {
        Box::new(ConnectFuture {
            fd,
            addr: addr as *const libc::sockaddr,
            addr_len,
            result: 0,
            state: IoState::Init,
        })
    }

    /// Poll this connect operation.
    ///
    /// Returns Ready with 0 on success, -1 on error, or Pending.
    pub fn poll(&mut self, _waker: &Waker) -> Poll {
        match self.state {
            IoState::Init => self.try_connect(),
            IoState::Waiting => self.check_connect(),
            IoState::Done => Poll::Ready(self.result as *mut u8),
        }
    }

    fn try_connect(&mut self) -> Poll {
        // Set non-blocking
        unsafe {
            let flags = libc::fcntl(self.fd, libc::F_GETFL);
            libc::fcntl(self.fd, libc::F_SETFL, flags | libc::O_NONBLOCK);
        }

        let result = unsafe { libc::connect(self.fd, self.addr, self.addr_len) };

        if result == 0 {
            self.result = 0;
            self.state = IoState::Done;
            Poll::Ready(std::ptr::null_mut())
        } else {
            let err = io::Error::last_os_error();
            if err.raw_os_error() == Some(libc::EINPROGRESS) {
                self.state = IoState::Waiting;
                Poll::Pending
            } else {
                self.result = -1;
                self.state = IoState::Done;
                Poll::Ready(-1isize as *mut u8)
            }
        }
    }

    fn check_connect(&mut self) -> Poll {
        // Check if connect completed using getsockopt SO_ERROR
        let mut error: libc::c_int = 0;
        let mut len: libc::socklen_t = std::mem::size_of::<libc::c_int>() as libc::socklen_t;

        let result = unsafe {
            libc::getsockopt(
                self.fd,
                libc::SOL_SOCKET,
                libc::SO_ERROR,
                &mut error as *mut _ as *mut _,
                &mut len,
            )
        };

        if result < 0 {
            self.result = -1;
            self.state = IoState::Done;
            return Poll::Ready(-1isize as *mut u8);
        }

        if error == 0 {
            // Connect succeeded
            self.result = 0;
            self.state = IoState::Done;
            Poll::Ready(std::ptr::null_mut())
        } else if error == libc::EINPROGRESS {
            // Still connecting
            Poll::Pending
        } else {
            // Connect failed
            self.result = -1;
            self.state = IoState::Done;
            Poll::Ready(-1isize as *mut u8)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_from_pipe() {
        // Create a pipe
        let mut fds = [0i32; 2];
        unsafe {
            assert_eq!(libc::pipe(fds.as_mut_ptr()), 0);
        }
        let (read_fd, write_fd) = (fds[0], fds[1]);

        // Write some data
        unsafe {
            libc::write(write_fd, b"hello".as_ptr() as *const _, 5);
        }

        // Create read future
        let mut buf = [0u8; 10];
        let mut future = ReadFuture::new(read_fd, buf.as_mut_ptr(), buf.len());

        // Poll should succeed immediately (data is available)
        let waker = Waker::null();
        let result = future.poll(&waker);

        assert!(result.is_ready());
        if let Poll::Ready(ptr) = result {
            assert_eq!(ptr as isize, 5);
        }
        assert_eq!(&buf[..5], b"hello");

        // Cleanup
        unsafe {
            libc::close(read_fd);
            libc::close(write_fd);
        }
    }

    #[test]
    fn test_write_to_pipe() {
        // Create a pipe
        let mut fds = [0i32; 2];
        unsafe {
            assert_eq!(libc::pipe(fds.as_mut_ptr()), 0);
        }
        let (read_fd, write_fd) = (fds[0], fds[1]);

        // Create write future
        let data = b"hello";
        let mut future = WriteFuture::new(write_fd, data.as_ptr(), data.len());

        // Poll should succeed immediately (pipe has buffer space)
        let waker = Waker::null();
        let result = future.poll(&waker);

        assert!(result.is_ready());
        if let Poll::Ready(ptr) = result {
            assert_eq!(ptr as isize, 5);
        }

        // Read it back
        let mut buf = [0u8; 10];
        let n = unsafe { libc::read(read_fd, buf.as_mut_ptr() as *mut _, buf.len()) };
        assert_eq!(n, 5);
        assert_eq!(&buf[..5], b"hello");

        // Cleanup
        unsafe {
            libc::close(read_fd);
            libc::close(write_fd);
        }
    }
}

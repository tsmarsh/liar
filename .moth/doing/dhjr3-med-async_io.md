# Async I/O for liar

**ID:** async-io
**Priority:** P1 (foundational)
**Estimate:** Large (multiple phases)

## Summary

Implement non-blocking async I/O for liar using:
- Rust runtime with OS-specific reactor (epoll/kqueue/IOCP)
- `Pollable` protocol in liar
- `async`/`await` macro that generates state machines
- Platform-specific I/O library

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│  liar user code                                                 │
│  (async defun fetch (url) (await (http/get url)))              │
├─────────────────────────────────────────────────────────────────┤
│  lib/async.liar              │  lib/io.liar                    │
│  - async macro               │  - read, write, open, close     │
│  - await transform           │  - Built on runtime primitives  │
│  - Pollable protocol         │                                  │
├─────────────────────────────────────────────────────────────────┤
│  liar-runtime (Rust crate, linked into executables)            │
│  ┌─────────────────┐  ┌─────────────────┐  ┌────────────────┐  │
│  │ Executor        │  │ Reactor         │  │ I/O Ops        │  │
│  │ - Task queue    │  │ - epoll.rs      │  │ - file.rs      │  │
│  │ - Poll loop     │  │ - kqueue.rs     │  │ - net.rs       │  │
│  │ - Work stealing │  │ - iocp.rs       │  │ - timer.rs     │  │
│  └─────────────────┘  └─────────────────┘  └────────────────┘  │
└─────────────────────────────────────────────────────────────────┘
```

## Prerequisites

Before starting this work:

### P0: Struct instantiation + field access (CRITICAL)
- [ ] `(Point 10 20)` allocates and returns struct pointer
- [ ] `(. p x)` does GEP + load
- **Location:** `liar/src/codegen.rs`
- **Test:** `(let ((p (Point 1 2))) (. p x))` → `1`

### P0: Macros (CRITICAL)
- [ ] `defmacro` parses and registers macro
- [ ] Quasiquote: `` `(a ,x ,@xs) ``
- [ ] `gensym` for hygienic names
- [ ] Macro expansion before type inference
- **Location:** `liar/src/macros.rs` (new), `liar/src/expand.rs` (new)
- **Test:** `(defmacro when (c &rest body) `(if ,c (do ,@body) nil))`

### P0: Protocols dispatch (CRITICAL)
- [ ] `extend-protocol` generates dispatch table
- [ ] Protocol method calls resolve at runtime
- **Location:** `liar/src/codegen.rs`, runtime dispatch
- **Test:** `(extend-protocol Foo Bar (method [self] 42))` then `(method (Bar))` → `42`

---

## Phase 1: Rust Runtime Foundation

**Location:** `liar-runtime/` (new crate)

### 1.1 Crate Setup

```
liar-runtime/
├── Cargo.toml
├── src/
│   ├── lib.rs           # Public API exposed to liar
│   ├── executor.rs      # Task executor
│   ├── task.rs          # Task representation
│   ├── waker.rs         # Waker implementation
│   ├── reactor/
│   │   ├── mod.rs       # Reactor trait
│   │   ├── epoll.rs     # Linux
│   │   ├── kqueue.rs    # macOS/BSD
│   │   └── iocp.rs      # Windows (stub initially)
│   └── io/
│       ├── mod.rs
│       ├── file.rs      # Async file ops
│       ├── net.rs       # Async sockets (later)
│       └── timer.rs     # Async timers
└── tests/
    ├── executor_test.rs
    ├── reactor_test.rs
    └── io_test.rs
```

### 1.2 Core Types (Rust)

```rust
// src/task.rs

/// Task state - mirrors what the liar macro generates
#[repr(C)]
pub struct Task {
    /// Pointer to the Pollable liar struct
    pub pollable: *mut u8,
    /// Function pointer: fn(pollable, waker) -> Poll
    pub poll_fn: extern "C" fn(*mut u8, *mut Waker) -> Poll,
    /// Intrusive linked list for task queue
    pub next: *mut Task,
    /// State: Pending, Ready, Complete
    pub state: TaskState,
}

#[repr(C)]
pub enum Poll {
    Ready(*mut u8),  // Pointer to result
    Pending,
}

#[repr(C)]
pub enum TaskState {
    Pending,
    Runnable,
    Complete,
}
```

```rust
// src/waker.rs

/// Waker that can reschedule a task
#[repr(C)]
pub struct Waker {
    /// Pointer back to the task
    task: *mut Task,
    /// Pointer to the executor's wake queue
    wake_queue: *mut WakeQueue,
}

impl Waker {
    /// Called by reactor when I/O is ready
    pub extern "C" fn wake(&self) {
        // Add task to executor's runnable queue
        unsafe {
            (*self.wake_queue).push(self.task);
        }
    }
}
```

### 1.3 Executor

```rust
// src/executor.rs

pub struct Executor {
    /// Tasks ready to be polled
    run_queue: VecDeque<*mut Task>,
    /// Thread pool for compute tasks
    thread_pool: ThreadPool,
    /// The I/O reactor
    reactor: Box<dyn Reactor>,
}

impl Executor {
    /// Run until all tasks complete
    pub fn block_on(&mut self, root_task: *mut Task) -> *mut u8 {
        self.run_queue.push_back(root_task);
        
        loop {
            // Poll all runnable tasks
            while let Some(task) = self.run_queue.pop_front() {
                let waker = Waker::new(task, &mut self.wake_queue);
                let poll_fn = unsafe { (*task).poll_fn };
                let pollable = unsafe { (*task).pollable };
                
                match poll_fn(pollable, &mut waker) {
                    Poll::Ready(result) => {
                        if task == root_task {
                            return result;
                        }
                        // Task complete, clean up
                    }
                    Poll::Pending => {
                        // Task parked, will be woken by reactor
                    }
                }
            }
            
            // No runnable tasks - wait for I/O
            self.reactor.wait(&mut self.wake_queue);
        }
    }
}
```

### 1.4 Reactor Trait

```rust
// src/reactor/mod.rs

pub trait Reactor: Send {
    /// Register interest in fd becoming readable
    fn register_read(&mut self, fd: RawFd, waker: Waker);
    
    /// Register interest in fd becoming writable
    fn register_write(&mut self, fd: RawFd, waker: Waker);
    
    /// Deregister interest
    fn deregister(&mut self, fd: RawFd);
    
    /// Wait for events, wake relevant tasks
    fn wait(&mut self, wake_queue: &mut WakeQueue);
}
```

### 1.5 epoll Reactor (Linux)

```rust
// src/reactor/epoll.rs

use std::os::unix::io::RawFd;
use libc::{epoll_create1, epoll_ctl, epoll_wait, EPOLLIN, EPOLLOUT};

pub struct EpollReactor {
    epoll_fd: RawFd,
    events: Vec<libc::epoll_event>,
    registered: HashMap<RawFd, Waker>,
}

impl Reactor for EpollReactor {
    fn register_read(&mut self, fd: RawFd, waker: Waker) {
        let mut event = libc::epoll_event {
            events: EPOLLIN as u32,
            u64: fd as u64,
        };
        unsafe {
            epoll_ctl(self.epoll_fd, libc::EPOLL_CTL_ADD, fd, &mut event);
        }
        self.registered.insert(fd, waker);
    }
    
    fn wait(&mut self, wake_queue: &mut WakeQueue) {
        let n = unsafe {
            epoll_wait(
                self.epoll_fd,
                self.events.as_mut_ptr(),
                self.events.len() as i32,
                -1,  // Block indefinitely
            )
        };
        
        for i in 0..n as usize {
            let fd = self.events[i].u64 as RawFd;
            if let Some(waker) = self.registered.get(&fd) {
                waker.wake();
            }
        }
    }
}
```

### 1.6 kqueue Reactor (macOS)

```rust
// src/reactor/kqueue.rs

use libc::{kqueue, kevent, EV_ADD, EVFILT_READ, EVFILT_WRITE};

pub struct KqueueReactor {
    kq: RawFd,
    events: Vec<libc::kevent>,
    registered: HashMap<RawFd, Waker>,
}

impl Reactor for KqueueReactor {
    fn register_read(&mut self, fd: RawFd, waker: Waker) {
        let event = libc::kevent {
            ident: fd as usize,
            filter: EVFILT_READ,
            flags: EV_ADD,
            fflags: 0,
            data: 0,
            udata: std::ptr::null_mut(),
        };
        unsafe {
            kevent(self.kq, &event, 1, std::ptr::null_mut(), 0, std::ptr::null());
        }
        self.registered.insert(fd, waker);
    }
    
    // ... similar to epoll
}
```

### 1.7 Tests (Rust)

```rust
// tests/executor_test.rs

#[test]
fn test_simple_task() {
    // Create a task that immediately returns
    let task = create_immediate_task(42);
    let mut executor = Executor::new();
    let result = executor.block_on(task);
    assert_eq!(result, 42);
}

#[test]
fn test_chained_tasks() {
    // Task A -> Task B -> return
    let task = create_chain_task();
    let mut executor = Executor::new();
    let result = executor.block_on(task);
    assert_eq!(result, expected);
}

// tests/reactor_test.rs

#[test]
fn test_epoll_read_ready() {
    let (read_fd, write_fd) = pipe();
    let mut reactor = EpollReactor::new();
    let waker = TestWaker::new();
    
    reactor.register_read(read_fd, waker.clone());
    
    // Write to pipe
    write(write_fd, b"hello");
    
    // Reactor should wake our waker
    reactor.wait(&mut wake_queue);
    assert!(waker.was_woken());
}

// tests/io_test.rs

#[test]
fn test_async_file_read() {
    let mut executor = Executor::new();
    let task = async_read_file("/tmp/test.txt");
    let result = executor.block_on(task);
    assert_eq!(result, b"file contents");
}
```

---

## Phase 2: FFI Bridge

**Location:** `liar-runtime/src/ffi.rs`, `liar/src/codegen.rs`

### 2.1 Expose Runtime to liar

The liar compiler needs to emit calls to the runtime. Define a stable FFI:

```rust
// src/ffi.rs - Functions callable from liar

/// Spawn a task onto the executor
#[no_mangle]
pub extern "C" fn liar_spawn(pollable: *mut u8, poll_fn: PollFn) -> *mut Task {
    EXECUTOR.with(|exec| {
        exec.borrow_mut().spawn(pollable, poll_fn)
    })
}

/// Block until task completes (entry point)
#[no_mangle]
pub extern "C" fn liar_block_on(task: *mut Task) -> *mut u8 {
    EXECUTOR.with(|exec| {
        exec.borrow_mut().block_on(task)
    })
}

/// Register read interest for fd, returns Future handle
#[no_mangle]
pub extern "C" fn liar_io_read(fd: i64, buf: *mut u8, len: i64) -> *mut IoFuture {
    IoFuture::read(fd as RawFd, buf, len as usize)
}

/// Poll an I/O future
#[no_mangle]
pub extern "C" fn liar_io_poll(future: *mut IoFuture, waker: *mut Waker) -> Poll {
    unsafe { (*future).poll(waker) }
}
```

### 2.2 Link Runtime into Executables

```rust
// lir-lair/src/main.rs (or wherever linking happens)

// Link liar-runtime when building executables
fn link_runtime(module: &Module) {
    // Add runtime library to linker invocation
    // -lliar_runtime
}
```

### 2.3 Tests

```rust
// Test FFI boundary
#[test]
fn test_ffi_spawn_poll() {
    // Simulate what liar codegen produces
    let pollable = create_test_pollable();
    let poll_fn = test_poll_fn as PollFn;
    
    let task = liar_spawn(pollable, poll_fn);
    let result = liar_block_on(task);
    
    assert_eq!(result, expected);
}
```

---

## Phase 3: Pollable Protocol in liar

**Location:** `lib/async.liar`

### 3.1 Core Protocol

```lisp
;; lib/async.liar

;; The fundamental async abstraction
(defprotocol Pollable
  "A value that can be polled for completion."
  (poll [self waker] "Returns :pending or (:ready value)"))

;; Result type for poll
(defenum PollResult
  (Ready value)
  Pending)
```

### 3.2 Basic Combinators (No Macros Yet)

```lisp
;; Chain two pollables: run first, then apply f to result
(defstruct ThenFuture
  first: ptr
  f: ptr
  state: i64      ;; 0 = polling first, 1 = polling second
  second: ptr)    ;; created after first completes

(extend-protocol Pollable ThenFuture
  (poll [self waker]
    (match (.state self)
      (0 (match (poll (.first self) waker)
           ((Ready v) 
             (do
               (set! (.second self) ((.f self) v))
               (set! (.state self) 1)
               (poll self waker)))  ;; immediately poll second
           (Pending Pending)))
      (1 (poll (.second self) waker)))))

(defun then (future f)
  (ThenFuture future f 0 nil))


;; Map a function over a future's result
(defstruct MapFuture
  inner: ptr
  f: ptr)

(extend-protocol Pollable MapFuture
  (poll [self waker]
    (match (poll (.inner self) waker)
      ((Ready v) (Ready ((.f self) v)))
      (Pending Pending))))

(defun fmap (f future)
  (MapFuture future f))


;; Join two futures, wait for both
(defstruct JoinFuture
  a: ptr
  b: ptr
  a-result: ptr
  b-result: ptr
  a-done: i64
  b-done: i64)

(extend-protocol Pollable JoinFuture
  (poll [self waker]
    (if (= 0 (.a-done self))
        (match (poll (.a self) waker)
          ((Ready v) (do (set! (.a-result self) v)
                         (set! (.a-done self) 1)))
          (Pending nil)))
    (if (= 0 (.b-done self))
        (match (poll (.b self) waker)
          ((Ready v) (do (set! (.b-result self) v)
                         (set! (.b-done self) 1)))
          (Pending nil)))
    (if (and (= 1 (.a-done self)) (= 1 (.b-done self)))
        (Ready [(.a-result self) (.b-result self)])
        Pending)))

(defun join (a b)
  (JoinFuture a b nil nil 0 0))
```

### 3.3 Tests

```lisp
;; Test in REPL or cert/

;; Immediate future (for testing)
(defstruct ImmediateFuture (value: i64))
(extend-protocol Pollable ImmediateFuture
  (poll [self waker] (Ready (.value self))))

(defun immediate (v) (ImmediateFuture v))

;; Test then
(let ((f (then (immediate 5) inc)))
  (poll f nil))  ;; => (Ready 6)

;; Test join
(let ((f (join (immediate 1) (immediate 2))))
  (poll f nil))  ;; => (Ready [1 2])

;; Test fmap
(let ((f (fmap square (immediate 4))))
  (poll f nil))  ;; => (Ready 16)
```

---

## Phase 4: I/O Futures

**Location:** `lib/io.liar`, calls into `liar-runtime`

### 4.1 Primitive I/O Futures

```lisp
;; lib/io.liar

;; FFI declarations (once we have extern in liar)
(extern liar_io_read (i64 ptr i64) ptr)
(extern liar_io_poll (ptr ptr) PollResult)

;; Read future - wraps runtime IoFuture
(defstruct ReadFuture
  handle: ptr)  ;; Opaque pointer to Rust IoFuture

(extend-protocol Pollable ReadFuture
  (poll [self waker]
    (liar_io_poll (.handle self) waker)))

(defun read-async (fd buf len)
  (ReadFuture (liar_io_read fd buf len)))


;; Similar for write, open, close...
(defstruct WriteFuture (handle: ptr))
(defstruct OpenFuture (handle: ptr))

;; High-level convenience
(defun slurp-async (path)
  (then (open-async path :read)
    (fn (fd)
      (then (read-all-async fd)
        (fn (contents)
          (then (close-async fd)
            (fn (_) (immediate contents))))))))
```

### 4.2 Tests

```lisp
;; Write actual async I/O test
;; (requires runtime linked)

(defun test-read-file ()
  (let ((task (spawn (slurp-async "/tmp/test.txt"))))
    (block-on task)))

;; BDD feature
;; cert/features/async_io.feature

Feature: Async I/O
  
  Scenario: Read file asynchronously
    Given a file "/tmp/async-test.txt" containing "hello async"
    When I evaluate:
      """
      (block-on (slurp-async "/tmp/async-test.txt"))
      """
    Then the result should be "hello async"
  
  Scenario: Concurrent reads
    Given files "/tmp/a.txt" and "/tmp/b.txt"
    When I evaluate:
      """
      (block-on 
        (fmap (fn (pair) (+ (first pair) (second pair)))
          (join (slurp-async "/tmp/a.txt")
                (slurp-async "/tmp/b.txt"))))
      """
    Then both files are read concurrently
```

---

## Phase 5: The async/await Macro

**Location:** `lib/async.liar`

### 5.1 The Transform

```lisp
;; The async macro transforms a function into a state machine

(defmacro async (defun-form)
  (let ((name (second defun-form))
        (params (third defun-form))
        (body (fourth defun-form)))
    (let ((states (analyze-awaits body))
          (struct-name (gensym (str name "-State")))
          (fields (extract-locals body)))
      `(do
         ;; Generate state struct
         (defstruct ,struct-name
           state: i64
           ,@(map (fn (f) `(,f ptr)) fields))
         
         ;; Generate Pollable implementation
         (extend-protocol Pollable ,struct-name
           (poll [self waker]
             ,(generate-state-machine states)))
         
         ;; Generate constructor function
         (defun ,name ,params
           (,struct-name 0 ,@(map (fn (f) `nil) fields)))))))

;; Helper: find all await points, assign state numbers
(defun analyze-awaits (body)
  ;; Returns list of (state-num before-code await-expr after-cont)
  ...)

;; Helper: generate match over states
(defun generate-state-machine (states)
  `(match (.state self)
     ,@(map generate-state-arm states)))

(defun generate-state-arm (state)
  (let ((n (first state))
        (setup (second state))
        (await-expr (third state))
        (next-state (fourth state)))
    `(,n (do
           ,@setup
           (match (poll ,await-expr waker)
             ((Ready v) (do
               (set! (.state self) ,next-state)
               (poll self waker)))  ;; Tail call to next state
             (Pending Pending))))))
```

### 5.2 Usage

```lisp
;; User writes:
(async defun fetch-both (a b)
  (let ((x (await (fetch a)))
        (y (await (fetch b))))
    (+ x y)))

;; Macro expands to state machine struct + Pollable impl

;; Then can use:
(block-on (fetch-both "http://a.com" "http://b.com"))
```

### 5.3 Tests

```lisp
;; Test macro expansion

(defun test-async-macro-expansion ()
  (let ((expanded (macroexpand-1 
                    '(async defun simple (x)
                       (await (immediate x))))))
    (assert (has-defstruct? expanded))
    (assert (has-extend-protocol? expanded))))

;; Test actual execution

Feature: async/await
  
  Scenario: Simple await
    When I evaluate:
      """
      (async defun add-one (x)
        (+ 1 (await (immediate x))))
      (block-on (add-one 5))
      """
    Then the result should be 6
  
  Scenario: Sequential awaits
    When I evaluate:
      """
      (async defun add-both (a b)
        (let ((x (await (immediate a)))
              (y (await (immediate b))))
          (+ x y)))
      (block-on (add-both 3 4))
      """
    Then the result should be 7
  
  Scenario: Nested async
    When I evaluate:
      """
      (async defun outer (x)
        (await (inner x)))
      (async defun inner (x)
        (await (immediate (* x 2))))
      (block-on (outer 5))
      """
    Then the result should be 10
```

---

## Phase 6: Platform-Specific I/O Library

**Location:** `lib/io/`, platform libs

### 6.1 Structure

```
lib/
├── async.liar           # Core async, Pollable, combinators, macro
├── io.liar              # Platform-agnostic I/O API
└── io/
    ├── posix.liar       # Linux + macOS implementation
    └── windows.liar     # Windows implementation (later)
```

### 6.2 Platform-Agnostic API

```lisp
;; lib/io.liar

;; This is what users import

(defprotocol AsyncRead
  (read-async [self buf] "Read into buffer, return future"))

(defprotocol AsyncWrite
  (write-async [self data] "Write data, return future"))

;; High-level functions
(defun slurp [path]
  "Read entire file as string"
  (block-on (slurp-async path)))

(defun spit [path data]
  "Write string to file"
  (block-on (spit-async path data)))

;; Load platform-specific implementation
(cond
  (= *platform* :linux)  (load "io/posix.liar")
  (= *platform* :macos)  (load "io/posix.liar")
  (= *platform* :windows) (load "io/windows.liar"))
```

### 6.3 POSIX Implementation

```lisp
;; lib/io/posix.liar

;; File handle wrapper
(defstruct PosixFile
  fd: i64)

(extend-protocol AsyncRead PosixFile
  (read-async [self buf]
    (ReadFuture (liar_io_read (.fd self) buf (len buf)))))

(extend-protocol AsyncWrite PosixFile
  (write-async [self data]
    (WriteFuture (liar_io_write (.fd self) data (len data)))))

;; Open file
(defun open-async [path mode]
  (then (OpenFuture (liar_io_open path mode))
    (fn (fd) (immediate (PosixFile fd)))))

;; Close file
(defun close-async [file]
  (CloseFuture (liar_io_close (.fd file))))
```

---

## Dependency Graph

```
Phase 0 (Prerequisites):
  ┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
  │ Struct codegen  │     │ Macros          │     │ Protocol        │
  │ (. p x) works   │     │ defmacro        │     │ dispatch        │
  └────────┬────────┘     └────────┬────────┘     └────────┬────────┘
           │                       │                       │
           └───────────────────────┼───────────────────────┘
                                   │
                                   ▼
Phase 1:                 ┌─────────────────────┐
                         │ Rust Runtime        │
                         │ executor + reactor  │
                         └──────────┬──────────┘
                                    │
                                    ▼
Phase 2:                 ┌─────────────────────┐
                         │ FFI Bridge          │
                         │ liar ↔ runtime      │
                         └──────────┬──────────┘
                                    │
                                    ▼
Phase 3:                 ┌─────────────────────┐
                         │ Pollable protocol   │
                         │ then, join, fmap    │
                         └──────────┬──────────┘
                                    │
                                    ▼
Phase 4:                 ┌─────────────────────┐
                         │ I/O Futures         │
                         │ read, write, open   │
                         └──────────┬──────────┘
                                    │
                                    ▼
Phase 5:                 ┌─────────────────────┐
                         │ async/await macro   │
                         │ state machine xform │
                         └──────────┬──────────┘
                                    │
                                    ▼
Phase 6:                 ┌─────────────────────┐
                         │ Platform I/O libs   │
                         │ posix, windows      │
                         └─────────────────────┘
```

---

## Acceptance Criteria

### Phase 0
- [ ] `(let ((p (Point 1 2))) (. p x))` returns `1`
- [ ] `(defmacro when (c b) \`(if ,c ,b nil))` works
- [ ] `(extend-protocol P T (m [self] 1))` then `(m (T))` returns `1`

### Phase 1
- [ ] Rust executor can poll tasks to completion
- [ ] epoll reactor wakes tasks when fd is ready (Linux)
- [ ] kqueue reactor wakes tasks when fd is ready (macOS)
- [ ] All Rust tests pass

### Phase 2
- [ ] liar can call `liar_spawn`, `liar_block_on`
- [ ] liar can call `liar_io_read`, `liar_io_poll`
- [ ] Runtime links correctly into liar executables

### Phase 3
- [ ] `Pollable` protocol defined
- [ ] `then`, `join`, `fmap` combinators work
- [ ] `(poll (immediate 5) nil)` returns `(Ready 5)`

### Phase 4
- [ ] `(block-on (read-async fd buf len))` reads from fd
- [ ] `(block-on (slurp-async "/etc/hostname"))` returns file contents
- [ ] Concurrent reads actually run concurrently

### Phase 5
- [ ] `(async defun f (x) (await (immediate x)))` compiles
- [ ] Generated state machine is correct
- [ ] `(block-on (f 5))` returns `5`

### Phase 6
- [ ] `(slurp "/etc/hostname")` works on Linux
- [ ] `(slurp "/etc/hostname")` works on macOS
- [ ] Platform detection works

---

## Open Questions

1. **Thread pool size**: Fixed? Work-stealing? Configurable?

2. **Cancellation**: How do we cancel a running task?

3. **Timeouts**: `(with-timeout 5000 (slurp-async url))`?

4. **Error handling**: How do I/O errors propagate through futures?

5. **Backpressure**: What happens when tasks spawn faster than complete?

6. **Blocking escape hatch**: `(spawn-blocking (fn () (slow-sync-op)))`?

---

## Estimated Effort

| Phase | Effort | Parallel? |
|-------|--------|-----------|
| P0: Prerequisites | 2-3 weeks | Some |
| P1: Rust Runtime | 1-2 weeks | After P0 |
| P2: FFI Bridge | 3-5 days | After P1 |
| P3: Pollable | 1 week | After P0 |
| P4: I/O Futures | 1 week | After P2+P3 |
| P5: async macro | 1-2 weeks | After P3+macros |
| P6: Platform libs | 1 week | After P4+P5 |

**Total: 8-12 weeks** (with prerequisites)

Without macros: Can stop at Phase 4 and use combinators manually.

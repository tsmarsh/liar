# Async I/O Library for liar

## Summary

Create `lib/io.liar` — a comprehensive async I/O library built on the existing `liar-runtime` async primitives. All I/O is non-blocking and integrates with the executor. Provides both low-level async operations and high-level conveniences like `slurp`/`spit`.

## Motivation

Bootstrap requires reading source files and writing output. Beyond bootstrap, a language needs comprehensive I/O.

**Key principle:** All I/O in liar is async. Even "synchronous-looking" operations like `slurp` are implemented as async operations that block the current task until completion. This keeps the runtime model consistent and allows future concurrent I/O.

## Existing Infrastructure

The `liar-runtime` crate provides:

```lisp
;; From lib/runtime.liar - already declared
(extern liar_runtime_init i32 ())
(extern liar_spawn ptr (ptr ptr))
(extern liar_block_on ptr (ptr))
(extern liar_io_read ptr (i64 ptr i64))      ; -> ReadFuture*
(extern liar_io_read_poll ptr (ptr ptr))     ; -> Poll
(extern liar_io_read_free void (ptr))
(extern liar_io_write ptr (i64 ptr i64))     ; -> WriteFuture*
(extern liar_io_write_poll ptr (ptr ptr))    ; -> Poll
(extern liar_io_write_free void (ptr))
(extern liar_reactor_register_read i32 (i64 ptr))
(extern liar_reactor_register_write i32 (i64 ptr))
(extern liar_reactor_deregister i32 (i64))
```

## Design

### Async Model

I/O operations return `Future` values. Use `await` (or `block-on` for synchronous contexts) to get results:

```lisp
;; Async style (inside async context)
(let ((bytes (await (read-async fd buf len))))
  ...)

;; Blocking style (wraps in spawn + block_on)
(let ((bytes (read-blocking fd buf len)))
  ...)
```

For bootstrap, we primarily use blocking wrappers. The async primitives exist for future concurrent I/O.

### File Descriptors

Unlike the sync version, we work with raw file descriptors (i32/i64) since that's what the runtime provides. File open/close still use libc:

```lisp
(extern open i32 (ptr i32 i32))     ; open(path, flags, mode) -> fd
(extern close i32 (i32))            ; close(fd) -> status
(extern lseek i64 (i32 i64 i32))    ; lseek(fd, offset, whence) -> position
(extern fstat i32 (i32 ptr))        ; fstat(fd, stat_buf) -> status
```

## Implementation

```lisp
;; lib/io.liar - Async I/O Library

;;; ============================================
;;; Additional libc FFI (beyond runtime.liar)
;;; ============================================

;; File open/close (these are sync syscalls, that's fine)
(extern open i32 (ptr i32 i32))
(extern close i32 (i32))
(extern lseek i64 (i32 i64 i32))

;; Memory
(extern malloc ptr (i64))
(extern free void (ptr))

;; String ops
(extern strlen i64 (ptr))

;; Error handling
(extern perror void (ptr))


;;; ============================================
;;; Constants
;;; ============================================

(def SEEK_SET 0)
(def SEEK_CUR 1)
(def SEEK_END 2)

(def STDIN_FD 0)
(def STDOUT_FD 1)
(def STDERR_FD 2)

(def O_RDONLY 0)
(def O_WRONLY 1)
(def O_RDWR 2)
(def O_CREAT 64)
(def O_TRUNC 512)
(def O_APPEND 1024)


;;; ============================================
;;; Low-Level Async Operations
;;; ============================================

(defun read-async (fd buf len)
  "Start an async read. Returns a ReadFuture."
  (liar_io_read fd buf len))

(defun write-async (fd buf len)
  "Start an async write. Returns a WriteFuture."
  (liar_io_write fd buf len))

(defun poll-read (future waker)
  "Poll a read future. Returns bytes-read or :pending."
  (liar_io_read_poll future waker))

(defun poll-write (future waker)
  "Poll a write future. Returns bytes-written or :pending."
  (liar_io_write_poll future waker))

(defun free-read-future (future)
  (liar_io_read_free future))

(defun free-write-future (future)
  (liar_io_write_free future))


;;; ============================================
;;; Blocking Wrappers (for bootstrap simplicity)
;;; ============================================

(defun read-blocking (fd buf len)
  "Read from fd, blocking until complete. Returns bytes read or -1."
  (let ((future (read-async fd buf len)))
    ;; Simple spin-poll for now
    ;; TODO: integrate with executor properly
    (let ((result (poll-until-ready future poll-read)))
      (free-read-future future)
      result)))

(defun write-blocking (fd buf len)
  "Write to fd, blocking until complete. Returns bytes written or -1."
  (let ((future (write-async fd buf len)))
    (let ((result (poll-until-ready future poll-write)))
      (free-write-future future)
      result)))

(defun poll-until-ready (future poll-fn)
  "Spin-poll until future is ready. Returns result."
  (let ((result (poll-fn future nil)))  ; nil waker for now
    (if (pending? result)
        (poll-until-ready future poll-fn)  ; tail-recursive spin
        result)))

(defun pending? (poll-result)
  "Check if poll result is Pending (implementation-specific)."
  ;; Poll::Pending is represented as a specific sentinel value
  ;; Need to check liar-runtime for exact representation
  ;; For now, assume non-negative = ready with byte count
  (< poll-result 0))


;;; ============================================
;;; File Operations
;;; ============================================

(defun file-open (path flags mode)
  "Open a file. Returns fd or -1 on error."
  (open path flags mode))

(defun file-open-read (path)
  "Open file for reading."
  (file-open path O_RDONLY 0))

(defun file-open-write (path)
  "Open file for writing (create/truncate)."
  (file-open path (bit-or O_WRONLY (bit-or O_CREAT O_TRUNC)) 420))  ; 0644

(defun file-open-append (path)
  "Open file for appending."
  (file-open path (bit-or O_WRONLY (bit-or O_CREAT O_APPEND)) 420))

(defun file-close (fd)
  "Close a file descriptor."
  (close fd))

(defun file-size (fd)
  "Get file size by seeking."
  (let ((pos (lseek fd 0 SEEK_CUR)))
    (let ((size (lseek fd 0 SEEK_END)))
      (lseek fd pos SEEK_SET)
      size)))


;;; ============================================
;;; High-Level Conveniences
;;; ============================================

(defun slurp (path)
  "Read entire file as string. Returns string or nil on error.
   
   This is a blocking operation - the current task waits until
   the entire file is read."
  (let ((fd (file-open-read path)))
    (if (< fd 0)
        nil
        (let ((size (file-size fd)))
          (let ((buf (malloc (+ size 1))))  ; +1 for null terminator
            (if (nil? buf)
                (do (file-close fd) nil)
                (do
                  (lseek fd 0 SEEK_SET)
                  (let ((bytes-read (read-all-blocking fd buf size)))
                    (if (< bytes-read 0)
                        (do (free buf) (file-close fd) nil)
                        (do
                          (store-byte (+ buf bytes-read) 0)  ; null terminate
                          (file-close fd)
                          buf))))))))))

(defun read-all-blocking (fd buf remaining)
  "Read all bytes, handling partial reads."
  (read-all-loop fd buf 0 remaining))

(defun read-all-loop (fd buf offset remaining)
  (if (<= remaining 0)
      offset
      (let ((n (read-blocking fd (+ buf offset) remaining)))
        (cond
          ((< n 0) -1)                           ; error
          ((= n 0) offset)                       ; EOF
          (true (read-all-loop fd buf 
                               (+ offset n) 
                               (- remaining n)))))))

(defun spit (path contents)
  "Write string to file. Returns bytes written or -1.
   
   Blocking operation."
  (let ((fd (file-open-write path)))
    (if (< fd 0)
        -1
        (let ((len (strlen contents)))
          (let ((written (write-all-blocking fd contents len)))
            (file-close fd)
            written)))))

(defun write-all-blocking (fd buf remaining)
  "Write all bytes, handling partial writes."
  (write-all-loop fd buf 0 remaining))

(defun write-all-loop (fd buf offset remaining)
  (if (<= remaining 0)
      offset
      (let ((n (write-blocking fd (+ buf offset) remaining)))
        (if (<= n 0)
            -1  ; error or can't write
            (write-all-loop fd buf
                            (+ offset n)
                            (- remaining n))))))

(defun spit-append (path contents)
  "Append string to file."
  (let ((fd (file-open-append path)))
    (if (< fd 0)
        -1
        (let ((len (strlen contents)))
          (let ((written (write-all-blocking fd contents len)))
            (file-close fd)
            written)))))


;;; ============================================
;;; Standard Streams
;;; ============================================

(defun print-to-fd (fd s)
  "Write string to fd."
  (write-blocking fd s (strlen s)))

(defun println-to-fd (fd s)
  "Write string + newline to fd."
  (print-to-fd fd s)
  (write-blocking fd "\n" 1))

(defun eprint (s)
  "Print to stderr."
  (print-to-fd STDERR_FD s))

(defun eprintln (s)
  "Print to stderr with newline."
  (println-to-fd STDERR_FD s))


;;; ============================================
;;; Line Reading (async-aware)
;;; ============================================

(defun read-line (fd buf max-len)
  "Read a line from fd into buf. Returns bytes read.
   
   Reads one byte at a time (inefficient but correct).
   Stops at newline or max-len."
  (read-line-loop fd buf 0 max-len))

(defun read-line-loop (fd buf pos max-len)
  (if (>= pos max-len)
      pos
      (let ((n (read-blocking fd (+ buf pos) 1)))
        (cond
          ((<= n 0) pos)  ; EOF or error
          ((= (load-byte (+ buf pos)) 10)  ; newline
           (+ pos 1))
          (true (read-line-loop fd buf (+ pos 1) max-len))))))


;;; ============================================
;;; Future: True Async API
;;; ============================================

;; These will be used when we have proper async/await syntax:

;; (defasync slurp-async (path)
;;   "Async version of slurp."
;;   (let ((fd (file-open-read path)))
;;     (if (< fd 0)
;;         nil
;;         (let ((size (file-size fd)))
;;           (let ((buf (malloc (+ size 1))))
;;             (lseek fd 0 SEEK_SET)
;;             (await (read-all-async fd buf size))
;;             (store-byte (+ buf size) 0)
;;             (file-close fd)
;;             buf)))))
```

## Missing Primitives

From liar side, we need:

1. **`store-byte` / `load-byte`** — for null-terminating strings
2. **`bit-or`** — for combining open flags
3. **`nil?`** — null pointer check

These are small additions to `builtins.rs`.

## Runtime Integration

The current `poll-until-ready` is a spin-loop, which works but wastes CPU. Proper integration:

```lisp
;; Better: yield to executor
(defun poll-until-ready (future poll-fn)
  (let ((result (poll-fn future (current-waker))))
    (if (pending? result)
        (do (yield)  ; give other tasks a chance
            (poll-until-ready future poll-fn))
        result)))
```

This requires exposing `yield` from the executor. For bootstrap, spin-polling is acceptable.

## Acceptance Criteria

### Async Primitives
- [ ] `read-async` / `write-async` create futures
- [ ] `poll-read` / `poll-write` poll futures
- [ ] Integration with `liar-runtime` works

### Blocking Wrappers
- [ ] `read-blocking` reads and blocks
- [ ] `write-blocking` writes and blocks
- [ ] Partial read/write handling works

### High-Level Functions
- [ ] `slurp` reads entire file
- [ ] `spit` writes string to file
- [ ] `spit-append` appends to file

### Standard Streams
- [ ] `eprint` / `eprintln` work
- [ ] Can read from stdin

## Test Scenarios

```gherkin
Feature: Async I/O Library

  Scenario: slurp reads file
    Given I write "hello world" to "/tmp/test-slurp.txt"
    When I evaluate (slurp "/tmp/test-slurp.txt")
    Then the result should be "hello world"

  Scenario: spit writes file
    When I evaluate:
      """
      (do
        (spit "/tmp/test-spit.txt" "test content")
        (slurp "/tmp/test-spit.txt"))
      """
    Then the result should be "test content"

  Scenario: read/write with partial operations
    Given a file larger than one read buffer
    When I slurp it
    Then I should get the complete contents

  Scenario: async read from pipe
    Given a pipe with data "hello"
    When I evaluate:
      """
      (let ((future (read-async read-fd buf 10)))
        (poll-until-ready future poll-read))
      """
    Then the result should be 5
```

## File Structure

```
lib/
  runtime.liar     ; Existing async runtime FFI declarations
  io.liar          ; This file - I/O library
  
liar-cert/features/
  io_async.feature     ; Async operation tests
  io_slurp_spit.feature
```

## Notes

The design keeps all I/O async-native while providing blocking wrappers for convenience. This means:

1. When we add `async/await` syntax, the primitives are already there
2. Concurrent I/O (multiple files, network) will "just work"
3. The blocking wrappers are thin layers over the async core

For bootstrap, we use the blocking wrappers exclusively. The async infrastructure pays off when we want to read multiple source files concurrently or add network I/O.

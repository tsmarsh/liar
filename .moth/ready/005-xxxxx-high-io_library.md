# Comprehensive I/O Library for liar

## Summary

Create `lib/io.liar` — a complete I/O library built on libc FFI. This provides file operations, standard streams, and the `slurp`/`spit` conveniences needed for bootstrap (reading source files, writing output).

## Motivation

Bootstrap requires:
- Reading source files (`slurp`)
- Writing compiled output (`spit`)
- Error messages to stderr

Beyond bootstrap, a language needs comprehensive I/O for real programs.

## Approach

All I/O is built on libc via `extern`. No new lIR primitives needed — just wire up the C standard library.

## FFI Declarations

```lisp
;; lib/io.liar - I/O Library

;;; ============================================
;;; libc FFI declarations
;;; ============================================

;; File operations
(extern fopen ptr (ptr ptr))        ; fopen(path, mode) -> FILE*
(extern fclose i32 (ptr))           ; fclose(FILE*) -> status
(extern fread i64 (ptr i64 i64 ptr))  ; fread(buf, size, count, FILE*) -> items read
(extern fwrite i64 (ptr i64 i64 ptr)) ; fwrite(buf, size, count, FILE*) -> items written
(extern fseek i32 (ptr i64 i32))    ; fseek(FILE*, offset, whence) -> status
(extern ftell i64 (ptr))            ; ftell(FILE*) -> position
(extern rewind void (ptr))          ; rewind(FILE*)
(extern fflush i32 (ptr))           ; fflush(FILE*) -> status
(extern feof i32 (ptr))             ; feof(FILE*) -> bool
(extern ferror i32 (ptr))           ; ferror(FILE*) -> bool
(extern fgetc i32 (ptr))            ; fgetc(FILE*) -> char or EOF
(extern fputc i32 (i32 ptr))        ; fputc(char, FILE*) -> char or EOF
(extern fgets ptr (ptr i32 ptr))    ; fgets(buf, size, FILE*) -> buf or null
(extern fputs i32 (ptr ptr))        ; fputs(str, FILE*) -> status
(extern ungetc i32 (i32 ptr))       ; ungetc(char, FILE*) -> char or EOF

;; Standard streams (these are globals, need special handling)
;; For now, use fileno + fdopen pattern or hardcode fd 0,1,2
(extern fdopen ptr (i32 ptr))       ; fdopen(fd, mode) -> FILE*
(extern fileno i32 (ptr))           ; fileno(FILE*) -> fd

;; Low-level I/O (POSIX)
(extern read i64 (i32 ptr i64))     ; read(fd, buf, count) -> bytes read
(extern write i64 (i32 ptr i64))    ; write(fd, buf, count) -> bytes written
(extern open i32 (ptr i32 i32))     ; open(path, flags, mode) -> fd
(extern close i32 (i32))            ; close(fd) -> status
(extern lseek i64 (i32 i64 i32))    ; lseek(fd, offset, whence) -> position

;; Memory allocation (for buffers)
(extern malloc ptr (i64))           ; malloc(size) -> ptr
(extern free void (ptr))            ; free(ptr)
(extern realloc ptr (ptr i64))      ; realloc(ptr, size) -> ptr

;; String operations (useful for paths)
(extern strlen i64 (ptr))           ; strlen(str) -> length
(extern strcpy ptr (ptr ptr))       ; strcpy(dest, src) -> dest
(extern strcat ptr (ptr ptr))       ; strcat(dest, src) -> dest

;; Error handling
(extern perror void (ptr))          ; perror(prefix) - print error to stderr
(extern strerror ptr (i32))         ; strerror(errno) -> error string

;; File system
(extern remove i32 (ptr))           ; remove(path) -> status
(extern rename i32 (ptr ptr))       ; rename(old, new) -> status
(extern stat i32 (ptr ptr))         ; stat(path, buf) -> status
(extern mkdir i32 (ptr i32))        ; mkdir(path, mode) -> status
(extern rmdir i32 (ptr))            ; rmdir(path) -> status
(extern getcwd ptr (ptr i64))       ; getcwd(buf, size) -> buf
(extern chdir i32 (ptr))            ; chdir(path) -> status

;; Environment
(extern getenv ptr (ptr))           ; getenv(name) -> value or null
(extern setenv i32 (ptr ptr i32))   ; setenv(name, value, overwrite) -> status


;;; ============================================
;;; Constants
;;; ============================================

;; Seek origins
(def SEEK_SET 0)
(def SEEK_CUR 1)
(def SEEK_END 2)

;; Standard file descriptors
(def STDIN_FD 0)
(def STDOUT_FD 1)
(def STDERR_FD 2)

;; Open flags (POSIX)
(def O_RDONLY 0)
(def O_WRONLY 1)
(def O_RDWR 2)
(def O_CREAT 64)      ; 0100 octal
(def O_TRUNC 512)     ; 01000 octal
(def O_APPEND 1024)   ; 02000 octal

;; EOF marker
(def EOF -1)


;;; ============================================
;;; File Handle Wrapper
;;; ============================================

(defstruct File (handle: ptr))

(defun file-open (path mode)
  "Open a file. mode is \"r\", \"w\", \"a\", \"rb\", \"wb\", etc.
   Returns File or nil on error."
  (let ((handle (fopen path mode)))
    (if (nil? handle)
        nil
        (File handle))))

(defun file-close (f)
  "Close a file. Returns 0 on success."
  (fclose (. f handle)))

(defun file-read (f buf size)
  "Read up to size bytes into buf. Returns bytes read."
  (fread buf 1 size (. f handle)))

(defun file-write (f buf size)
  "Write size bytes from buf. Returns bytes written."
  (fwrite buf 1 size (. f handle)))

(defun file-seek (f offset whence)
  "Seek to position. whence is SEEK_SET, SEEK_CUR, or SEEK_END."
  (fseek (. f handle) offset whence))

(defun file-tell (f)
  "Return current position in file."
  (ftell (. f handle)))

(defun file-eof? (f)
  "True if at end of file."
  (not (= 0 (feof (. f handle)))))

(defun file-error? (f)
  "True if file has error."
  (not (= 0 (ferror (. f handle)))))

(defun file-flush (f)
  "Flush buffered writes."
  (fflush (. f handle)))

(defun file-getc (f)
  "Read one byte. Returns byte value or EOF (-1)."
  (fgetc (. f handle)))

(defun file-putc (f c)
  "Write one byte."
  (fputc c (. f handle)))

(defun file-puts (f s)
  "Write a string."
  (fputs s (. f handle)))


;;; ============================================
;;; High-Level Conveniences
;;; ============================================

(defun file-size (f)
  "Get file size by seeking to end and back."
  (let ((pos (file-tell f)))
    (file-seek f 0 SEEK_END)
    (let ((size (file-tell f)))
      (file-seek f pos SEEK_SET)
      size)))

(defun slurp (path)
  "Read entire file contents as a string.
   Returns string or nil on error."
  (let ((f (file-open path "rb")))
    (if (nil? f)
        nil
        (let ((size (file-size f))
              (buf (malloc (+ size 1))))  ; +1 for null terminator
          (if (nil? buf)
              (do (file-close f) nil)
              (do
                (file-seek f 0 SEEK_SET)
                (let ((bytes-read (file-read f buf size)))
                  ;; Null-terminate
                  (store-byte (+ buf bytes-read) 0)
                  (file-close f)
                  ;; Return as string (buf is the string data)
                  ;; TODO: wrap in String struct when string library exists
                  buf)))))))

(defun spit (path contents)
  "Write string to file. Overwrites if exists.
   Returns bytes written or -1 on error."
  (let ((f (file-open path "wb")))
    (if (nil? f)
        -1
        (let ((len (strlen contents))
              (written (file-write f contents len)))
          (file-close f)
          written))))

(defun spit-append (path contents)
  "Append string to file.
   Returns bytes written or -1 on error."
  (let ((f (file-open path "ab")))
    (if (nil? f)
        -1
        (let ((len (strlen contents))
              (written (file-write f contents len)))
          (file-close f)
          written))))

(defun file-exists? (path)
  "Check if file exists by trying to open it."
  (let ((f (file-open path "rb")))
    (if (nil? f)
        false
        (do (file-close f) true))))


;;; ============================================
;;; Standard Streams
;;; ============================================

(defun stdin ()
  "Get stdin as a File."
  (File (fdopen STDIN_FD "r")))

(defun stdout ()
  "Get stdout as a File."
  (File (fdopen STDOUT_FD "w")))

(defun stderr ()
  "Get stderr as a File."
  (File (fdopen STDERR_FD "w")))

;; Note: these create new FILE* handles each call.
;; For efficiency, cache them at startup:
;; (def *stdin* (stdin))
;; (def *stdout* (stdout))
;; (def *stderr* (stderr))


;;; ============================================
;;; Output Helpers
;;; ============================================

(defun eprint (s)
  "Print string to stderr."
  (let ((err (stderr)))
    (file-puts err s)
    (file-flush err)))

(defun eprintln (s)
  "Print string to stderr with newline."
  (let ((err (stderr)))
    (file-puts err s)
    (file-putc err 10)  ; newline
    (file-flush err)))


;;; ============================================
;;; Line Reading
;;; ============================================

(defun file-read-line (f)
  "Read a line from file. Returns string or nil at EOF.
   Note: includes trailing newline if present."
  (let ((buf (malloc 1024)))  ; TODO: grow dynamically
    (if (nil? buf)
        nil
        (let ((result (fgets buf 1024 (. f handle))))
          (if (nil? result)
              (do (free buf) nil)
              buf)))))

(defun read-lines (path)
  "Read file as list of lines."
  (let ((f (file-open path "r")))
    (if (nil? f)
        nil
        (let ((lines (read-lines-loop f nil)))
          (file-close f)
          (reverse lines)))))

(defun read-lines-loop (f acc)
  (let ((line (file-read-line f)))
    (if (nil? line)
        acc
        (read-lines-loop f (cons line acc)))))


;;; ============================================
;;; Binary I/O Helpers
;;; ============================================

(defun read-bytes (path)
  "Read file as raw bytes. Returns (ptr . length) or nil."
  (let ((f (file-open path "rb")))
    (if (nil? f)
        nil
        (let ((size (file-size f))
              (buf (malloc size)))
          (if (nil? buf)
              (do (file-close f) nil)
              (do
                (file-seek f 0 SEEK_SET)
                (let ((bytes-read (file-read f buf size)))
                  (file-close f)
                  (cons buf bytes-read))))))))

(defun write-bytes (path data len)
  "Write raw bytes to file."
  (let ((f (file-open path "wb")))
    (if (nil? f)
        -1
        (let ((written (file-write f data len)))
          (file-close f)
          written))))


;;; ============================================
;;; Path Operations
;;; ============================================

(defun path-join (dir file)
  "Join directory and filename with /."
  (let ((dir-len (strlen dir))
        (file-len (strlen file))
        (buf (malloc (+ dir-len file-len 2))))  ; +2 for / and null
    (strcpy buf dir)
    (strcat buf "/")
    (strcat buf file)
    buf))

(defun working-directory ()
  "Get current working directory."
  (let ((buf (malloc 4096)))
    (if (nil? (getcwd buf 4096))
        (do (free buf) nil)
        buf)))

(defun change-directory (path)
  "Change current directory. Returns 0 on success."
  (chdir path))
```

## Dependencies

- liar `extern` for FFI (already working)
- liar `defstruct` for File wrapper
- `store-byte` primitive (may need to add — for null-terminating strings)

## Missing Primitive: `store-byte`

The `slurp` function needs to write a null terminator. This requires a `store-byte` primitive:

```lisp
(store-byte ptr value)  ; store single byte at ptr
```

This maps to LLVM `store i8 %value, ptr %ptr`. Add to `builtins.rs`:

```rust
"store-byte" => {
    if args.len() != 2 {
        return Err(...);
    }
    let ptr = generate_expr(ctx, &args[0])?;
    let val = generate_expr(ctx, &args[1])?;
    Some(lir::Expr::Store {
        value: Box::new(lir::Expr::Trunc { 
            ty: lir::ScalarType::I8, 
            value: Box::new(val) 
        }),
        ptr: Box::new(ptr),
    })
}
```

Also add `load-byte`:

```rust
"load-byte" => {
    check_unary(expr, "load-byte", args)?;
    let ptr = generate_expr(ctx, &args[0])?;
    Some(lir::Expr::ZExt {
        ty: lir::ScalarType::I64,
        value: Box::new(lir::Expr::Load {
            ty: lir::ScalarType::I8,
            ptr: Box::new(ptr),
        }),
    })
}
```

## Acceptance Criteria

### Core File Operations
- [ ] `file-open` / `file-close` work
- [ ] `file-read` / `file-write` work
- [ ] `file-seek` / `file-tell` work
- [ ] `file-getc` / `file-putc` work

### High-Level Functions
- [ ] `slurp` reads entire file to string
- [ ] `spit` writes string to file
- [ ] `spit-append` appends to file
- [ ] `file-exists?` checks existence
- [ ] `read-lines` returns list of lines

### Standard Streams
- [ ] `stdin` / `stdout` / `stderr` work
- [ ] `eprint` / `eprintln` write to stderr

### Path Operations
- [ ] `path-join` concatenates paths
- [ ] `working-directory` returns cwd
- [ ] `change-directory` changes cwd

## Test Scenarios

```gherkin
Feature: I/O Library

  Scenario: slurp and spit round-trip
    Given a file "test.txt" containing "hello world"
    When I evaluate:
      """
      (do
        (spit "/tmp/test.txt" "hello world")
        (slurp "/tmp/test.txt"))
      """
    Then the result should be "hello world"

  Scenario: Read file that doesn't exist
    When I evaluate (slurp "/nonexistent/path")
    Then the result should be nil

  Scenario: Write and append
    When I evaluate:
      """
      (do
        (spit "/tmp/append.txt" "line1\n")
        (spit-append "/tmp/append.txt" "line2\n")
        (slurp "/tmp/append.txt"))
      """
    Then the result should be "line1\nline2\n"

  Scenario: Read lines
    Given I write "a\nb\nc\n" to "/tmp/lines.txt"
    When I evaluate (count (read-lines "/tmp/lines.txt"))
    Then the result should be 3

  Scenario: File size
    Given I write "12345" to "/tmp/size.txt"
    When I evaluate:
      """
      (let ((f (file-open "/tmp/size.txt" "r")))
        (let ((sz (file-size f)))
          (file-close f)
          sz))
      """
    Then the result should be 5
```

## File Structure

```
lib/
  io.liar          ; This file
  runtime.liar     ; Existing async runtime FFI
  
liar-cert/features/
  io_file.feature      ; File operation tests
  io_slurp_spit.feature ; High-level tests
```

## Bootstrap Usage

Once this exists, the compiler can:

```lisp
;; Read source file
(def source (slurp "program.liar"))

;; Compile it (assuming reader and compiler exist)
(def lir (compile (read source)))

;; Write output
(spit "program.lir" lir)
```

## Notes

This is pure liar code — no changes to lIR needed except the `store-byte`/`load-byte` primitives.

The library assumes POSIX. Windows would need different constants and possibly different FFI calls, but the interface stays the same.

Memory management is manual (malloc/free). Once the String type exists (story 003/004), slurp should return a proper String rather than a raw pointer.

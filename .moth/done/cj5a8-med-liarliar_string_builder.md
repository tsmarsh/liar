# liarliar: String Builder

Create `lib/liarliar/strings.liar` - efficient string building for code generation.

**Priority:** HIGH (required for codegen output)

## Related ADRs

- [ADR 019: lIR as Universal Backend](../../doc/adr/019-lir-universal-backend.md) — Output is lIR text
- [ADR 020: Toolchain Architecture](../../doc/adr/020-toolchain-architecture.md) — Compiler emits lIR source

## Overview

StringBuilder for constructing lIR output. Needed because string concatenation would be O(n^2). The self-hosted compiler's output is lIR source text, so this is performance-critical.

## Data Structure

```clojure
(defstruct StringBuilder (buf: ptr len: i64 cap: i64))

(defun make-string-builder () -> ptr
  (let ((cap 1024)
        (buf (malloc cap)))
    (share (StringBuilder buf 0 cap))))
```

## Functions to Implement

```clojure
;; Growth - double capacity when needed
(defun sb-grow (sb: ptr needed: i64) -> i64
  (let ((new-cap (* (+ (. sb cap) needed) 2))
        (new-buf (malloc new-cap)))
    (do
      (copy-bytes new-buf (. sb buf) (. sb len))
      (free (. sb buf))
      ;; Update fields
      0)))

;; Append string
(defun sb-append (sb: ptr s: ptr) -> i64
  (let ((slen (strlen-ptr s)))
    (do
      (if (> (+ (. sb len) slen) (. sb cap))
          (sb-grow sb slen)
          0)
      (copy-bytes (ptr+ (. sb buf) (. sb len)) s slen)
      ;; Update len
      0)))

;; Append integer as string
(defun sb-append-int (sb: ptr n: i64) -> i64
  ...)

;; Finalize - null terminate and return
(defun sb-to-string (sb: ptr) -> ptr
  (do
    (store-byte (ptr+ (. sb buf) (. sb len)) 0)
    (. sb buf)))
```

## Dependencies

- Builtins: `malloc`, `free`, `load-byte`, `store-byte`, `ptr+`
- Helper: `strlen-ptr`, `copy-bytes`

## Test Cases

- Append multiple strings
- Append integers
- Handle growth correctly
- Final string is null-terminated
- Large output (> initial capacity) works

## Ordering

Depends on: Builtins only (no liarliar deps)
Required by: `codegen.liar`

## Design Notes

Initial capacity of 1024 is reasonable for small programs. For compiling the compiler itself, may need larger default or smarter growth strategy (1.5x instead of 2x to reduce memory pressure).

Consider adding `sb-append-char` for single characters — avoids creating string for common case of emitting `(`, `)`, ` `.

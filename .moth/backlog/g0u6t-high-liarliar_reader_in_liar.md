# liarliar: Reader in liar

Create `lib/liarliar/reader.liar` - a full reader written in liar itself.

This is **HIGH priority** because it proves the toolchain works and produces native structures directly.

## Related ADRs

- [ADR 019: lIR as Universal Backend](../../doc/adr/019-lir-universal-backend.md) — Reader output becomes lIR input
- [ADR 020: Toolchain Architecture](../../doc/adr/020-toolchain-architecture.md) — Self-hosting bootstrap path
- [ADR 015: Numeric Primitives](../../doc/adr/015-numeric-primitives.md) — Number literal parsing
- [ADR 025: Explicit Arithmetic Names](../../doc/adr/025-explicit-arithmetic-names.md) — `+` is just a symbol, not special

## Overview

A character-by-character parser using `load-byte` and `ptr+` that produces native tagged Cons cells. This is the front door of the self-hosted compiler — source text goes in, AST comes out.

## Lexer State

```clojure
(defstruct Lexer (src: ptr pos: i64 len: i64))

(defun make-lexer (src: ptr len: i64)
  (share (Lexer src 0 len)))

(defun peek-char (lex: ptr) -> i64
  (if (>= (. lex pos) (. lex len))
      -1  ;; EOF
      (load-byte (ptr+ (. lex src) (. lex pos)))))
```

## Character Classification

Per ADR 025, arithmetic operators (`+`, `-`, `*`, `/`) are just regular symbol characters with no special meaning. The standard prelude defines them as macros/functions.

```clojure
(defun whitespace? (c) (or (= c 32) (= c 10) (= c 9) (= c 13)))
(defun digit? (c) (and (>= c 48) (<= c 57)))
(defun symbol-start? (c)
  (or (and (>= c 97) (<= c 122))   ;; a-z
      (and (>= c 65) (<= c 90))    ;; A-Z
      (= c 95)                      ;; _
      (= c 43) (= c 45)             ;; + - (just symbols per ADR 025)
      (= c 42) (= c 47)             ;; * / (just symbols per ADR 025)
      (= c 60) (= c 62) (= c 61)    ;; < > =
      (= c 63) (= c 33)))           ;; ? !
```

## Main Parser

```clojure
(defun read-value (lex: ptr)
  (do
    (skip-ws lex)
    (let ((c (peek-char lex)))
      (cond
        ((= c -1) NIL)
        ((= c 40) (read-list lex))       ;; (
        ((= c 91) (read-vector lex))     ;; [
        ((= c 123) (read-map lex))       ;; {
        ((= c 34) (read-string lex))     ;; "
        ((= c 58) (read-keyword lex))    ;; :
        ((= c 39) (read-quote lex))      ;; '
        ((digit? c) (read-number lex))
        ((= c 45) (read-number-or-symbol lex))
        ((symbol-start? c) (read-symbol lex))
        (true (error "unexpected char"))))))
```

## Functions to Implement

- `skip-ws` - Skip whitespace and comments (;)
- `read-list` - Parse (a b c) into Cons cells
- `read-vector` - Parse [a b c] into PersistentVector
- `read-map` - Parse {:a 1 :b 2} into HashMap
- `read-string` - Parse "hello" with escape handling
- `read-keyword` - Parse :foo
- `read-symbol` - Parse foo, +, defun, etc.
- `read-number` - Parse integers (and later floats)
- `read-quote` - Parse 'x into (quote x)

## Test Cases

- Read and print back: `(+ 1 2)`
- Read nested: `(defun foo (x) (+ x 1))`
- Read with strings: `(println "hello")`
- Read vectors: `[1 2 3]`
- Read maps: `{:a 1 :b 2}`

## Dependencies

- `lib/liarliar/value.liar` - Tagged value constructors
- `lib/liarliar/symbols.liar` - Symbol interning
- Builtins: `load-byte`, `ptr+`, `malloc`
- `liar.io` - `slurp` for reading files

## Ordering

Depends on: `value.liar`, `symbols.liar`
Required by: `main.liar` (entry point)

## Design Notes

The reader should be streaming-capable (not require entire file in memory) for future REPL integration. Current design with Lexer struct supports this.

Error handling: accumulate errors with source positions rather than failing on first error. This enables better IDE integration later.

Float parsing can be deferred — integer-only is sufficient for bootstrap.

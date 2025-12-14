# liarliar: Reader in liar

Create `lib/liarliar/reader.liar` - a full reader written in liar itself.

This is HIGH priority because it proves the toolchain works and produces native structures directly.

## Overview

A character-by-character parser using `load-byte` and `ptr+` that produces native tagged Cons cells.

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

```clojure
(defun whitespace? (c) (or (= c 32) (= c 10) (= c 9) (= c 13)))
(defun digit? (c) (and (>= c 48) (<= c 57)))
(defun symbol-start? (c)
  (or (and (>= c 97) (<= c 122))   ;; a-z
      (and (>= c 65) (<= c 90))    ;; A-Z
      (= c 95) (= c 43) (= c 45)   ;; _ + -
      (= c 42) (= c 47) (= c 60)   ;; * / <
      (= c 62) (= c 61) (= c 63)   ;; > = ?
      (= c 33)))                    ;; !
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
- Builtins: `load-byte`, `ptr+`, `malloc`
- `liar.io` - `slurp` for reading files

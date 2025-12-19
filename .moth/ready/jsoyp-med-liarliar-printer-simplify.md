# Liarliar Printer Simplify

## Summary

Simplify `liarliar/printer.liar` and `liarliar/strings.liar` by using stdlib string operations where possible.

## Current State

- `strings.liar` (~200 lines) - hand-rolled StringBuilder
- `printer.liar` (~250 lines) - S-expression printer using StringBuilder

Total: ~450 lines for printing/string building

## Target State

~80-100 lines total by:
1. Using stdlib string operations if available
2. Simplifying StringBuilder if we must keep it
3. Removing unused helpers

## Analysis Needed

First, check what string operations are in `liar.core` or other stdlib:
- String concatenation?
- String from number?
- String comparison? (already have `streq`)

If stdlib has these, use them. If not, keep minimal StringBuilder.

## Target printer.liar

```lisp
(ns liarliar.printer
  (:require [liar.core :refer :all]
            [liarliar.value :refer :all]
            [liarliar.symbols :refer :all]))

;; Minimal StringBuilder if stdlib lacks string building
(defstruct StringBuilder (sb-data: ptr sb-len: i64 sb-cap: i64))

;; ... minimal impl ...

(defun print-sexp (form: ptr) -> ptr
  (cond
    ((nil? form) "()")
    ((symbol? form) (symbol-name form))
    ((boxed-int? form) (int-to-string (unbox-int form)))
    ((boxed-bool? form) (if (= (unbox-bool form) 0) "0" "1"))
    ((scons? form) (print-list form))
    (else "<unknown>")))

(defun print-list (lst: ptr) -> ptr
  (let ((sb (string-builder)))
    (sb-append sb "(")
    (print-list-elements sb lst 1)
    (sb-append sb ")")
    (sb-to-string sb)))
```

## Key Simplifications

1. Remove redundant helpers (`emit-indent-loop` if not used)
2. Inline small functions
3. Use direct string literals where possible
4. Remove dead code

## Testing

1. Rebuild liarliar
2. Compile a test file: `/tmp/liarliar /tmp/test.liar`
3. Verify output is valid lIR (pipe to lair)

## Can Run In Parallel With

- `f75jg` (symbols)
- `fdrg3` (env)
- `uhgbj` (expand)

## Acceptance Criteria

- [ ] strings.liar simplified or removed
- [ ] printer.liar uses stdlib where possible
- [ ] Total line count < 100
- [ ] Output still valid lIR
- [ ] All liarliar tests pass

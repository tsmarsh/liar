# stdlib: println

**Priority:** P4  
**Category:** stdlib/io  
**Dependencies:** print  
**Order:** 400

## Description

**BLOCKED: Requires I/O primitives in lIR**

Print a value followed by newline to stdout.

## Implementation

```lisp
;; BLOCKED: Depends on print
;; (defun println (x) (do (print x) (print-char 10)))
```

## Tests

```lisp
(println 42)      ; prints: 42 then newline
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment

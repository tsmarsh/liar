# stdlib: fib

**Tier:** P2
**Category:** stdlib/math
**Dependencies:** none

## Description

Returns the nth Fibonacci number. Naive recursive implementation.

## Implementation

```lisp
(defun fib (n)
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
```

## Tests

```lisp
(fib 0)   ; => 0
(fib 1)   ; => 1
(fib 10)  ; => 55
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment

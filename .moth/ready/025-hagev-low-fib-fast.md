# stdlib: fib-fast

**Tier:** P3
**Category:** stdlib/math
**Dependencies:** none

## Description

Returns the nth Fibonacci number. O(n) iterative via tail recursion.

## Implementation

```lisp
(defun fib-iter (n a b)
  (if (= n 0)
      a
      (fib-iter (- n 1) b (+ a b))))

(defun fib-fast (n)
  (fib-iter n 0 1))
```

## Tests

```lisp
(fib-fast 0)    ; => 0
(fib-fast 1)    ; => 1
(fib-fast 50)   ; => 12586269025
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment

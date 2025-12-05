# stdlib: pow-fast

**Tier:** P3
**Category:** stdlib/math
**Dependencies:** even?

## Description

Fast exponentiation using squaring. O(log n).

## Implementation

```lisp
(defun pow-fast (base exp)
  (if (= exp 0)
      1
      (if (even? exp)
          (let ((half (pow-fast base (/ exp 2))))
            (* half half))
          (* base (pow-fast base (- exp 1))))))
```

## Tests

```lisp
(pow-fast 2 10)   ; => 1024
(pow-fast 2 20)   ; => 1048576
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment

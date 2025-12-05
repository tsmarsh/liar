# stdlib: complement

**Priority:** P3  
**Category:** stdlib/higher-order  
**Dependencies:** none  
**Order:** 370

## Description

Returns a function that returns the logical negation of f.

## Implementation

```lisp
(defun complement (f)
  (fn (x) (not (f x))))
```

## Tests

```lisp
(let ((not-zero? (complement zero?)))
  (not-zero? 5))  ; => true
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment

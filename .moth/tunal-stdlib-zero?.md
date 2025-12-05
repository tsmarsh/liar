# stdlib: zero?

**Priority:** P1  
**Category:** stdlib/predicates  
**Dependencies:** none  
**Order:** 110

## Description

Returns true if x equals zero.

## Implementation

```lisp
(defun zero? (x) (= x 0))
```

## Tests

```lisp
(zero? 0)   ; => true
(zero? 1)   ; => false
(zero? -1)  ; => false
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment

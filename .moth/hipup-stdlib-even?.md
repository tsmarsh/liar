# stdlib: even?

**Priority:** P1  
**Category:** stdlib/predicates  
**Dependencies:** none  
**Order:** 140

## Description

Returns true if x is even.

## Implementation

```lisp
(defun even? (x) (= 0 (rem x 2)))
```

## Tests

```lisp
(even? 0)  ; => true
(even? 2)  ; => true
(even? 3)  ; => false
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment

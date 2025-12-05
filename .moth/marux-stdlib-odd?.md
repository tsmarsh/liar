# stdlib: odd?

**Priority:** P1  
**Category:** stdlib/predicates  
**Dependencies:** none  
**Order:** 150

## Description

Returns true if x is odd.

## Implementation

```lisp
(defun odd? (x) (= 1 (rem x 2)))
```

## Tests

```lisp
(odd? 1)   ; => true
(odd? 3)   ; => true
(odd? 2)   ; => false
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment

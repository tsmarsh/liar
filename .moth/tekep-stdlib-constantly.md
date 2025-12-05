# stdlib: constantly

**Priority:** P2  
**Category:** stdlib/higher-order  
**Dependencies:** none  
**Order:** 330

## Description

Returns a function that always returns v, ignoring its argument.

## Implementation

```lisp
(defun constantly (v)
  (fn (x) v))
```

## Tests

```lisp
(let ((always-5 (constantly 5)))
  (always-5 100))  ; => 5
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment

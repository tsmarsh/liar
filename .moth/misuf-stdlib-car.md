# stdlib: car

**Priority:** P4  
**Category:** stdlib/lists  
**Dependencies:** cons  
**Order:** 420

## Description

**BLOCKED: Requires cons cell runtime**

Returns the first element of a cons cell.

## Implementation

```lisp
;; (defun car (pair) (. pair car))
```

## Tests

```lisp
(car (cons 1 2))  ; => 1
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment

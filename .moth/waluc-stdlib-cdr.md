# stdlib: cdr

**Priority:** P4  
**Category:** stdlib/lists  
**Dependencies:** cons  
**Order:** 430

## Description

**BLOCKED: Requires cons cell runtime**

Returns the second element of a cons cell.

## Implementation

```lisp
;; (defun cdr (pair) (. pair cdr))
```

## Tests

```lisp
(cdr (cons 1 2))  ; => 2
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment

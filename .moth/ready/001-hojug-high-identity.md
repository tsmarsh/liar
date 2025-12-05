# stdlib: identity

**Tier:** P1
**Category:** stdlib/core
**Dependencies:** none

## Description

Returns its argument unchanged. Useful for higher-order functions.

## Implementation

```lisp
(defun identity (x) x)
```

## Tests

```lisp
(identity 5)        ; => 5
(identity nil)      ; => nil
(identity true)     ; => true
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment

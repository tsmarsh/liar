# stdlib: between?

**Tier:** P2
**Category:** stdlib/predicates
**Dependencies:** none

## Description

Returns true if lo <= x <= hi.

## Implementation

```lisp
(defun between? (x lo hi)
  (and (<= lo x) (<= x hi)))
```

## Tests

```lisp
(between? 5 0 10)   ; => true
(between? -1 0 10)  ; => false
(between? 10 0 10)  ; => true
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment

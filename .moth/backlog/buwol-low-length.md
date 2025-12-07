# stdlib: length

**Tier:** P4
**Category:** stdlib/collections
**Dependencies:** cons, car, cdr, nil?

## Description

**BLOCKED: Requires list/vector iteration**

Returns the number of elements in a collection.

## Implementation

```lisp
;; BLOCKED: Needs iteration
;; (defun length (coll)
;;   (if (nil? coll)
;;       0
;;       (+ 1 (length (cdr coll)))))
```

## Tests

```lisp
(length [1 2 3])     ; => 3
(length [])          ; => 0
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment

# stdlib: reverse

**Tier:** P4
**Category:** stdlib/collections
**Dependencies:** cons, car, cdr, nil?

## Description

**BLOCKED: Requires list iteration**

Returns a new list with elements in reverse order.

## Implementation

```lisp
;; BLOCKED: Needs cons/car/cdr
;; (defun reverse-acc (coll acc)
;;   (if (nil? coll)
;;       acc
;;       (reverse-acc (cdr coll) (cons (car coll) acc))))
;; (defun reverse (coll) (reverse-acc coll nil))
```

## Tests

```lisp
(reverse [1 2 3])  ; => [3 2 1]
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment

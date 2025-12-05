# stdlib: clamp

**Priority:** P2  
**Category:** stdlib/arithmetic  
**Dependencies:** min, max  
**Order:** 220

## Description

Constrains x to be between lo and hi (inclusive).

## Implementation

```lisp
(defun clamp (x lo hi)
  (max lo (min x hi)))
```

## Tests

```lisp
(clamp 5 0 10)   ; => 5
(clamp -5 0 10)  ; => 0
(clamp 15 0 10)  ; => 10
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment

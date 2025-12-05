# stdlib: nil?

**Priority:** P4  
**Category:** stdlib/predicates  
**Dependencies:** nil equality  
**Order:** 440

## Description

**BLOCKED: Needs nil comparison working**

Returns true if x is nil.

## Implementation

```lisp
;; May need runtime support for nil comparison
(defun nil? (x) (= x nil))
```

## Tests

```lisp
(nil? nil)        ; => true
(nil? 0)          ; => false
(nil? [])         ; => false
```

## Acceptance Criteria

- [ ] Function implemented in lib/stdlib.liar
- [ ] Tests pass in REPL
- [ ] Documented in lib/stdlib.liar header comment

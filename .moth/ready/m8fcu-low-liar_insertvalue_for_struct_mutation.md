## Summary

Add `insertvalue` support for functional struct updates.

## Problem

Currently liar uses `extractvalue` to read struct fields, but there's no way to create a modified copy of a struct (functional update).

## liar Syntax

```lisp
(defstruct point (x y))

(let ((p (point 1 2)))
  (struct-set p :x 10))  ; => (point 10 2)
```

Or with-style:
```lisp
(with-struct p :x 10)
```

## lIR Output

```lisp
(insertvalue %struct.point %p (i64 10) 0)
```

## Use Cases

- Immutable struct updates
- Record manipulation
- Functional programming patterns

## Acceptance Criteria

- [ ] Parse struct update syntax
- [ ] Generate lIR insertvalue
- [ ] Feature file with test scenarios
- [ ] Works with nested structs

## Notes

Low priority - workaround is to construct new struct with extracted fields.
lIR already supports `InsertValue`.

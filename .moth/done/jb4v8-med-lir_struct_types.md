## Summary
Add struct type definitions.

## Syntax
```lisp
(defstruct name (field-type ...))
```

## Examples
```lisp
(defstruct point (double double))
(defstruct person (ptr i32 double))
```

## LLVM IR Output
```llvm
%struct.point = type { double, double }
%struct.person = type { ptr, i32, double }
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/aggregates.feature`
- [ ] Parse struct definitions
- [ ] Named struct types
- [ ] Use in other types

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.

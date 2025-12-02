## Summary
Add aggregate value access.

## Syntax
```lisp
(extractvalue aggregate index...)
(insertvalue aggregate value index...)
```

## Examples
```lisp
(extractvalue %point 0)              ; get first field
(insertvalue %point (double 1.0) 0)  ; set first field
```

## LLVM IR Output
```llvm
%0 = extractvalue %struct.point %point, 0
%1 = insertvalue %struct.point %point, double 1.0, 0
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/aggregates.feature`
- [ ] Parse extractvalue
- [ ] Parse insertvalue
- [ ] Handle nested indices

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.

## Summary
Add pointer arithmetic to lIR.

## Syntax
```lisp
(getelementptr type ptr index...)
(getelementptr inbounds type ptr index...)
```

## Examples
```lisp
; Array indexing
(getelementptr i32 %arr (i64 5))

; Struct field access
(getelementptr %struct.point %p (i32 0) (i32 1))
```

## LLVM IR Output
```llvm
%0 = getelementptr i32, ptr %arr, i64 5
%1 = getelementptr inbounds %struct.point, ptr %p, i32 0, i32 1
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/memory.feature`
- [ ] Parse GEP with indices
- [ ] Handle inbounds flag
- [ ] Type calculation for result

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- GEP is complex but essential for array/struct access.

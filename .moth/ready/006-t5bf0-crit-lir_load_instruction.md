## Summary
Add memory load to lIR.

## Syntax
```lisp
(load type ptr-value)
```

## Examples
```lisp
(load i32 %ptr)
(load double %dptr)
```

## LLVM IR Output
```llvm
%0 = load i32, ptr %ptr
%1 = load double, ptr %dptr
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/memory.feature`
- [ ] Parse load with type and pointer
- [ ] Type check pointer argument
- [ ] Returns the loaded type

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.

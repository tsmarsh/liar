## Summary
Add external function declarations.

## Syntax
```lisp
(declare name return-type (param-types...))
```

## Examples
```lisp
(declare printf i32 (ptr ...))
(declare malloc ptr (i64))
(declare exit void (i32))
```

## LLVM IR Output
```llvm
declare i32 @printf(ptr, ...)
declare ptr @malloc(i64)
declare void @exit(i32)
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/globals.feature`
- [ ] Parse declare
- [ ] Handle varargs (...)
- [ ] Functions callable but not defined

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- **ADR-013**: External declarations are how lIR calls C/system functions (unsafe boundary).

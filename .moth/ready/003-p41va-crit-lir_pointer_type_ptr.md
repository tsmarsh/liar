## Summary
Add opaque pointer type to lIR.

## Syntax
```lisp
ptr             ; opaque pointer type
(ptr null)      ; null pointer
```

## Notes
LLVM now uses opaque pointers (just `ptr`), not typed pointers (`i32*`).

## Acceptance Criteria
- [ ] Feature file in `cert/features/pointers.feature`
- [ ] Add ptr to type system
- [ ] Parse null pointer literals
- [ ] Pointer type in function signatures

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- Opaque pointers are the only pointer type in modern LLVM (15+).

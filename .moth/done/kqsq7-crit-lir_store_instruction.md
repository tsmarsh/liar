## Summary
Add memory store to lIR.

## Syntax
```lisp
(store value ptr)
```

## Examples
```lisp
(store (i32 42) %ptr)
(store %value %ptr)
```

## LLVM IR Output
```llvm
store i32 42, ptr %ptr
store i32 %value, ptr %ptr
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/memory.feature`
- [ ] Parse store with value and pointer
- [ ] Type check value and pointer
- [ ] Generates store instruction (returns void)

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- Store is a side effect, returns void.

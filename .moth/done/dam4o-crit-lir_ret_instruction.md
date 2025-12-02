## Summary
Add ret (return) instruction to lIR.

## Syntax
```lisp
(ret value)     ; return with value
(ret)           ; void return
```

## Examples
```lisp
(ret (i32 42))
(ret (add (i32 1) (i32 2)))
(ret)
```

## LLVM IR Output
```llvm
ret i32 42
ret i32 %0
ret void
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/functions.feature` (same file as define)
- [ ] Parse ret with value
- [ ] Parse void ret
- [ ] Type check return value matches function return type
- [ ] Generates terminator instruction

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- Ret is a terminator instruction - every basic block must end with one.

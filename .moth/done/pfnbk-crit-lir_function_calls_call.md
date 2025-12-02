## Summary
Add function call support to lIR.

## Syntax
```lisp
(call @function-name arg1 arg2 ...)
```

## Examples
```lisp
(call @add-one (i32 41))
(call @printf (ptr @format) (i32 42))
(call @exit (i32 0))
```

## LLVM IR Output
```llvm
%0 = call i32 @add-one(i32 41)
call void @exit(i32 0)
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/functions.feature`
- [ ] Parse call expressions
- [ ] Resolve function references
- [ ] Type check arguments
- [ ] Handle void vs non-void return

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- **ADR-019**: Functions are essential - liar compiles to lIR function calls.

## Summary
Add string literal support.

## Syntax
```lisp
(string "content")
```

## Examples
```lisp
(global fmt ptr (string "Value: %d\n") :constant)
(call @printf (getelementptr i8 @fmt (i64 0)) (i32 42))
```

## LLVM IR Output
```llvm
@fmt = constant [11 x i8] c"Value: %d\0A\00"
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/globals.feature`
- [ ] Parse string literals
- [ ] Handle escape sequences (\n, \t, \0, \\, \")
- [ ] Generate as constant array

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- Strings are null-terminated [N x i8] arrays in LLVM.

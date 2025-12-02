## Summary
Add global variable definitions.

## Syntax
```lisp
(global name type initializer)
(global name type initializer :constant)
```

## Examples
```lisp
(global counter i32 (i32 0))
(global pi double (double 3.14159) :constant)
(global message ptr (string "Hello\n") :constant)
```

## LLVM IR Output
```llvm
@counter = global i32 0
@pi = constant double 3.14159
@message = constant [7 x i8] c"Hello\0A\00"
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/globals.feature`
- [ ] Parse global definitions
- [ ] Handle constant flag
- [ ] Initialize with value

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.

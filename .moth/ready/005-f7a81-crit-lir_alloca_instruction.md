## Summary
Add stack allocation to lIR.

## Syntax
```lisp
(alloca type)           ; allocate one element
(alloca type count)     ; allocate array
```

## Examples
```lisp
(alloca i32)            ; allocate space for one i32
(alloca double)         ; allocate space for one double
(alloca i8 (i64 100))   ; allocate 100 bytes
```

## LLVM IR Output
```llvm
%0 = alloca i32
%1 = alloca double
%2 = alloca i8, i64 100
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/memory.feature`
- [ ] Parse alloca with type
- [ ] Parse alloca with count
- [ ] Returns ptr type

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- Alloca is for stack allocation. Essential for local variables.

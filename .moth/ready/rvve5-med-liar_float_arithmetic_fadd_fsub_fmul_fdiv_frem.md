## Summary

Add float arithmetic operations to liar, compiling to lIR's `fadd`, `fsub`, `fmul`, `fdiv`, `frem`.

## liar Syntax

```lisp
(+. 1.0 2.0)   ; fadd
(-. 3.0 1.0)   ; fsub
(*. 2.0 3.0)   ; fmul
(/. 6.0 2.0)   ; fdiv
(%. 5.0 2.0)   ; frem (float remainder)
```

Alternative: overload `+`, `-`, `*`, `/`, `%` based on operand types (requires type inference).

## lIR Output

```lisp
(fadd (double 1.0) (double 2.0))
(fsub (double 3.0) (double 1.0))
```

## Acceptance Criteria

- [ ] Parse float arithmetic operators
- [ ] Generate correct lIR float ops
- [ ] Feature file with test scenarios
- [ ] Works with both `float` and `double` types

## Notes

lIR already supports these fully. This is purely liar frontend work.

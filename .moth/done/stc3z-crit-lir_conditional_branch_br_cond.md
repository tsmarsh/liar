## Summary
Add conditional branch to lIR.

## Syntax
```lisp
(br condition true-label false-label)
```

## Examples
```lisp
(br (icmp eq x (i32 0)) is-zero not-zero)
(br %flag then else)
```

## LLVM IR Output
```llvm
br i1 %0, label %is-zero, label %not-zero
br i1 %flag, label %then, label %else
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/control_flow.feature`
- [ ] Parse conditional br
- [ ] Condition must be i1
- [ ] Resolve both label references

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.

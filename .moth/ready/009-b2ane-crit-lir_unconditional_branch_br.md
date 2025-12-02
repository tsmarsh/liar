## Summary
Add unconditional branch to lIR.

## Syntax
```lisp
(br label)
```

## Examples
```lisp
(br loop)
(br exit)
```

## LLVM IR Output
```llvm
br label %loop
br label %exit
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/control_flow.feature`
- [ ] Parse unconditional br
- [ ] Resolve label references
- [ ] Terminator instruction

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.

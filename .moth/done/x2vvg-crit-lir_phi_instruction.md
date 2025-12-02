## Summary
Add phi nodes for SSA form.

## Syntax
```lisp
(phi type (value1 label1) (value2 label2) ...)
```

## Examples
```lisp
(phi i32 ((i32 0) entry) (%next loop))
(phi ptr ((%a then) (%b else)))
```

## LLVM IR Output
```llvm
%0 = phi i32 [ 0, %entry ], [ %next, %loop ]
%1 = phi ptr [ %a, %then ], [ %b, %else ]
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/control_flow.feature`
- [ ] Parse phi with incoming values
- [ ] All values must have same type
- [ ] Labels must exist

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- Phi nodes are essential for SSA - they select values based on which block we came from.

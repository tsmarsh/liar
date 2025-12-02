## Summary
Add basic block support to lIR.

## Syntax
```lisp
(define (name ret-type) (params...)
  (block entry
    ...)
  (block label2
    ...)
  (block label3
    ...))
```

## Examples
```lisp
(define (abs i32) ((i32 x))
  (block entry
    (br (icmp slt x (i32 0)) neg pos))
  (block neg
    (ret (sub (i32 0) x)))
  (block pos
    (ret x)))
```

## LLVM IR Output
```llvm
define i32 @abs(i32 %x) {
entry:
  %0 = icmp slt i32 %x, 0
  br i1 %0, label %neg, label %pos
neg:
  %1 = sub i32 0, %x
  ret i32 %1
pos:
  ret i32 %x
}
```

## Acceptance Criteria
- [ ] Feature file in `cert/features/control_flow.feature`
- [ ] Parse block definitions
- [ ] Named labels
- [ ] First block is entry point

## Notes
- **Spec-first**: Write the feature file BEFORE implementing.
- Basic blocks are the foundation of control flow in SSA form.

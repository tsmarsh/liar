## Summary

Add memory fence/barrier operation to liar.

## liar Syntax

```lisp
(fence)              ; seq_cst by default
(fence :acquire)     ; acquire fence
(fence :release)     ; release fence
(fence :acq-rel)     ; acquire-release fence
```

## lIR Output

```lisp
(fence seq_cst)
(fence acquire)
```

## Use Cases

- Ensuring memory ordering between non-atomic operations
- Implementing custom synchronization primitives
- Low-level concurrent data structures

## Acceptance Criteria

- [ ] Parse fence with optional ordering
- [ ] Generate correct lIR fence
- [ ] Feature file with test scenarios

## Notes

Low priority - most code should use atomic operations which include ordering.
lIR already supports this.

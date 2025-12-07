## Summary

Add atomic read-modify-write operations to liar for lock-free data structures.

## liar Syntax

```lisp
(atomic-add! ptr val)      ; atomicrmw add
(atomic-sub! ptr val)      ; atomicrmw sub
(atomic-and! ptr val)      ; atomicrmw and
(atomic-or! ptr val)       ; atomicrmw or
(atomic-xor! ptr val)      ; atomicrmw xor
(atomic-xchg! ptr val)     ; atomicrmw xchg (swap)
(atomic-min! ptr val)      ; atomicrmw min (signed)
(atomic-max! ptr val)      ; atomicrmw max (signed)
```

## lIR Output

```lisp
(atomicrmw add seq_cst %ptr (i64 1))
```

## Use Cases

- Lock-free counters
- Atomic flags
- HAMT node updates
- Reference counting (already using cmpxchg, but atomicrmw add/sub is faster)

## Acceptance Criteria

- [ ] Parse atomic RMW operations
- [ ] Generate correct lIR atomicrmw
- [ ] Feature file with test scenarios
- [ ] Memory ordering parameter (default seq_cst)

## Notes

lIR already supports `AtomicRMW`. This enables more efficient atomic patterns than cmpxchg loops.

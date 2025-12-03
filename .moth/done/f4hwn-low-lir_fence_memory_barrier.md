## Summary
Add fence instruction for explicit memory barriers.
Usually not needed (atomic ops have implicit fences), but useful for
advanced lock-free algorithms.

## Syntax
```lisp
(fence ordering)
```

## Semantics
- Prevents reordering of memory operations across the fence
- No return value (void)

## AST Addition
```rust
pub enum Expr {
    // ... existing ...
    Fence {
        ordering: MemoryOrdering,
    },
}
```

## Codegen
```rust
Expr::Fence { ordering } => {
    self.builder.build_fence(ordering.to_llvm(), "");
    // Returns void/unit
}
```

## Use Cases
- Implementing spin locks
- Memory barriers for non-atomic data
- Synchronizing with hardware

## Test Cases
```gherkin
Scenario: Fence compiles
  Given the expression (define (test void) () (block entry (fence seq_cst) (ret)))
  Then compilation succeeds
```

## Acceptance Criteria
- [ ] fence parses with all orderings
- [ ] Codegen produces LLVM fence
- [ ] No return value

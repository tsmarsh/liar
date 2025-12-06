## Summary

lIR is a stable, complete compilation target. Document and enforce the boundary.

## The Boundary

```
┌─────────────────────────────────────────┐
│                liar                      │
│  All abstractions: closures, protocols,  │
│  atoms, type inference, etc.             │
│                                          │
│  Codegen just emits lIR nodes            │
└────────────────┬────────────────────────┘
                 │ lIR AST
                 ▼
┌─────────────────────────────────────────┐
│                lIR                       │
│  Generic primitives only                 │
│  No liar concepts                        │
│  Standalone, testable without liar       │
└─────────────────────────────────────────┘
```

## Enforcement Rules

### In lir-core and lir-codegen:
1. No liar imports
2. No liar terminology ("closure", "protocol", "atom")
3. Generic primitives only (IndirectCall, not ClosureCall)

### In liar:
1. No lIR modification
2. All abstraction in liar
3. Codegen is dumb - just translates

## Verification

lIR should be testable without liar:

```rust
// In lir-core tests
#[test]
fn test_closure_pattern_without_liar() {
    let source = r#"
        (defstruct env (i64))
        (define (lambda_0 i64) ((ptr env) (i64 x))
          (block entry ...))
    "#;
    // Compiles without liar
}
```

## Documentation

Add to `lir-core/README.md`:
- Design principles
- Relationship to liar
- What lIR does NOT know about

## Acceptance Criteria

- [ ] lir-core has no liar dependency
- [ ] lir-codegen has no liar dependency
- [ ] lir-core tests cover patterns without liar
- [ ] README documents boundary
- [ ] No liar terminology in lIR codebase

## Summary
Verify that phi node syntax is consistent between feature files, parser, and codegen.

## Current AST
```rust
Phi {
    ty: ScalarType,
    incoming: Vec<(String, Box<Expr>)>,  // (label, value) order
}
```

## Feature file syntax
```lisp
(phi i32 (neg (i32 0)) (pos x))
```

This appears to be `(label value)` pairs, which matches the AST.

## Verification needed
1. Check parser `parse_phi()` - does it parse `(label value)` or `(value label)`?
2. Check codegen - does it use the tuple in the right order?
3. Run phi tests to verify

## If there's a mismatch
Either:
- Fix parser to match feature file syntax
- Fix feature files to match parser expectation
- Fix codegen to use correct tuple order

## Acceptance Criteria
- [ ] Phi syntax documented in lIR.md
- [ ] Parser matches documented syntax
- [ ] Codegen uses correct order for `phi.add_incoming()`
- [ ] control_flow.feature phi scenarios pass

## Summary
Phi node compilation fails because codegen doesn't provide block context.
Phi nodes need to know which basic block they're being compiled in to set up
the incoming edges correctly.

## Symptom
Tests with phi nodes fail at runtime or produce wrong results.

## Root cause
`compile_expr` or `compile_expr_recursive` doesn't thread the current block
through to phi node compilation. When we call `phi.add_incoming()`, we need
the LLVM BasicBlock reference for each predecessor.

## Current code path (likely)
```rust
fn compile_expr(&self, expr: &Expr) -> Result<...> {
    match expr {
        Expr::Phi { ty, incoming } => {
            // Problem: we don't know what block we're in
            // Can't properly set up phi.add_incoming(&[(value, block)])
        }
    }
}
```

## Fix approach
Thread `current_block: BasicBlockValue` through compilation:

```rust
fn compile_expr_in_block(
    &self, 
    expr: &Expr, 
    block: BasicBlockValue,
    locals: &HashMap<String, BasicValueEnum>
) -> Result<...> {
    match expr {
        Expr::Phi { ty, incoming } => {
            let phi = self.builder.build_phi(llvm_type, "phi")?;
            for (label, value_expr) in incoming {
                let pred_block = self.block_map.get(label)?;
                let value = self.compile_expr_in_block(value_expr, *pred_block, locals)?;
                phi.add_incoming(&[(&value, *pred_block)]);
            }
            Ok(phi.as_basic_value())
        }
    }
}
```

## Key insight
The phi value itself is compiled in the *current* block, but each incoming
value is associated with its *predecessor* block. The codegen needs access to
the block map to resolve label names to BasicBlockValue references.

## Test cases that should pass after fix
- control_flow.feature: "Phi node merging values"
- control_flow.feature: "Loop with phi"  
- integration_milestone.feature: "Phi node merging values from two paths"
- integration_milestone.feature: "Loop with phi - sum 1 to n"

## Acceptance criteria
- [ ] Phi nodes compile correctly with proper incoming edges
- [ ] control_flow.feature phi scenarios pass
- [ ] integration_milestone.feature phi scenarios pass

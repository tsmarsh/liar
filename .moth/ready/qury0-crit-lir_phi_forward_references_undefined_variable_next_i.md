## Summary
Phi nodes that reference values defined later in the same block fail with
"undefined variable: next-i". This is a fundamental SSA issue.

## Example that fails
```lisp
(block loop
  (let ((i (phi i32 (entry (i32 0)) (loop next-i)))      ; references next-i
        (acc (phi i32 (entry (i32 0)) (loop next-acc)))) ; references next-acc
    (let ((next-i (add i (i32 1)))                        ; defines next-i
          (next-acc (add acc i)))                         ; defines next-acc
      (br (icmp sle i n) loop done))))
```

The phi references `next-i` which is defined AFTER the phi in the same block.

## Root cause
Compilation is sequential - when compiling the phi, `next-i` doesn't exist yet.

## LLVM's solution
In LLVM IR, phi nodes use forward references naturally because:
1. All phi nodes are at the TOP of a block
2. Incoming values are resolved AFTER the block is fully compiled
3. The incoming (value, block) pairs are added as a separate pass

## Fix approach
Two-pass compilation within each block:

**Pass 1:** Create all phi nodes as empty placeholders
```rust
for expr in &block.instructions {
    if let Expr::Let { bindings, .. } = expr {
        for (name, value) in bindings {
            if let Expr::Phi { ty, .. } = value.as_ref() {
                let phi = builder.build_phi(ty, name)?;
                locals.insert(name, phi.as_basic_value());
            }
        }
    }
}
```

**Pass 2:** Compile all expressions, populate phi incoming edges
```rust
for expr in &block.instructions {
    compile_expr(expr, &mut locals)?;
}

// Now go back and fill in phi incoming edges
for (name, phi_node) in &phi_placeholders {
    let incoming = get_phi_incoming(name);
    for (label, value_expr) in incoming {
        let value = locals.get(value_expr_name)?;  // Now defined!
        let pred_block = block_map.get(label)?;
        phi_node.add_incoming(&[(value, pred_block)]);
    }
}
```

## Alternative: Require phi values defined before use
Change the syntax to require forward-declared values:
```lisp
(block loop
  (let ((next-i (undef i32))    ; placeholder
        (next-acc (undef i32)))
    (let ((i (phi i32 (entry (i32 0)) (loop next-i)))
          (acc (phi i32 (entry (i32 0)) (loop next-acc))))
      (set! next-i (add i (i32 1)))   ; mutate placeholder
      ...)))
```
This is ugly and un-SSA-like. Two-pass is better.

## Test cases
- control_flow.feature: "Loop with phi" 
- integration_milestone.feature: "Loop with phi - sum 1 to n"

## Acceptance criteria
- [ ] Phi nodes can reference values defined later in same block
- [ ] Loop patterns with phi work correctly
- [ ] control_flow.feature loop scenario passes

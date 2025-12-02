#!/bin/bash

# =============================================================================
# Moths for exposed codegen bugs (phi context + type checker function context)
# =============================================================================

moth new "lIR: phi nodes need block context in codegen" -s crit --no-edit --stdin << 'EOF'
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
EOF

moth new "lIR: type checker needs function context for parameters" -s crit --no-edit --stdin << 'EOF'
## Summary
Type checking fails for function parameters because `compile_expr` creates a
fresh type checker without knowledge of parameter types.

## Symptom
Functions with parameters fail type checking - parameter references like `x`
or `n` are treated as undefined.

## Root cause
When compiling a function body, we need to seed the type environment with
parameter types:

```rust
fn compile_function(&self, func: &FunctionDef) -> Result<...> {
    // Parameters are added to locals for codegen
    for param in &func.params {
        locals.insert(param.name.clone(), param_value);
    }
    
    // But type checker doesn't know about them!
    let checker = TypeChecker::new();  // Fresh, empty context
    checker.check(expr)?;  // Fails: "unknown variable x"
}
```

## Fix approach
Initialize type checker with parameter types:

```rust
fn compile_function(&self, func: &FunctionDef) -> Result<...> {
    let mut type_env = HashMap::new();
    
    // Seed type environment with parameters
    for param in &func.params {
        type_env.insert(param.name.clone(), param.ty.clone());
    }
    
    // Create checker with pre-populated environment
    let checker = TypeChecker::with_env(type_env);
    
    // Now parameter references will resolve
    for block in &func.blocks {
        for expr in &block.instructions {
            checker.check(expr)?;
        }
    }
}
```

## Alternative: Skip type checking for variable refs in codegen
If the variable is in `locals`, trust it - codegen already has the value.
Type checking is really for catching mismatches in operations, not for
validating that variables exist (that's a name resolution issue).

## Test cases that should pass after fix
- Any function with parameters that references them:
  - `(define (square i64) ((i64 x)) (ret (mul x x)))`
  - `(define (add2 i64) ((i64 a) (i64 b)) (ret (add a b)))`

## Acceptance criteria
- [ ] Functions with parameters compile without type errors
- [ ] Parameter types are correctly known during type checking
- [ ] functions.feature scenarios with parameters pass
- [ ] integration_milestone.feature function scenarios pass
EOF

moth new "lIR: remaining 11 skipped scenarios" -s high --no-edit --stdin << 'EOF'
## Summary
After adding function call steps, 11 scenarios remain skipped. Audit and fix.

## Investigation needed
Run tests with verbose output to identify which scenarios are still skipped:
```bash
cargo test --test cert -- --nocapture 2>&1 | grep -A2 "skipped"
```

## Likely causes
1. Missing step definitions for edge cases:
   - `When I allocate X and call Y with Z`
   - `And I create an adder with captured value (i64 10)`
   - `When I call the adder with (i64 32)`
   
2. Complex integration_milestone scenarios that need special handling

3. Struct/GEP scenarios that need pointer setup

## Approach
1. List all 11 skipped scenarios
2. Check what step patterns they use
3. Add missing step definitions or fix feature file syntax

## Acceptance criteria
- [ ] All 11 skipped scenarios identified
- [ ] Missing step definitions added
- [ ] 201 passed, 0 skipped (after phi and type checker fixes)
EOF

echo ""
echo "Created 3 moths:"
echo "  1. [CRIT] Phi nodes need block context"
echo "  2. [CRIT] Type checker needs function context" 
echo "  3. [HIGH] Remaining 11 skipped scenarios"
echo ""
echo "Fix order:"
echo "  1. Type checker context (likely unblocks most failures)"
echo "  2. Phi block context (unblocks control flow)"
echo "  3. Audit remaining skipped"
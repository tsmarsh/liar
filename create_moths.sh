#!/bin/bash

# =============================================================================
# Moths for the 13 remaining codegen failures
# =============================================================================

moth new "lIR: phi forward references (undefined variable: next-i)" -s crit --no-edit --stdin << 'EOF'
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
EOF

moth new "lIR: external declarations not compiled (undefined function: abs)" -s crit --no-edit --stdin << 'EOF'
## Summary
Calling external functions declared with `(declare ...)` fails with
"undefined function: abs". The declaration is parsed but not registered
with the JIT/codegen.

## Example that fails
```lisp
(declare abs i64 (i64))
(define (call-abs i64) ((i64 x)) (ret (call @abs x)))
```

## Root cause
`ExternDecl` items are parsed but `compile_item()` or the JIT doesn't
handle them - only `FunctionDef` items get compiled.

## Fix approach
In codegen, handle ExternDecl by declaring the external function:

```rust
fn compile_item(&self, item: &Item) -> Result<()> {
    match item {
        Item::Function(func) => self.compile_function(func),
        Item::ExternDecl(decl) => self.declare_external(decl),
        Item::Global(global) => self.compile_global(global),
        Item::Struct(struct_def) => self.register_struct(struct_def),
    }
}

fn declare_external(&self, decl: &ExternDecl) -> Result<()> {
    let fn_type = self.function_type(&decl.return_type, &decl.param_types, decl.varargs);
    self.module.add_function(&decl.name, fn_type, Some(Linkage::External));
    Ok(())
}
```

The key is `Linkage::External` - this tells LLVM the function is defined
elsewhere and will be resolved at link/runtime.

## JIT resolution
For JIT execution, external functions like `abs` need to be resolvable.
Options:
1. Link against libc (abs is in libc)
2. Use `execution_engine.add_global_mapping()` to point to the C function
3. Rely on the system's dynamic linker

Most JITs just let the dynamic linker handle it - libc symbols are available.

## Test cases
- integration_milestone.feature: "External function declaration and call"

## Acceptance criteria
- [ ] `(declare name ret (params))` creates external function declaration
- [ ] Calls to declared externals resolve at JIT runtime
- [ ] `(call @abs (i64 -42))` returns `(i64 42)`
EOF

moth new "lIR: local references require function context" -s high --no-edit --stdin << 'EOF'
## Summary
Parameter and local variable references fail because the type checker or
codegen doesn't have function context when compiling expressions.

## Symptom
```
Error: undefined variable: x
```
For code like:
```lisp
(define (square i64) ((i64 x)) (ret (mul x x)))
```

## Root cause
When `compile_expr` is called, it doesn't know about function parameters.
The `locals` map needs to be populated with parameter values before
compiling the function body.

## Current flow (broken)
```rust
fn compile_function(&self, func: &FunctionDef) -> Result<()> {
    let fn_value = self.module.add_function(...);
    
    for block in &func.blocks {
        for expr in &block.instructions {
            self.compile_expr(expr)?;  // locals is empty!
        }
    }
}
```

## Fix
```rust
fn compile_function(&self, func: &FunctionDef) -> Result<()> {
    let fn_value = self.module.add_function(...);
    
    // Initialize locals with parameters
    let mut locals = HashMap::new();
    for (i, param) in func.params.iter().enumerate() {
        let param_value = fn_value.get_nth_param(i as u32)?;
        locals.insert(param.name.clone(), param_value);
    }
    
    for block in &func.blocks {
        for expr in &block.instructions {
            self.compile_expr_with_locals(expr, &locals)?;
        }
    }
}
```

## Acceptance criteria
- [ ] Function parameters are accessible in function body
- [ ] functions.feature parameter scenarios pass
- [ ] integration_milestone.feature function scenarios pass
EOF

moth new "lIR: JIT missing ptr return signature" -s high --no-edit --stdin << 'EOF'
## Summary
Functions returning `ptr` can't be called from tests because the JIT
only has signatures for i64 returns.

## Current signatures (likely)
```rust
// In JIT call code
type NoArgsI64 = unsafe extern "C" fn() -> i64;
type OneArgI64 = unsafe extern "C" fn(i64) -> i64;
type TwoArgsI64 = unsafe extern "C" fn(i64, i64) -> i64;
```

## Missing signatures
```rust
type NoArgsPtr = unsafe extern "C" fn() -> *mut u8;
type OneArgI64Ptr = unsafe extern "C" fn(i64) -> *mut u8;
// etc.
```

## Fix approach
Add ptr return signatures and dispatch based on function return type:

```rust
fn call_function(&self, name: &str, args: &[i64]) -> Result<Value> {
    let func = self.get_function(name)?;
    let ret_type = func.get_return_type();
    
    match (ret_type, args.len()) {
        (ReturnType::Ptr, 0) => {
            let f: NoArgsPtr = transmute(func_ptr);
            let ptr = unsafe { f() };
            Ok(Value::Ptr(ptr))
        }
        (ReturnType::Ptr, 1) => {
            let f: OneArgI64Ptr = transmute(func_ptr);
            let ptr = unsafe { f(args[0]) };
            Ok(Value::Ptr(ptr))
        }
        (ReturnType::Scalar(I64), 0) => {
            let f: NoArgsI64 = transmute(func_ptr);
            let val = unsafe { f() };
            Ok(Value::I64(val))
        }
        // ... etc
    }
}
```

## Test cases
- integration_milestone.feature: Functions returning ptr (make_adder, etc.)

## Acceptance criteria
- [ ] Functions returning ptr can be called from tests
- [ ] Pointer values displayed as `(ptr 0x...)` or `(ptr null)`
EOF

moth new "lIR: complex integration scenarios marked PENDING" -s low --no-edit --stdin << 'EOF'
## Summary
Some integration_milestone scenarios require complex test scaffolding
(allocating structs, calling closures) that are marked PENDING.

## Affected scenarios
- "When I allocate counter and call set-count with X"
- "And I create an adder with captured value X"  
- "When I call the adder with X"
- "Then loading field X returns Y"

## These require
1. Allocating memory from test harness
2. Passing pointers between test and JIT
3. Reading back struct fields

## Options
1. **Implement scaffolding** - test harness allocates memory, passes to JIT
2. **Rewrite scenarios** - make them self-contained in lIR code
3. **Accept as out of scope** - these test patterns liar won't need

## Recommendation
Option 2: Rewrite scenarios to be self-contained. The closure test should:
1. Call make_adder (returns ptr)
2. Call adder_fn with that ptr
3. Return the result

No external allocation needed - all memory management in lIR.

## Acceptance criteria
- [ ] Decide on approach
- [ ] Either implement scaffolding or rewrite scenarios
- [ ] 201 passed, 0 failed
EOF

echo ""
echo "Created 5 moths for the 13 failures:"
echo "  1. [CRIT] Phi forward references - loop patterns broken"
echo "  2. [CRIT] External declarations - FFI calls fail"
echo "  3. [HIGH] Local references - parameter access broken"  
echo "  4. [HIGH] JIT ptr signatures - can't call ptr-returning functions"
echo "  5. [LOW]  Integration PENDING - test scaffolding needs work"
echo ""
echo "Note: Moths 1-3 likely overlap - fixing locals context may fix several."
echo "Fix order: 3 → 1 → 2 → 4 → 5"
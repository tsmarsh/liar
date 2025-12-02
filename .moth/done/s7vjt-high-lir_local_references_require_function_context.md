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

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

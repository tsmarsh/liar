## Summary

Once closure conversion runs before codegen, remove all closure-related complexity from codegen.

## Prerequisites

- Closure conversion pass (MOTH 002) must be complete
- Lambdas are converted to ClosureStruct before codegen sees them

## Remove From codegen.rs

Delete thread-local tables:
- `CLOSURE_INFO`
- `GENERATED_LAMBDAS`
- `GENERATED_ENV_STRUCTS`
- `LAMBDA_COUNTER`
- `NEEDS_MALLOC`
- `FUNC_PARAM_TYPES`
- `GENERATED_THUNKS`
- `THUNK_CACHE`

Delete related functions:
- All `reset_*`, `set_*`, `lookup_*`, `take_*` for closure state
- `generate_lambda()`
- `generate_closure_call()`
- `generate_closure_call_expr()`
- `get_or_create_thunk()`

## Simplify Cases

### Expr::Lambda
```rust
Expr::Lambda(_, _) => {
    // Should never reach here
    Err(CompileError::codegen(
        expr.span,
        "internal error: lambda should have been converted",
    ))
}
```

### Expr::ClosureStruct (NEW)
```rust
Expr::ClosureStruct { fn_name, env } => {
    let fn_ptr = lir::Expr::GlobalRef(fn_name.clone());
    let env_ptr = match env {
        Some(e) => generate_expr(e)?,
        None => lir::Expr::NullPtr,
    };
    Ok(lir::Expr::StructLit(vec![fn_ptr, env_ptr]))
}
```

### Expr::ClosureCall (NEW)
```rust
Expr::ClosureCall { closure, args } => {
    // Extract fn_ptr and env_ptr, do indirect call
}
```

### Expr::Var
```rust
// Before: complex logic checking for function references
// After: just emit LocalRef
Expr::Var(name) => Ok(lir::Expr::LocalRef(name.clone()))
```

## Expected Result

codegen.rs should shrink by ~300-400 lines and have no closure-specific logic.

## Acceptance Criteria

- [ ] codegen.rs has no thread-local closure state
- [ ] Expr::Lambda case is unreachable error
- [ ] No thunk generation code
- [ ] All tests pass
- [ ] `(let ((f (fn (x) x))) (f 5))` still works

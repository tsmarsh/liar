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

## Summary

`liar/src/codegen.rs` is ~1100+ lines. Split into focused modules.

## Target State

```
liar/src/codegen/
  mod.rs          - generate(), generate_expr() dispatch (~150 lines)
  context.rs      - CodegenContext struct (~100 lines)
  types.rs        - type conversion utilities (~150 lines)
  builtins.rs     - arithmetic, comparison, boolean ops (~200 lines)
  structs.rs      - struct construction, field access (~150 lines)
  protocols.rs    - protocol method dispatch (~100 lines)
  atoms.rs        - atom operations (~150 lines)
  collections.rs  - vector, map, keyword literals (~100 lines)
  tests.rs        - all codegen tests (~300 lines)
```

## Key Change: Explicit Context

Replace thread-local state with explicit context:

```rust
pub struct CodegenContext {
    var_counter: usize,
    func_return_types: HashMap<String, lir::ReturnType>,
    struct_defs: HashMap<String, StructInfo>,
    // etc.
}

impl CodegenContext {
    pub fn fresh_var(&mut self, prefix: &str) -> String { ... }
    pub fn lookup_struct(&self, name: &str) -> Option<&StructInfo> { ... }
}
```

All functions take `&mut CodegenContext`:

```rust
fn generate_struct_constructor(
    ctx: &mut CodegenContext,
    ...
) -> Result<lir::Expr>
```

## Steps

1. Create `CodegenContext` in `context.rs`
2. Extract type utilities to `types.rs`
3. Extract builtins to `builtins.rs`
4. Extract struct handling to `structs.rs`
5. Extract protocol dispatch to `protocols.rs`
6. Extract atom ops to `atoms.rs`
7. Extract collections to `collections.rs`
8. Simplify `mod.rs` to just dispatch
9. Thread context through all functions
10. Move tests to `tests.rs`

## Acceptance Criteria

- [ ] No file in codegen/ exceeds 300 lines
- [ ] No thread-local state (all in CodegenContext)
- [ ] All existing tests pass
- [ ] Context passed explicitly everywhere

## Summary
Create the liar compiler crate that parses liar source and emits lIR.

## Architecture
```
liar/
  Cargo.toml
  src/
    lib.rs           # Public API
    lexer.rs         # Tokenization
    parser.rs        # S-expr parsing → AST
    ast.rs           # liar AST types
    resolve.rs       # Name resolution
    types.rs         # Type representation
    infer.rs         # Type inference
    ownership.rs     # Borrow checking
    closures.rs      # Closure analysis (capture, color)
    codegen.rs       # AST → lIR emission
    error.rs         # Compiler errors with spans
    span.rs          # Source locations
```

## Cargo.toml
```toml
[package]
name = "liar"
version = "0.1.0"
edition = "2021"

[lib]
name = "liar"
path = "src/lib.rs"

[[bin]]
name = "liarc"
path = "src/bin/main.rs"

[dependencies]
lir-core = { path = "../lir-core" }
```

## Compiler pipeline
```
Source → Lexer → Tokens
              ↓
         Parser → AST
              ↓
         Resolve → AST with resolved names
              ↓
         Infer → Typed AST
              ↓
         Ownership → Verified AST (borrow-checked)
              ↓
         Closures → AST with closure info
              ↓
         Codegen → lIR items
```

## Public API
```rust
// src/lib.rs
pub fn compile(source: &str) -> Result<Vec<lir_core::Item>, Error>;
pub fn compile_to_string(source: &str) -> Result<String, Error>;
```

## Acceptance criteria
- [ ] Crate compiles
- [ ] Pipeline skeleton in place
- [ ] Can parse `(+ 1 2)` and emit lIR

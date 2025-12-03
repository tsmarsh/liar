## Summary

Refactor liar's codegen from string manipulation to direct lIR AST construction.

**Current (bad):**
```
liar AST → format!() strings → lIR string → Parser → lIR AST → LLVM
```

**Target (good):**
```
liar AST → lIR AST → LLVM
```

## Why This Matters

1. **Type safety** — Compiler catches malformed lIR at build time, not runtime
2. **Performance** — No serialize/parse round-trip
3. **Debuggability** — Can inspect/print lIR AST at any point
4. **Maintainability** — No string escaping bugs, no manual temp variable naming
5. **Enables optimization** — Can transform lIR AST before emitting LLVM

## Current State Analysis

### liar/src/codegen.rs

Currently ~1100 lines of string formatting:

```rust
// Current: String manipulation
Expr::Atom(value) => {
    let value = generate_expr(value)?;
    Ok(format!(
        "(let ((_atom (rc-alloc i64))) (atomic-store seq_cst {} (rc-ptr _atom)) _atom)",
        value
    ))
}

Expr::If(cond, then, else_) => {
    let cond = generate_expr(cond)?;
    let then = generate_expr(then)?;
    let else_ = generate_expr(else_)?;
    Ok(format!("(select {} {} {})", cond, then, else_))
}
```

### lir-core/src/ast.rs

The lIR AST already exists and is well-defined:

```rust
pub enum Expr {
    // Literals
    I1(bool),
    I8(i8),
    I64(i64),
    Double(f64),
    // ...
    
    // Operations
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    // ...
    
    // Memory
    Alloca(Type),
    Load(Type, Box<Expr>),
    Store(Box<Expr>, Box<Expr>),
    // ...
    
    // Atomics
    AtomicLoad { ordering: Ordering, ty: Type, ptr: Box<Expr> },
    AtomicStore { ordering: Ordering, value: Box<Expr>, ptr: Box<Expr> },
    AtomicRMW { op: RMWOp, ordering: Ordering, ptr: Box<Expr>, value: Box<Expr> },
    CmpXchg { ordering: Ordering, ptr: Box<Expr>, expected: Box<Expr>, new: Box<Expr> },
    
    // Control flow
    Select(Box<Expr>, Box<Expr>, Box<Expr>),
    // ...
    
    // Bindings
    Let { name: String, value: Box<Expr>, body: Box<Expr> },
    Var(String),
    
    // RC
    RcAlloc(Type),
    RcClone(Box<Expr>),
    RcDrop(Box<Expr>),
    RcPtr(Box<Expr>),
    RcCount(Box<Expr>),
}
```

## Implementation Plan

### Phase 1: Setup (~30 min)

1. Add `lir-core` as dependency to `liar/Cargo.toml`:
   ```toml
   [dependencies]
   lir-core = { path = "../lir-core" }
   ```

2. Create fresh variable generator in `liar/src/codegen.rs`:
   ```rust
   use std::sync::atomic::{AtomicUsize, Ordering};
   
   static VAR_COUNTER: AtomicUsize = AtomicUsize::new(0);
   
   fn fresh_var(prefix: &str) -> String {
       let n = VAR_COUNTER.fetch_add(1, Ordering::SeqCst);
       format!("_{}_{}", prefix, n)
   }
   
   pub fn reset_var_counter() {
       VAR_COUNTER.store(0, Ordering::SeqCst);
   }
   ```

3. Change function signature:
   ```rust
   // Old
   pub fn generate(program: &Program) -> Result<String>
   
   // New
   pub fn generate(program: &Program) -> Result<lir_core::ast::Module>
   ```

### Phase 2: Core Expression Translation (~2 hours)

Replace each `format!()` call with AST construction. Work through systematically:

#### Literals

```rust
// Old
Expr::Int(n) => Ok(format!("(i64 {})", n)),
Expr::Float(f) => Ok(format!("(double {})", f)),
Expr::Bool(b) => Ok(format!("(i1 {})", if *b { 1 } else { 0 })),

// New
Expr::Int(n) => Ok(lir::Expr::I64(*n)),
Expr::Float(f) => Ok(lir::Expr::Double(*f)),
Expr::Bool(b) => Ok(lir::Expr::I1(*b)),
```

#### Variables

```rust
// Old
Expr::Var(name) => Ok(name.clone()),

// New
Expr::Var(name) => Ok(lir::Expr::Var(name.clone())),
```

#### Arithmetic (via Call)

```rust
// Old (in generate_call)
"+" => Ok(format!("(add {} {})", args[0], args[1])),
"-" => Ok(format!("(sub {} {})", args[0], args[1])),

// New
"+" => Ok(lir::Expr::Add(Box::new(args[0].clone()), Box::new(args[1].clone()))),
"-" => Ok(lir::Expr::Sub(Box::new(args[0].clone()), Box::new(args[1].clone()))),
```

#### Control Flow

```rust
// Old
Expr::If(cond, then, else_) => {
    Ok(format!("(select {} {} {})", 
        generate_expr(cond)?, 
        generate_expr(then)?, 
        generate_expr(else_)?))
}

// New
Expr::If(cond, then, else_) => {
    Ok(lir::Expr::Select(
        Box::new(generate_expr(cond)?),
        Box::new(generate_expr(then)?),
        Box::new(generate_expr(else_)?),
    ))
}
```

#### Let Bindings

```rust
// Old
Expr::Let(bindings, body) => {
    let mut result = generate_expr(body)?;
    for binding in bindings.iter().rev() {
        let value = generate_expr(&binding.value)?;
        result = format!("(let (({} {})) {})", binding.name.node, value, result);
    }
    Ok(result)
}

// New
Expr::Let(bindings, body) => {
    let mut result = generate_expr(body)?;
    for binding in bindings.iter().rev() {
        let value = generate_expr(&binding.value)?;
        result = lir::Expr::Let {
            name: binding.name.node.clone(),
            value: Box::new(value),
            body: Box::new(result),
        };
    }
    Ok(result)
}
```

#### Atoms

```rust
// Old
Expr::Atom(value) => {
    let value = generate_expr(value)?;
    Ok(format!(
        "(let ((_atom (rc-alloc i64))) (atomic-store seq_cst {} (rc-ptr _atom)) _atom)",
        value
    ))
}

// New
Expr::Atom(value) => {
    let value_expr = generate_expr(value)?;
    let atom_var = fresh_var("atom");
    
    Ok(lir::Expr::Let {
        name: atom_var.clone(),
        value: Box::new(lir::Expr::RcAlloc(lir::Type::I64)),
        body: Box::new(lir::Expr::Seq(vec![
            lir::Expr::AtomicStore {
                ordering: lir::Ordering::SeqCst,
                value: Box::new(value_expr),
                ptr: Box::new(lir::Expr::RcPtr(Box::new(lir::Expr::Var(atom_var.clone())))),
            },
            lir::Expr::Var(atom_var),
        ])),
    })
}
```

#### AtomDeref (@)

```rust
// Old
Expr::AtomDeref(atom) => {
    let atom = generate_expr(atom)?;
    Ok(format!("(atomic-load seq_cst i64 (rc-ptr {}))", atom))
}

// New
Expr::AtomDeref(atom) => {
    let atom_expr = generate_expr(atom)?;
    Ok(lir::Expr::AtomicLoad {
        ordering: lir::Ordering::SeqCst,
        ty: lir::Type::I64,
        ptr: Box::new(lir::Expr::RcPtr(Box::new(atom_expr))),
    })
}
```

#### Swap!

```rust
// Old
Expr::Swap(atom, func) => {
    let atom = generate_expr(atom)?;
    let func = generate_expr(func)?;
    Ok(format!(
        "(let ((_ptr (rc-ptr {}))) \
           (let ((_old (atomic-load seq_cst i64 _ptr))) \
             (let ((_new (call @{} _old))) \
               (atomic-store seq_cst _new _ptr) \
               _new)))",
        atom, func
    ))
}

// New
Expr::Swap(atom, func) => {
    let atom_expr = generate_expr(atom)?;
    let func_expr = generate_expr(func)?;
    
    let ptr_var = fresh_var("ptr");
    let old_var = fresh_var("old");
    let new_var = fresh_var("new");
    
    Ok(lir::Expr::Let {
        name: ptr_var.clone(),
        value: Box::new(lir::Expr::RcPtr(Box::new(atom_expr))),
        body: Box::new(lir::Expr::Let {
            name: old_var.clone(),
            value: Box::new(lir::Expr::AtomicLoad {
                ordering: lir::Ordering::SeqCst,
                ty: lir::Type::I64,
                ptr: Box::new(lir::Expr::Var(ptr_var.clone())),
            }),
            body: Box::new(lir::Expr::Let {
                name: new_var.clone(),
                value: Box::new(lir::Expr::Call {
                    func: Box::new(func_expr),
                    args: vec![lir::Expr::Var(old_var)],
                }),
                body: Box::new(lir::Expr::Seq(vec![
                    lir::Expr::AtomicStore {
                        ordering: lir::Ordering::SeqCst,
                        value: Box::new(lir::Expr::Var(new_var.clone())),
                        ptr: Box::new(lir::Expr::Var(ptr_var)),
                    },
                    lir::Expr::Var(new_var),
                ])),
            }),
        }),
    })
}
```

### Phase 3: Function Definitions (~1 hour)

```rust
// Old
fn generate_defun(defun: &Defun) -> Result<String> {
    let body = generate_expr(&defun.body)?;
    let params = defun.params.iter()
        .map(|p| format!("({} {})", infer_type(p), p.name.node))
        .collect::<Vec<_>>()
        .join(" ");
    Ok(format!("(define ({} {}) ({}) (block entry {}))",
        defun.name.node, return_type, params, body))
}

// New
fn generate_defun(defun: &Defun) -> Result<lir::FunctionDef> {
    let body = generate_expr(&defun.body)?;
    
    Ok(lir::FunctionDef {
        name: defun.name.node.clone(),
        return_type: infer_return_type(defun),
        params: defun.params.iter().map(|p| lir::Param {
            name: p.name.node.clone(),
            ty: infer_param_type(p),
        }).collect(),
        body: lir::Block {
            label: "entry".to_string(),
            body: body,
        },
    })
}
```

### Phase 4: Module/Program Level (~30 min)

```rust
pub fn generate(program: &Program) -> Result<lir::Module> {
    reset_var_counter();
    
    let mut functions = Vec::new();
    let mut structs = Vec::new();
    
    for item in &program.items {
        match &item.node {
            Item::Defun(defun) => {
                functions.push(generate_defun(defun)?);
            }
            Item::Defstruct(defstruct) => {
                structs.push(generate_struct(defstruct)?);
            }
            // ...
        }
    }
    
    Ok(lir::Module { functions, structs })
}
```

### Phase 5: Update Callers (~30 min)

#### liar/src/lib.rs

```rust
// Old
pub fn compile(source: &str) -> Result<String, Vec<CompileError>> {
    // ...
    codegen::generate(&program).map_err(|e| vec![e])
}

// New  
pub fn compile(source: &str) -> Result<lir_core::ast::Module, Vec<CompileError>> {
    // ...
    codegen::generate(&program).map_err(|e| vec![e])
}

// Add convenience function for string output (tests, debugging)
pub fn compile_to_string(source: &str) -> Result<String, Vec<CompileError>> {
    let module = compile(source)?;
    Ok(module.to_string())  // Implement Display for lir::Module
}
```

#### liar/src/bin/main.rs

```rust
fn main() {
    let source = std::fs::read_to_string(&args.file)?;
    let module = liar::compile(&source)?;
    
    // Option 1: Print lIR
    println!("{}", module);
    
    // Option 2: Direct to LLVM (future)
    // let llvm_ir = lir_codegen::emit(&module)?;
}
```

### Phase 6: Add Display for lIR AST (~1 hour)

If not already present, add `Display` impl for `lir_core::ast::Module` and friends:

```rust
// lir-core/src/ast.rs or lir-core/src/display.rs

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::I64(n) => write!(f, "(i64 {})", n),
            Expr::Double(n) => write!(f, "(double {})", n),
            Expr::I1(b) => write!(f, "(i1 {})", if *b { 1 } else { 0 }),
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Add(a, b) => write!(f, "(add {} {})", a, b),
            Expr::Let { name, value, body } => {
                write!(f, "(let (({} {})) {})", name, value, body)
            }
            // ... etc
        }
    }
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for func in &self.functions {
            writeln!(f, "{}", func)?;
        }
        Ok(())
    }
}
```

This gives us the best of both worlds:
- Internal representation is AST (type-safe, transformable)
- Can still print to string for debugging/testing

## Testing Strategy

1. **Unit tests**: Each `generate_*` function should have tests comparing AST output
2. **Integration tests**: Compile liar source, print to string, compare with expected lIR
3. **End-to-end tests**: Existing cucumber features should still pass (via string output)

```rust
#[test]
fn test_generate_int() {
    let liar_expr = liar::ast::Expr::Int(42);
    let lir_expr = generate_expr(&liar_expr).unwrap();
    assert_eq!(lir_expr, lir::Expr::I64(42));
}

#[test]
fn test_generate_atom() {
    let liar_expr = liar::ast::Expr::Atom(Box::new(liar::ast::Expr::Int(0)));
    let lir_expr = generate_expr(&liar_expr).unwrap();
    // Verify structure without caring about exact variable names
    assert!(matches!(lir_expr, lir::Expr::Let { .. }));
}
```

## Expressions to Convert (Checklist)

Core:
- [ ] Int, Float, Bool, String, Nil
- [ ] Var
- [ ] Call (including arithmetic operators)
- [ ] Lambda
- [ ] Let, Plet
- [ ] If
- [ ] Do (sequence)
- [ ] Set!
- [ ] Ref, RefMut, Deref

Structs:
- [ ] Struct literal
- [ ] Field access

Pattern matching:
- [ ] Match

Atoms (ADR-011):
- [ ] Atom
- [ ] AtomDeref (@)
- [ ] Reset!
- [ ] Swap!
- [ ] CompareAndSet!

Collections (ADR-018) - stub for now:
- [ ] Vector
- [ ] Map
- [ ] Keyword
- [ ] ConvVector
- [ ] ConvMap

Async (ADR-014) - stub for now:
- [ ] Async
- [ ] Await

SIMD (ADR-016) - stub for now:
- [ ] SimdVector

STM (ADR-012) - stub for now:
- [ ] Dosync
- [ ] RefSetStm
- [ ] Alter
- [ ] Commute

Iterator - stub for now:
- [ ] Iter
- [ ] Collect

Other:
- [ ] Quote
- [ ] Unsafe
- [ ] ByteArray
- [ ] Regex
- [ ] Boxed
- [ ] Wrapping

## Acceptance Criteria

- [ ] `liar/Cargo.toml` depends on `lir-core`
- [ ] `codegen::generate()` returns `lir_core::ast::Module`
- [ ] Fresh variable generator exists and is used
- [ ] All `format!()` calls in codegen.rs are replaced with AST construction
- [ ] `Display` implemented for lIR AST types
- [ ] `liar::compile_to_string()` convenience function exists
- [ ] All existing tests pass
- [ ] Unit tests for each expression type

## Notes for Agent

- Work methodically through the expression types
- Run tests frequently (`cargo test -p liar`)
- If lIR AST is missing a variant you need, add it to `lir-core/src/ast.rs`
- Keep the old string-based code commented out until tests pass, then delete
- The `Seq` variant in lIR might not exist — may need to add it for multi-expression bodies

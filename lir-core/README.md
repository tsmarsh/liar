# lIR Core

Core library for lIR: an S-expression assembler for LLVM IR.

## Architecture

lIR is a stable, complete compilation target. It sits between high-level languages (like liar) and LLVM:

```
┌─────────────────────────────────────────┐
│           Higher-level language          │
│  (closures, protocols, atoms, etc.)      │
│                                          │
│  Compiler emits lIR AST nodes            │
└────────────────┬────────────────────────┘
                 │ lIR AST (Rust types)
                 ▼
┌─────────────────────────────────────────┐
│                lIR Core                  │
│  Generic primitives only                 │
│  No high-level concepts                  │
│  Standalone, testable in isolation       │
└────────────────┬────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────┐
│            LLVM via Inkwell              │
└─────────────────────────────────────────┘
```

## Design Principles

### What lIR IS

- **1:1 LLVM IR mapping**: Every lIR construct maps directly to LLVM IR
- **Explicit types**: No implicit conversions, no type inference
- **Generic primitives**: `IndirectCall`, `StructLit`, `ExtractValue` - not language-specific
- **Standalone**: Can be used without any higher-level language
- **Testable**: All features testable with pure lIR code

### What lIR is NOT

lIR has no knowledge of:
- Closures or lambda lifting
- Protocols or method dispatch
- Atoms or concurrency primitives (beyond LLVM atomics)
- Type inference or polymorphism
- Garbage collection or ownership
- Macros or syntax transformation

These are concerns for higher-level languages that compile *to* lIR.

## Boundary Rules

### In lir-core and lir-codegen:
1. **No higher-level imports** - lir-core depends only on thiserror
2. **No higher-level terminology** - Use LLVM/lIR terms, not language-specific ones
3. **Generic primitives only** - `IndirectCall` not `ClosureCall`

### For languages targeting lIR:
1. **No lIR modification required** - If you need a new primitive, it belongs in LLVM IR
2. **All abstraction in your compiler** - lIR is just the target format
3. **Codegen is translation** - Your compiler translates to lIR AST nodes

## Implementing Higher-Level Features via lIR

### Closures → Struct + IndirectCall

A closure `(fn (x) (+ captured x))` compiles to:

```lisp
; Environment struct
(defstruct __env_0 (i64))

; Lifted function (takes env as first param)
(define (__lambda_0 i64) ((ptr __env) (i64 x))
  (block entry
    (let ((captured (load i64 (getelementptr %struct.__env_0 __env (i64 0) (i32 0)))))
      (ret (add captured x)))))

; Closure creation: { fn_ptr, env_ptr }
(let ((env (call @malloc (i64 8))))
  (store captured_value (getelementptr %struct.__env_0 env (i64 0) (i32 0)))
  { @__lambda_0 env })

; Closure call: extract fn_ptr and env_ptr, then indirect call
(let ((fn_ptr (extractvalue closure 0))
      (env_ptr (extractvalue closure 1)))
  (indirect-call fn_ptr i64 (env_ptr arg)))
```

### Protocol Dispatch → Static Function Call

Protocol method `(greet person)` compiles to a direct call to the implementation:

```lisp
(call @__Greet_Person__greet person)
```

### Atoms → Atomic Operations

Atom operations compile to LLVM atomic instructions:

```lisp
; (atom 0) → allocate + atomic store
(let ((ptr (call @malloc (i64 8))))
  (atomic-store seq_cst (i64 0) ptr)
  ptr)

; @atom → atomic load
(atomic-load seq_cst i64 ptr)

; (reset! atom val) → atomic store
(atomic-store seq_cst val ptr)
```

## Module Structure

```
lir-core/
├── src/
│   ├── lib.rs      # Public API
│   ├── ast.rs      # AST types (Expr, Item, ScalarType, etc.)
│   ├── parser.rs   # S-expression parser
│   ├── display.rs  # Pretty printing
│   ├── types.rs    # Type checking
│   ├── borrow.rs   # Borrow checking (experimental)
│   └── error.rs    # Error types
└── README.md       # This file
```

## Usage

Add to your `Cargo.toml`:

```toml
[dependencies]
lir-core = { path = "../lir-core" }
```

Parse and type-check lIR source:

```rust
use lir_core::parser::Parser;
use lir_core::types::TypeChecker;

let source = "(add (i32 1) (i32 2))";
let mut parser = Parser::new(source)?;
let expr = parser.parse_expr()?;

let mut checker = TypeChecker::new();
let ty = checker.check_expr(&expr)?;
// ty == ScalarType::I32
```

Build lIR AST directly (for compilers):

```rust
use lir_core::ast::{Expr, ScalarType};

let expr = Expr::Add(
    Box::new(Expr::IntLit { ty: ScalarType::I32, value: 1 }),
    Box::new(Expr::IntLit { ty: ScalarType::I32, value: 2 }),
);
```

## See Also

- [lIR Language Guide](../doc/lIR.md) - Full language reference
- [lir-codegen](../lir-codegen) - LLVM code generation

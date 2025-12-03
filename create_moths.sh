#!/bin/bash

# =============================================================================
# Moths for liar compiler
# =============================================================================

moth new "liar: compiler crate structure" -s crit --no-edit --stdin << 'EOF'
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
EOF

# =============================================================================

moth new "liar: lexer" -s high --no-edit --stdin << 'EOF'
## Summary
Tokenize liar source into tokens with spans for error reporting.

## Token types
```rust
pub enum Token {
    // Delimiters
    LParen,           // (
    RParen,           // )
    LBracket,         // [
    RBracket,         // ]
    LBrace,           // {
    RBrace,           // }
    
    // Literals
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    
    // Identifiers and keywords
    Symbol(String),   // foo, bar, +, -, etc.
    Keyword(String),  // :foo
    
    // Special
    Quote,            // '
    Quasiquote,       // `
    Unquote,          // ,
    UnquoteSplice,    // ,@
    Ampersand,        // &
    AmpersandMut,     // &mut
    Caret,            // ^
    
    // SIMD vector delimiters
    DoubleLAngle,     // <<
    DoubleRAngle,     // >>
}
```

## Span tracking
```rust
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub file: Option<String>,
}

pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

pub type SpannedToken = Spanned<Token>;
```

## Lexer API
```rust
pub struct Lexer<'a> {
    source: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self;
    pub fn next_token(&mut self) -> Result<Option<SpannedToken>, LexError>;
    pub fn tokenize_all(&mut self) -> Result<Vec<SpannedToken>, LexError>;
}
```

## Special cases
- Comments: `;` to end of line
- Nested comments: `#| ... |#` (optional)
- String escapes: `\n`, `\t`, `\\`, `\"`, `\xHH`
- Character literals: `\a`, `\newline`, `\space`
- Numeric literals: `42`, `-42`, `3.14`, `0xFF`, `0b1010`

## Acceptance criteria
- [ ] All token types recognized
- [ ] Spans are accurate
- [ ] Comments skipped
- [ ] String escapes handled
- [ ] Hex/binary literals work
EOF

# =============================================================================

moth new "liar: parser and AST" -s high --no-edit --stdin << 'EOF'
## Summary
Parse tokens into liar AST. S-expression based with special forms.

## AST types
```rust
pub enum Expr {
    // Literals
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Char(char),
    Symbol(String),
    Keyword(String),
    Nil,
    
    // Collections
    List(Vec<Expr>),            // '(1 2 3)
    Vector(Vec<Expr>),          // [1 2 3]
    Map(Vec<(Expr, Expr)>),     // {:a 1 :b 2}
    SimdVector(Vec<Expr>),      // <<1 2 3 4>>
    
    // References
    Borrow(Box<Expr>),          // &x
    BorrowMut(Box<Expr>),       // &mut x
    Move(Box<Expr>),            // ^x
    
    // Special forms
    If {
        cond: Box<Expr>,
        then_: Box<Expr>,
        else_: Option<Box<Expr>>,
    },
    Let {
        bindings: Vec<(Pattern, Expr)>,
        body: Vec<Expr>,
    },
    Fn {
        params: Vec<Param>,
        body: Vec<Expr>,
    },
    Do(Vec<Expr>),
    Quote(Box<Expr>),
    Quasiquote(Box<Expr>),
    Unquote(Box<Expr>),
    UnquoteSplice(Box<Expr>),
    
    // Application
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    
    // Pattern matching
    Match {
        expr: Box<Expr>,
        arms: Vec<(Pattern, Expr)>,
    },
    
    // Loops
    Loop {
        bindings: Vec<(String, Expr)>,
        body: Vec<Expr>,
    },
    Recur(Vec<Expr>),
}

pub enum Pattern {
    Wildcard,                   // _
    Binding(String),            // x
    BorrowBinding(String),      // &x
    Literal(Expr),              // 42, "foo"
    Constructor {               // (Some x), (Point x y)
        name: String,
        fields: Vec<Pattern>,
    },
    Vector(Vec<Pattern>),       // [a b c]
    VectorRest {                // [a b . rest]
        init: Vec<Pattern>,
        rest: String,
    },
}

pub struct Param {
    pub name: String,
    pub mode: ParamMode,
    pub ty: Option<Type>,       // optional type annotation
}

pub enum ParamMode {
    Move,       // default
    Borrow,     // &
    BorrowMut,  // &mut
}
```

## Top-level forms
```rust
pub enum TopLevel {
    Define {
        name: String,
        params: Vec<Param>,
        body: Vec<Expr>,
    },
    DefStruct {
        name: String,
        fields: Vec<(String, Option<Type>)>,
    },
    DefEnum {
        name: String,
        variants: Vec<Variant>,
    },
    DefMacro {
        name: String,
        params: Vec<String>,
        body: Expr,
    },
    Expr(Expr),
}
```

## Parser API
```rust
pub struct Parser<'a> {
    tokens: &'a [SpannedToken],
    pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [SpannedToken]) -> Self;
    pub fn parse_expr(&mut self) -> Result<Expr, ParseError>;
    pub fn parse_top_level(&mut self) -> Result<TopLevel, ParseError>;
    pub fn parse_program(&mut self) -> Result<Vec<TopLevel>, ParseError>;
}
```

## Acceptance criteria
- [ ] All expression types parsed
- [ ] Special forms recognized (if, let, fn, do, match, loop)
- [ ] Patterns parsed
- [ ] Top-level forms parsed (define, defstruct, defenum, defmacro)
- [ ] Error messages include spans
EOF

# =============================================================================

moth new "liar: name resolution" -s high --no-edit --stdin << 'EOF'
## Summary
Resolve all names to their definitions. Detect undefined variables,
duplicate definitions, and build scope information.

## Scope tracking
```rust
pub struct Scope {
    bindings: HashMap<String, BindingInfo>,
    parent: Option<Box<Scope>>,
}

pub struct BindingInfo {
    pub kind: BindingKind,
    pub span: Span,
    pub id: BindingId,  // Unique ID for later passes
}

pub enum BindingKind {
    Local,
    Parameter,
    Function,
    Struct,
    Enum,
    EnumVariant,
    Macro,
}
```

## Resolution pass
```rust
pub struct Resolver {
    scopes: Vec<Scope>,
    current_function: Option<String>,
    errors: Vec<ResolveError>,
}

impl Resolver {
    pub fn resolve(&mut self, program: &[TopLevel]) -> Result<ResolvedProgram, Vec<ResolveError>>;
}
```

## What gets resolved
- Variable references → binding site
- Function calls → function definition
- Struct constructors → struct definition
- Enum variants → enum definition
- Forward references (mutual recursion)

## Errors detected
- Undefined variable
- Undefined function
- Duplicate definition in same scope
- Use of variable in own initializer (except recursion)

## Acceptance criteria
- [ ] All names resolved to binding sites
- [ ] Scopes correctly nested
- [ ] Forward references work for functions
- [ ] Undefined variable detected
- [ ] Duplicate definition detected
EOF

# =============================================================================

moth new "liar: type system and inference" -s high --no-edit --stdin << 'EOF'
## Summary
Implement type representation and Hindley-Milner style inference.
No explicit type annotations required (but allowed).

## Type representation
```rust
pub enum Type {
    // Primitives
    Int(IntSize),       // i8, i16, i32, i64
    Float(FloatSize),   // f32, f64
    Bool,
    Char,
    String,
    Unit,               // ()
    
    // References
    Ref(Box<Type>),     // &T
    RefMut(Box<Type>),  // &mut T
    
    // Collections
    List(Box<Type>),
    Vector(Box<Type>),
    Map(Box<Type>, Box<Type>),
    SimdVector(Box<Type>, usize),  // <T x N>
    
    // Functions
    Fn {
        params: Vec<Type>,
        ret: Box<Type>,
    },
    
    // User-defined
    Struct(String),
    Enum(String),
    
    // Inference
    Var(TypeVarId),     // Unresolved type variable
    
    // Special
    Never,              // ! (diverges)
    Error,              // Type error placeholder
}
```

## Inference algorithm
```rust
pub struct Inferencer {
    substitutions: HashMap<TypeVarId, Type>,
    constraints: Vec<Constraint>,
    next_var: TypeVarId,
}

pub enum Constraint {
    Equal(Type, Type),              // T1 = T2
    Subtype(Type, Type),            // T1 <: T2 (for numeric promotion)
}

impl Inferencer {
    pub fn fresh_var(&mut self) -> Type;
    pub fn unify(&mut self, t1: &Type, t2: &Type) -> Result<(), TypeError>;
    pub fn infer_expr(&mut self, expr: &Expr, env: &TypeEnv) -> Result<Type, TypeError>;
    pub fn solve(&mut self) -> Result<Substitution, TypeError>;
}
```

## Type rules
- Literals: `42` → `i64`, `3.14` → `f64`
- Arithmetic: operands must match, result is same type
- Comparison: operands must match, result is `Bool`
- If: branches must have same type
- Let: binding type inferred from initializer
- Fn: param types from usage, return type from body
- Call: function type must match arg types

## Numeric promotion (ADR-017)
- Integer literals polymorphic until constrained
- Explicit type wins: `(i32 42)` is `i32`
- Mixed operations: error (no implicit promotion)
- Explicit promotion: `(as i64 x)`

## Acceptance criteria
- [ ] All types representable
- [ ] Literal types inferred
- [ ] Arithmetic types unified
- [ ] Function types inferred from usage
- [ ] Unification algorithm works
- [ ] Good error messages for type mismatches
EOF

# =============================================================================

moth new "liar: ownership and borrow checking" -s crit --no-edit --stdin << 'EOF'
## Summary
Implement the borrow checker per ADRs 001-007. Track ownership state,
detect use-after-move, verify borrow rules.

## Ownership states
```rust
pub enum OwnershipState {
    Owned,              // Value is owned here
    Moved,              // Value has been moved away
    Borrowed(BorrowId), // Immutably borrowed
    BorrowedMut(BorrowId), // Mutably borrowed
    Shared,             // Reference counted (no move tracking)
}
```

## Borrow tracking
```rust
pub struct BorrowChecker {
    bindings: HashMap<BindingId, OwnershipState>,
    active_borrows: HashMap<BorrowId, BorrowInfo>,
    errors: Vec<BorrowError>,
}

pub struct BorrowInfo {
    pub kind: BorrowKind,
    pub borrowed_from: BindingId,
    pub span: Span,
    pub scope_end: ScopeId,
}
```

## Rules to enforce

### ADR-001: Immutability by default
- Values are immutable unless explicitly mutated via `&mut`

### ADR-002: Pass by reference  
- Function args are references under the hood (optimization)
- But semantically, value types move

### ADR-003: Mutable reference sigil
- `&x` = shared borrow
- `&mut x` = exclusive mutable borrow

### ADR-004: Lexical ownership
- Every value has one owner
- Owner controls lifetime
- When owner goes out of scope, value is dropped

### ADR-005: Closure captures
- Closures capture by move (default)
- Or by borrow if `&x` used in closure
- Closure cannot escape if it borrows

### ADR-006: No redefinition
- Cannot shadow in same scope
- Inner scope shadowing OK

### ADR-007: Aliasing allowed
- Multiple shared borrows OK
- Mutable borrow is exclusive
- No mutable + shared at same time

## Checker algorithm
```rust
impl BorrowChecker {
    pub fn check_function(&mut self, func: &Function) -> Result<(), Vec<BorrowError>>;
    
    fn check_expr(&mut self, expr: &Expr) -> Result<(), BorrowError>;
    fn check_move(&mut self, binding: BindingId, span: Span) -> Result<(), BorrowError>;
    fn check_borrow(&mut self, binding: BindingId, kind: BorrowKind, span: Span) -> Result<(), BorrowError>;
    fn end_borrow(&mut self, borrow: BorrowId);
    fn end_scope(&mut self, scope: ScopeId);
}
```

## Errors detected
- Use after move
- Move of borrowed value
- Mutable borrow while borrowed
- Double mutable borrow
- Borrow escapes scope
- Return reference to local

## Acceptance criteria
- [ ] Use-after-move detected
- [ ] Borrow rules enforced
- [ ] Mutable exclusivity enforced
- [ ] Borrow escape detected
- [ ] Drop insertion at scope end
- [ ] Good error messages with spans
EOF

# =============================================================================

moth new "liar: closure analysis" -s high --no-edit --stdin << 'EOF'
## Summary
Analyze closures for captures and color (ADR-010). Determine what
each closure captures and whether it's Send/Sync safe.

## Capture analysis
```rust
pub struct CaptureInfo {
    pub captured: Vec<Capture>,
    pub color: ClosureColor,
}

pub struct Capture {
    pub binding: BindingId,
    pub mode: CaptureMode,
}

pub enum CaptureMode {
    Move,       // Closure owns the value
    Borrow,     // Closure borrows (cannot escape)
    Clone,      // Closure owns a clone
}
```

## Closure color (ADR-010)
```rust
pub enum ClosureColor {
    Pure,       // No captures, can go anywhere
    Local,      // Captures borrows, cannot escape scope
    Sync,       // Captures only Send+Sync values, thread-safe
    NonSync,    // Captures non-Send values, single-threaded only
}
```

Color propagation:
- Pure + anything = that thing
- Local + anything = Local
- Sync + Sync = Sync
- NonSync + anything = NonSync

## Analysis pass
```rust
pub struct ClosureAnalyzer {
    current_scope_bindings: HashSet<BindingId>,
}

impl ClosureAnalyzer {
    pub fn analyze_closure(&mut self, closure: &Fn, env: &Env) -> CaptureInfo;
    fn find_free_variables(&self, body: &[Expr]) -> Vec<BindingId>;
    fn determine_capture_mode(&self, binding: BindingId, usage: &Usage) -> CaptureMode;
    fn compute_color(&self, captures: &[Capture]) -> ClosureColor;
}
```

## Code generation implications
- Move capture: value moved into closure struct
- Borrow capture: closure has lifetime bound to scope
- Clone capture: clone called, closure owns copy
- Color affects where closure can be passed (spawn, async, etc.)

## Acceptance criteria
- [ ] Free variables identified
- [ ] Capture mode determined correctly
- [ ] Closure color computed
- [ ] Borrow captures prevent escape
- [ ] Color propagates through nested closures
EOF

# =============================================================================

moth new "liar: codegen to lIR" -s crit --no-edit --stdin << 'EOF'
## Summary
Generate lIR from typed, ownership-checked AST.

## Code generation context
```rust
pub struct CodeGen {
    items: Vec<lir_core::Item>,
    current_function: Option<FunctionBuilder>,
    locals: HashMap<BindingId, String>,  // binding → lIR variable name
    label_counter: usize,
    temp_counter: usize,
}

struct FunctionBuilder {
    name: String,
    params: Vec<(String, lir_core::ParamType)>,
    return_type: lir_core::ReturnType,
    blocks: Vec<BlockBuilder>,
    current_block: usize,
}
```

## Translation rules

### Primitives
```lisp
; liar                → lIR
42                   → (i64 42)
3.14                 → (double 3.14)
true                 → (i1 1)
false                → (i1 0)
```

### Arithmetic
```lisp
; liar                → lIR
(+ a b)              → (add a b)      ; integers
(+ a b)              → (fadd a b)     ; floats
(- a b)              → (sub a b)
(* a b)              → (mul a b)
(/ a b)              → (sdiv a b)     ; signed
(rem a b)            → (srem a b)
```

### Comparison
```lisp
; liar                → lIR
(= a b)              → (icmp eq a b)
(< a b)              → (icmp slt a b)
(> a b)              → (icmp sgt a b)
(<= a b)             → (icmp sle a b)
(>= a b)             → (icmp sge a b)
```

### Conditionals
```lisp
; liar
(if cond then else)

; lIR
(block entry
  (br cond then_label else_label))
(block then_label
  ... then code ...
  (br merge))
(block else_label
  ... else code ...
  (br merge))
(block merge
  (ret (phi T (then_label then_val) (else_label else_val))))
```

### Let bindings
```lisp
; liar
(let ((x 10) (y 20))
  (+ x y))

; lIR
(let ((x (i64 10))
      (y (i64 20)))
  (add x y))
```

### Functions
```lisp
; liar
(define (add a b)
  (+ a b))

; lIR
(define (add i64) ((i64 a) (i64 b))
  (block entry
    (ret (add a b))))
```

### Closures
Closures become a struct (environment) + function:

```lisp
; liar
(let ((x 10))
  (fn (y) (+ x y)))

; lIR
(defstruct closure_env_0 (i64))  ; captured x

(define (closure_fn_0 i64) ((ptr env) (i64 y))
  (block entry
    (let ((x (load i64 (getelementptr %struct.closure_env_0 env (i64 0) (i32 0)))))
      (ret (add x y)))))

; At closure creation site:
(let ((env (alloca i64)))
  (store (i64 10) env)
  ; return {env, closure_fn_0} as closure object
  ...)
```

### Structs
```lisp
; liar
(defstruct Point x y)
(Point 10 20)
(Point-x p)

; lIR
(defstruct Point (i64 i64))

; Constructor - allocate and populate
(let ((p (alloca i64 (i32 2))))
  (store (i64 10) (getelementptr %struct.Point p (i64 0) (i32 0)))
  (store (i64 20) (getelementptr %struct.Point p (i64 0) (i32 1)))
  p)

; Accessor
(load i64 (getelementptr %struct.Point p (i64 0) (i32 0)))
```

### Pattern matching
Compiles to nested conditionals:

```lisp
; liar
(match x
  (0 "zero")
  (1 "one")
  (_ "many"))

; lIR
(block entry
  (br (icmp eq x (i64 0)) case_0 check_1))
(block check_1
  (br (icmp eq x (i64 1)) case_1 case_default))
(block case_0
  (br merge))
(block case_1
  (br merge))
(block case_default
  (br merge))
(block merge
  (ret (phi ptr (case_0 str_zero) (case_1 str_one) (case_default str_many))))
```

### Memory management
Insert drops at scope exits:

```lisp
; liar
(let ((x (make-big-thing)))
  (use x))
; x dropped here

; lIR
(let ((x (call @make_big_thing)))
  (call @use x)
  (call @drop_big_thing x))  ; inserted by compiler
```

## Acceptance criteria
- [ ] All expression types compile
- [ ] Functions compile with correct signatures
- [ ] Closures compile to struct + function
- [ ] Structs compile to lIR structs
- [ ] Pattern matching compiles to branches
- [ ] Drop calls inserted at scope exits
- [ ] Output is valid lIR (parseable by lir-core)
EOF

# =============================================================================

moth new "liar: CLI binary (liarc)" -s med --no-edit --stdin << 'EOF'
## Summary
Create the liar compiler CLI that compiles .liar to .lir.

## Usage
```bash
# Compile to lIR (stdout)
liarc input.liar

# Compile to lIR file
liarc input.liar -o output.lir

# Compile and assemble (requires lair)
liarc input.liar --emit=obj -o output.o
liarc input.liar --emit=exe -o program

# Check only (no output)
liarc input.liar --check

# Verbose (show passes)
liarc input.liar -v
```

## Options
```rust
#[derive(Parser)]
pub struct Args {
    /// Input .liar file
    pub input: PathBuf,
    
    /// Output file (default: stdout)
    #[arg(short, long)]
    pub output: Option<PathBuf>,
    
    /// Output format
    #[arg(long, default_value = "lir")]
    pub emit: Emit,
    
    /// Type check only, no codegen
    #[arg(long)]
    pub check: bool,
    
    /// Verbose output
    #[arg(short, long)]
    pub verbose: bool,
    
    /// Dump AST
    #[arg(long)]
    pub dump_ast: bool,
    
    /// Dump typed AST
    #[arg(long)]
    pub dump_typed: bool,
}

pub enum Emit {
    Lir,    // .lir text
    Obj,    // .o (via lair)
    Exe,    // executable (via lair)
}
```

## Error output
Errors should be human-readable with source locations:

```
error[E0382]: use of moved value: `x`
 --> input.liar:10:5
  |
8 |     (consume x)
  |              - value moved here
9 |     ...
10|     x
  |     ^ value used here after move
  |
  = note: move occurs because `x` has type `Pair`, which does not implement `Copy`
```

## Acceptance criteria
- [ ] Compiles .liar to .lir
- [ ] --check validates without output
- [ ] Error messages have source locations
- [ ] --verbose shows pipeline stages
- [ ] Exit code 0 on success, 1 on error
EOF

echo ""
echo "Created 8 moths for the liar compiler:"
echo ""
echo "  1. [CRIT] Crate structure - skeleton"
echo "  2. [HIGH] Lexer - tokenization"
echo "  3. [HIGH] Parser/AST - s-expr parsing"
echo "  4. [HIGH] Name resolution - symbol lookup"
echo "  5. [HIGH] Type inference - H-M style"
echo "  6. [CRIT] Ownership/borrow checking - the hard part"
echo "  7. [HIGH] Closure analysis - captures, color"
echo "  8. [CRIT] Codegen - emit lIR"
echo "  9. [MED]  CLI binary - liarc"
echo ""
echo "Suggested implementation order:"
echo "  1 → 2 → 3 → 4 → 8 (minimal pipeline, no types/ownership)"
echo "  Then add: 5 → 6 → 7"
echo "  Finally: 9"
#!/bin/bash

# =============================================================================
# Moths to get lIR to A+ status
# =============================================================================

# -----------------------------------------------------------------------------
# CRITICAL: let bindings (blocks most tests)
# -----------------------------------------------------------------------------

moth new "lIR: let bindings for SSA values" -s crit --no-edit --stdin << 'EOF'
## Summary
Add `let` bindings to name intermediate SSA values. This is **critical** — most 
feature tests use `let` but it's not implemented.

## Syntax
```lisp
(let ((name1 expr1)
      (name2 expr2))
  body...)
```

## Examples
```lisp
; Simple binding
(let ((x (i32 42)))
  (ret x))

; Multiple bindings  
(let ((a (alloca i32))
      (b (i32 5)))
  (store b a)
  (ret (load i32 a)))

; Nested let
(let ((x (i32 1)))
  (let ((y (add x (i32 2))))
    (ret y)))

; With phi (from control_flow.feature)
(block loop
  (let ((i (phi i32 (entry (i32 0)) (loop next-i)))
        (acc (phi i32 (entry (i32 0)) (loop next-acc))))
    (let ((next-i (add i (i32 1)))
          (next-acc (add acc i)))
      (br (icmp sle i n) loop done))))
```

## Implementation

### AST addition
```rust
// In ast.rs, add to Expr enum:
Let {
    bindings: Vec<(String, Box<Expr>)>,  // (name, value)
    body: Vec<Expr>,                      // body expressions
}
```

### Parser addition
```rust
// In parse_form match:
"let" => self.parse_let(),

// New method:
fn parse_let(&mut self) -> Result<Expr, ParseError> {
    // Expect opening paren for bindings list
    self.expect(Token::LParen)?;
    
    let mut bindings = Vec::new();
    while !matches!(self.lexer.peek()?, Some(Token::RParen)) {
        self.expect(Token::LParen)?;
        let name = self.expect_ident()?;
        let value = self.parse_expr()?;
        self.expect(Token::RParen)?;
        bindings.push((name, Box::new(value)));
    }
    self.expect(Token::RParen)?;  // close bindings list
    
    // Parse body expressions until outer RParen
    let mut body = Vec::new();
    while !matches!(self.lexer.peek()?, Some(Token::RParen)) {
        body.push(self.parse_expr()?);
    }
    
    Ok(Expr::Let { bindings, body })
}
```

### Codegen addition
```rust
// In compile_expr_recursive or compile_expr_inner:
Expr::Let { bindings, body } => {
    let mut new_locals = locals.clone();
    
    // Evaluate each binding and add to locals
    for (name, expr) in bindings {
        let value = self.compile_expr_recursive(expr, &new_locals)?;
        new_locals.insert(name.clone(), value);
    }
    
    // Evaluate body expressions, return last
    let mut result = None;
    for expr in body {
        result = self.compile_expr_with_locals(expr, &new_locals)?;
    }
    
    result.ok_or_else(|| CodeGenError::CodeGen("empty let body".to_string()))
}
```

## Acceptance Criteria
- [ ] Add `Let` variant to `Expr` enum
- [ ] Add `parse_let()` to parser
- [ ] Add let handling to codegen with scope extension
- [ ] Feature tests in memory.feature pass
- [ ] Feature tests in control_flow.feature pass

## Notes
- This unblocks the majority of existing feature tests
- Let bindings extend the locals map, not replace it
- Body returns the value of the last expression
EOF

# -----------------------------------------------------------------------------
# HIGH: Type flexibility for memory operations
# -----------------------------------------------------------------------------

moth new "lIR: extend alloca/load/store to support all types" -s high --no-edit --stdin << 'EOF'
## Summary
Currently `Alloca`, `Load`, and `Store` only support `ScalarType`. They should
support `ptr` and potentially vectors/structs for full LLVM IR coverage.

## Current limitation
```rust
Alloca {
    ty: ScalarType,  // Can't alloca ptr or vector
    count: Option<Box<Expr>>,
}
Load {
    ty: ScalarType,  // Can't load ptr
    ptr: Box<Expr>,
}
```

## Proposed fix
```rust
// Option 1: Use ParamType (already exists, supports Scalar and Ptr)
Alloca {
    ty: ParamType,
    count: Option<Box<Expr>>,
}
Load {
    ty: ParamType,
    ptr: Box<Expr>,
}

// Option 2: Create a more comprehensive MemoryType
enum MemoryType {
    Scalar(ScalarType),
    Ptr,
    Vector(VectorType),
    Struct(String),  // Named struct reference
}
```

## Examples that should work
```lisp
; Alloca a pointer
(alloca ptr)

; Load a pointer from memory
(load ptr %ptr_to_ptr)

; Store a pointer
(store (ptr null) %dest)

; Alloca a vector (nice to have)
(alloca <4 x i32>)
```

## Acceptance Criteria
- [ ] `alloca ptr` works
- [ ] `load ptr %p` works  
- [ ] `store (ptr x) %p` works
- [ ] Update parser to accept ptr type in these positions
- [ ] Update codegen to generate correct LLVM types

## Notes
- ParamType already exists and may be sufficient
- Vectors and structs can be a follow-up if needed
EOF

# -----------------------------------------------------------------------------
# HIGH: GEP struct type support
# -----------------------------------------------------------------------------

moth new "lIR: getelementptr with struct types" -s high --no-edit --stdin << 'EOF'
## Summary
`GetElementPtr` currently only accepts `ScalarType` for the element type. For
struct field access (critical for closure environments), it needs to support
named struct types.

## Current limitation
```rust
GetElementPtr {
    ty: ScalarType,     // Can't reference %struct.point
    ptr: Box<Expr>,
    indices: Vec<Expr>,
    inbounds: bool,
}
```

## Required for closures
```lisp
(defstruct adder_env (i64))

; Access field 0 of the struct
(getelementptr %struct.adder_env %env (i32 0) (i32 0))
```

## Proposed fix
```rust
// New enum for GEP pointee types
enum GepType {
    Scalar(ScalarType),
    Struct(String),  // Named struct like "point" -> %struct.point
}

GetElementPtr {
    ty: GepType,
    ptr: Box<Expr>,
    indices: Vec<Expr>,
    inbounds: bool,
}
```

## Parser changes
```rust
fn parse_getelementptr(&mut self, _: bool) -> Result<Expr, ParseError> {
    // ... handle inbounds ...
    
    // Parse type - could be scalar or %struct.name
    let ty = match self.lexer.peek()? {
        Some(Token::Ident(s)) if s.starts_with("%struct.") => {
            self.lexer.next_token_peeked()?;
            GepType::Struct(s[8..].to_string())  // strip %struct. prefix
        }
        Some(Token::Ident(s)) => {
            self.lexer.next_token_peeked()?;
            GepType::Scalar(self.type_from_name(&s)?)
        }
        ...
    };
    ...
}
```

## Codegen changes
Need to look up struct type from module's type registry when generating GEP.

## Acceptance Criteria
- [ ] Parse `(getelementptr %struct.name ptr indices...)`
- [ ] Generate correct LLVM GEP for struct field access
- [ ] integration_milestone.feature struct scenarios pass

## Notes
- This is required for the closure simulation milestone test
- Struct types are defined with `defstruct` and registered in the module
EOF

# -----------------------------------------------------------------------------
# MEDIUM: Verify phi incoming order
# -----------------------------------------------------------------------------

moth new "lIR: audit phi node syntax and fix if needed" -s med --no-edit --stdin << 'EOF'
## Summary
Verify that phi node syntax is consistent between feature files, parser, and codegen.

## Current AST
```rust
Phi {
    ty: ScalarType,
    incoming: Vec<(String, Box<Expr>)>,  // (label, value) order
}
```

## Feature file syntax
```lisp
(phi i32 (neg (i32 0)) (pos x))
```

This appears to be `(label value)` pairs, which matches the AST.

## Verification needed
1. Check parser `parse_phi()` - does it parse `(label value)` or `(value label)`?
2. Check codegen - does it use the tuple in the right order?
3. Run phi tests to verify

## If there's a mismatch
Either:
- Fix parser to match feature file syntax
- Fix feature files to match parser expectation
- Fix codegen to use correct tuple order

## Acceptance Criteria
- [ ] Phi syntax documented in lIR.md
- [ ] Parser matches documented syntax
- [ ] Codegen uses correct order for `phi.add_incoming()`
- [ ] control_flow.feature phi scenarios pass
EOF

# -----------------------------------------------------------------------------
# MEDIUM: Function return type should support ptr
# -----------------------------------------------------------------------------

moth new "lIR: function definitions with ptr return type" -s med --no-edit --stdin << 'EOF'
## Summary
Function definitions use `ScalarType` for return type, but functions often
need to return pointers (e.g., `alloca`, factory functions).

## Current limitation
```rust
pub struct FunctionDef {
    pub name: String,
    pub return_type: ScalarType,  // Can't return ptr
    pub params: Vec<Param>,
    pub blocks: Vec<BasicBlock>,
}
```

## Example that should work
```lisp
(define (make_closure ptr) ((i64 x))
  (block entry
    (let ((env (call @malloc (i64 8))))
      (ret env))))
```

## Proposed fix
Use `ReturnType` (already exists for extern declarations):

```rust
pub struct FunctionDef {
    pub name: String,
    pub return_type: ReturnType,  // Scalar or Ptr
    pub params: Vec<Param>,
    pub blocks: Vec<BasicBlock>,
}
```

## Acceptance Criteria
- [ ] Parse `(define (name ptr) ...)` for ptr return
- [ ] Generate LLVM function with ptr return type
- [ ] memory.feature alloca tests pass (they return ptr)
EOF

# -----------------------------------------------------------------------------
# MEDIUM: Function parameters should support ptr
# -----------------------------------------------------------------------------

moth new "lIR: function parameters with ptr type" -s med --no-edit --stdin << 'EOF'
## Summary
Function parameters use `ScalarType` but need to support `ptr` for passing
pointers to functions.

## Current limitation
```rust
pub struct Param {
    pub ty: ScalarType,  // Can't be ptr
    pub name: String,
}
```

## Example that should work
```lisp
(define (read-ptr i32) ((ptr p))
  (block entry
    (ret (load i32 p))))
```

## Proposed fix
Use `ParamType`:

```rust
pub struct Param {
    pub ty: ParamType,  // Scalar or Ptr
    pub name: String,
}
```

## Acceptance Criteria
- [ ] Parse `((ptr name) ...)` in function parameters
- [ ] Generate LLVM function with ptr parameters
- [ ] memory.feature "read-ptr" test passes
EOF

# -----------------------------------------------------------------------------
# HIGH: Add integration milestone feature
# -----------------------------------------------------------------------------

moth new "lIR: add integration milestone feature file" -s high --no-edit --stdin << 'EOF'
## Summary
Add a comprehensive integration test that proves lIR is ready for liar to target.
When all scenarios pass, lIR has sufficient capabilities.

## The file
Create `cert/features/integration_milestone.feature` with phases:

1. **Basic Functions** - define, ret, call, params
2. **Control Flow** - blocks, br, conditional br, phi
3. **Recursion/Loops** - recursive calls, phi loops
4. **Memory** - alloca, load, store
5. **External Calls** - declare, FFI
6. **Structs** - defstruct, GEP for field access
7. **Function Pointers** - indirect calls (nice to have)
8. **Closure Simulation** - The ultimate test

## Key tests

### Recursive factorial
```lisp
(define (factorial i64) ((i64 n))
  (block entry
    (br (icmp eq n (i64 0)) base_case recursive_case))
  (block base_case
    (ret (i64 1)))
  (block recursive_case
    (let ((n_minus_1 (sub n (i64 1)))
          (sub_result (call @factorial n_minus_1)))
      (ret (mul n sub_result)))))
```

### Closure simulation (milestone)
```lisp
(defstruct adder_env (i64))

(define (adder_fn i64) ((ptr env) (i64 y))
  (let ((x_ptr (getelementptr %struct.adder_env env (i32 0) (i32 0)))
        (x (load i64 x_ptr)))
    (ret (add x y))))

(define (make_adder ptr) ((i64 x))
  (declare malloc ptr (i64))
  (let ((env (call @malloc (i64 8))))
    (store x (getelementptr %struct.adder_env env (i32 0) (i32 0)))
    (ret env)))
```

## Acceptance Criteria
- [ ] Feature file created
- [ ] All phases documented
- [ ] Final closure simulation test included
- [ ] When all pass, lIR is "A+" ready
EOF

# -----------------------------------------------------------------------------
# LOW: Documentation updates
# -----------------------------------------------------------------------------

moth new "lIR: update doc/lIR.md with full syntax reference" -s low --no-edit --stdin << 'EOF'
## Summary
Update the lIR language documentation to cover all implemented features.

## Sections needed

### Types
- Scalars: i1, i8, i16, i32, i64, float, double, void
- Pointer: ptr
- Vectors: <N x type>
- Structs: %struct.name

### Literals
- Integers: (i32 42)
- Floats: (double 3.14), inf, -inf, nan
- Pointer: (ptr null)
- String: (string "hello\n")
- Vector: (<4 x i32> 1 2 3 4)
- Struct: { (i32 1) (i32 2) }

### Instructions
Document all with examples:
- Arithmetic: add, sub, mul, sdiv, udiv, srem, urem
- Float: fadd, fsub, fmul, fdiv, frem
- Bitwise: and, or, xor, shl, lshr, ashr
- Comparison: icmp, fcmp
- Conversion: trunc, zext, sext, fptrunc, fpext, etc.
- Memory: alloca, load, store, getelementptr
- Control: br, phi, ret
- Aggregate: extractvalue, insertvalue
- Other: select, call

### Top-level forms
- (define ...)
- (declare ...)
- (global ...)
- (defstruct ...)

### Let bindings
Document the syntax once implemented.

## Acceptance Criteria
- [ ] All types documented
- [ ] All instructions documented with examples
- [ ] Top-level forms documented
- [ ] Let bindings documented
EOF

# -----------------------------------------------------------------------------
# LOW: Clean up dead code warnings
# -----------------------------------------------------------------------------

moth new "lIR: clean up clippy warnings and dead code" -s low --no-edit --stdin << 'EOF'
## Summary
Run clippy and fix any warnings. Remove dead code, unused imports, etc.

## Commands
```bash
cargo clippy --all-targets --all-features -- -D warnings
cargo fmt --all -- --check
```

## Common issues to fix
- `#[allow(dead_code)]` annotations that can be removed
- Unused imports
- Redundant clones
- Missing docs on public items

## Acceptance Criteria
- [ ] `cargo clippy` passes with no warnings
- [ ] `cargo fmt` passes
- [ ] No `#[allow(dead_code)]` unless justified
EOF

echo ""
echo "Created moths for A+ status. Run 'moth ls' to see them."
echo ""
echo "Priority order:"
echo "  1. [CRIT] let bindings - unblocks most tests"
echo "  2. [HIGH] ptr return/param types - needed for memory tests"  
echo "  3. [HIGH] GEP struct types - needed for closures"
echo "  4. [HIGH] integration milestone - the target"
echo "  5. [MED]  alloca/load/store ptr support"
echo "  6. [MED]  phi syntax audit"
echo "  7. [LOW]  docs and cleanup"
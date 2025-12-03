#!/bin/bash

# =============================================================================
# Moths for lIR Safety Features (ADR-021)
# =============================================================================

# -----------------------------------------------------------------------------
# Feature 1: Tail Calls
# -----------------------------------------------------------------------------

moth new "lIR: tailcall instruction" -s high --no-edit --stdin << 'EOF'
## Summary
Add guaranteed tail call instruction to lIR.

## Syntax
```lisp
(tailcall @function args...)
```

## AST Addition
```rust
// In ast.rs
pub enum Instruction {
    // ... existing ...
    TailCall {
        func: String,
        args: Vec<Expr>,
    },
}
```

## Parser Addition
```rust
// In parser.rs, parse_instruction()
"tailcall" => {
    let func = self.parse_func_ref()?;
    let args = self.parse_args()?;
    Ok(Instruction::TailCall { func, args })
}
```

## Codegen
```rust
// In codegen.rs
Instruction::TailCall { func, args } => {
    let fn_val = self.get_function(&func)?;
    let arg_vals: Vec<_> = args.iter()
        .map(|a| self.compile_expr(a))
        .collect::<Result<_>>()?;
    
    let call = self.builder.build_call(fn_val, &arg_vals, "tailcall");
    call.set_tail_call(true);
    self.builder.build_return(Some(&call.try_as_basic_value().left().unwrap()));
}
```

## Verifier Checks
1. Must be in tail position (last instruction before implicit ret)
2. Return type must match function return type
3. No owned values pending drop after tailcall

## Test Cases
```gherkin
Scenario: Simple tail call
  Given the expression (define (loop void) () (block entry (tailcall @loop)))
  Then compilation succeeds

Scenario: Tail recursive factorial
  Given the expression (define (fact-tail i64) ((i64 n) (i64 acc)) (block entry (br (icmp eq n (i64 0)) done recurse)) (block done (ret acc)) (block recurse (tailcall @fact-tail (sub n (i64 1)) (mul n acc))))
  When I call fact-tail with (i64 5) (i64 1)
  Then the result is (i64 120)

Scenario: Mutual tail recursion
  Given the expression (define (even? i1) ((i64 n)) (block entry (br (icmp eq n (i64 0)) yes no)) (block yes (ret (i1 1))) (block no (tailcall @odd? (sub n (i64 1)))))
  And the expression (define (odd? i1) ((i64 n)) (block entry (br (icmp eq n (i64 0)) no yes)) (block no (ret (i1 0))) (block yes (tailcall @even? (sub n (i64 1)))))
  # Test via wrapper returning i64
```

## Acceptance Criteria
- [ ] `tailcall` parses
- [ ] Codegen emits LLVM tail call
- [ ] Tail position verified
- [ ] Return type verified
- [ ] Feature file passes
EOF

# -----------------------------------------------------------------------------
# Feature 2: Bounds-Checked Arrays
# -----------------------------------------------------------------------------

moth new "lIR: bounds-checked arrays" -s high --no-edit --stdin << 'EOF'
## Summary
Add fixed-size arrays with bounds-checked access to lIR.

## Types
```lisp
(array T N)         ; Fixed-size array of N elements of type T
```

## Operations
```lisp
(array-alloc T N)           ; Stack allocate array
(array-get arr idx)         ; Bounds-checked read, panics if OOB
(array-set arr idx val)     ; Bounds-checked write, panics if OOB
(array-len arr)             ; Get length (compile-time constant)
(array-ptr arr)             ; Get raw pointer (for FFI)
```

## AST Additions
```rust
pub enum Type {
    // ... existing ...
    Array { elem: Box<Type>, size: usize },
}

pub enum Instruction {
    // ... existing ...
    ArrayAlloc { elem_type: ScalarType, size: usize },
    ArrayGet { array: Box<Expr>, index: Box<Expr> },
    ArraySet { array: Box<Expr>, index: Box<Expr>, value: Box<Expr> },
    ArrayLen { array: Box<Expr> },
    ArrayPtr { array: Box<Expr> },
}
```

## Codegen
```rust
// array-alloc: alloca [N x T]
Instruction::ArrayAlloc { elem_type, size } => {
    let arr_type = elem_type.llvm_type(ctx).array_type(size as u32);
    self.builder.build_alloca(arr_type, "arr")
}

// array-get: bounds check then GEP + load
Instruction::ArrayGet { array, index } => {
    let arr = self.compile_expr(array)?;
    let idx = self.compile_expr(index)?;
    let len = /* get from type */;
    
    // Bounds check
    let in_bounds = self.builder.build_int_compare(
        IntPredicate::ULT, idx, len.into(), "bounds");
    let panic_block = self.append_block("panic");
    let ok_block = self.append_block("ok");
    self.builder.build_conditional_branch(in_bounds, ok_block, panic_block);
    
    // Panic path
    self.builder.position_at_end(panic_block);
    self.builder.build_call(self.get_panic_fn(), &[], "");
    self.builder.build_unreachable();
    
    // OK path
    self.builder.position_at_end(ok_block);
    let ptr = self.builder.build_gep(arr, &[zero, idx], "elem_ptr");
    self.builder.build_load(ptr, "elem")
}
```

## Bounds Check Elimination
When index is a constant and provably in-bounds, skip runtime check:
```rust
if let Expr::Literal(Literal::Int(i)) = index {
    if (i as usize) < size {
        // Skip bounds check, direct GEP
    }
}
```

## Test Cases
```gherkin
Scenario: Array allocation and access
  Given the expression (define (test-array i64) () (block entry (let ((arr (array-alloc i64 10))) (array-set arr (i64 5) (i64 42)) (ret (array-get arr (i64 5))))))
  When I call test-array
  Then the result is (i64 42)

Scenario: Array length
  Given the expression (define (test-len i64) () (block entry (let ((arr (array-alloc i64 10))) (ret (array-len arr)))))
  When I call test-len
  Then the result is (i64 10)

Scenario: Static bounds elimination
  Given the expression (define (static-access i64) () (block entry (let ((arr (array-alloc i64 10))) (array-set arr (i64 0) (i64 99)) (ret (array-get arr (i64 0))))))
  When I call static-access
  Then the result is (i64 99)
  # Verify no bounds check in LLVM IR (manual inspection)
```

## Acceptance Criteria
- [ ] Array type parses
- [ ] array-alloc works
- [ ] array-get with bounds check works
- [ ] array-set with bounds check works
- [ ] array-len returns size
- [ ] Static bounds elimination for constant indices
- [ ] Out-of-bounds access panics
EOF

# -----------------------------------------------------------------------------
# Feature 3: Native Closures
# -----------------------------------------------------------------------------

moth new "lIR: native closures" -s crit --no-edit --stdin << 'EOF'
## Summary
Add native closure support to lIR, replacing the struct+function encoding.

## Syntax
```lisp
; Closure definition
(closure NAME ((CAPTURE-MODE TYPE NAME) ...)
  (fn RETURN-TYPE ((TYPE PARAM) ...)
    BODY))

; Closure type declaration
(closuretype NAME RETURN-TYPE (PARAM-TYPES...))

; Closure call
(closure-call CLOSURE ARGS...)
```

## Capture Modes
```lisp
own    ; Move into closure, closure owns value
ref    ; Borrow, closure cannot escape scope  
rc     ; Reference-counted, shared ownership
```

## AST Additions
```rust
pub enum Item {
    // ... existing ...
    Closure {
        name: String,
        captures: Vec<Capture>,
        params: Vec<Param>,
        return_type: ReturnType,
        body: Vec<Block>,
    },
    ClosureType {
        name: String,
        return_type: ReturnType,
        param_types: Vec<ParamType>,
    },
}

pub struct Capture {
    pub mode: CaptureMode,
    pub ty: ParamType,
    pub name: String,
}

pub enum CaptureMode {
    Own,
    Ref,
    Rc,
}

pub enum Instruction {
    // ... existing ...
    ClosureCall {
        closure: Box<Expr>,
        args: Vec<Expr>,
    },
}
```

## Codegen Strategy
Closures compile to:
1. Environment struct (captures)
2. Function taking (env_ptr, args...)
3. Closure object = { env_ptr, fn_ptr }

```rust
// closure add-n ((own i64 n)) (fn i64 ((i64 x)) ...)
// becomes:

// 1. Env struct
%closure_add_n_env = type { i64 }

// 2. Function
define i64 @closure_add_n_fn(ptr %env, i64 %x) {
    %n_ptr = getelementptr %closure_add_n_env, ptr %env, i32 0, i32 0
    %n = load i64, ptr %n_ptr
    %result = add i64 %n, %x
    ret i64 %result
}

// 3. Closure type
%closuretype_IntToInt = type { ptr, ptr }  ; { env, fn }
```

## Closure Creation
```rust
// At creation site:
let env = self.builder.build_alloca(env_type, "closure_env");
// Store captures into env
for (i, capture) in captures.iter().enumerate() {
    let ptr = self.builder.build_gep(env, &[zero, i.into()], "cap_ptr");
    let val = self.get_local(&capture.name)?;
    self.builder.build_store(val, ptr);
}
// Build closure struct
let closure = self.builder.build_alloca(closure_type, "closure");
let env_field = self.builder.build_gep(closure, &[zero, zero], "env_field");
let fn_field = self.builder.build_gep(closure, &[zero, one], "fn_field");
self.builder.build_store(env, env_field);
self.builder.build_store(fn_ptr, fn_field);
```

## Closure Call
```rust
Instruction::ClosureCall { closure, args } => {
    let closure_val = self.compile_expr(closure)?;
    let env = self.builder.build_extract_value(closure_val, 0, "env");
    let fn_ptr = self.builder.build_extract_value(closure_val, 1, "fn");
    let mut call_args = vec![env];
    call_args.extend(args.iter().map(|a| self.compile_expr(a)).collect::<Result<Vec<_>>>()?);
    self.builder.build_indirect_call(fn_type, fn_ptr, &call_args, "closure_result")
}
```

## Test Cases
```gherkin
Scenario: Simple closure with capture
  Given the expression (closure add-n ((own i64 n)) (fn i64 ((i64 x)) (block entry (ret (add n x)))))
  And the expression (define (test-closure i64) () (block entry (let ((f (make-closure add-n (i64 10)))) (ret (closure-call f (i64 32))))))
  When I call test-closure
  Then the result is (i64 42)

Scenario: Closure with multiple captures
  Given the expression (closure add-mul ((own i64 a) (own i64 b)) (fn i64 ((i64 x)) (block entry (ret (add (mul a x) b)))))
  And the expression (define (test i64) () (block entry (let ((f (make-closure add-mul (i64 2) (i64 3)))) (ret (closure-call f (i64 10))))))
  When I call test
  Then the result is (i64 23)

Scenario: Closure passed to function
  Given the expression (closuretype IntToInt i64 (i64))
  And the expression (define (apply i64) ((own IntToInt f) (i64 x)) (block entry (ret (closure-call f x))))
  # ... setup closure and call apply
```

## Acceptance Criteria
- [ ] Closure syntax parses
- [ ] Closuretype parses
- [ ] Env struct generated correctly
- [ ] Captures stored in env
- [ ] closure-call works
- [ ] own capture moves value
- [ ] ref capture borrows (verification in borrow checker moth)
- [ ] Feature file passes
EOF

# -----------------------------------------------------------------------------
# Feature 4: Reference Counting
# -----------------------------------------------------------------------------

moth new "lIR: reference counting" -s high --no-edit --stdin << 'EOF'
## Summary
Add reference-counted pointers to lIR with automatic inc/dec.

## Type
```lisp
rc T        ; Reference-counted pointer to T
```

## Operations
```lisp
(rc-alloc T)        ; Allocate with refcount 1, returns rc T
(rc-clone x)        ; Increment refcount, return alias
(rc-drop x)         ; Decrement refcount, free if zero
(rc-count x)        ; Get current refcount (for debugging)
(rc-get x)          ; Get value (like load)
(rc-ptr x)          ; Get raw pointer (unsafe)
```

## AST Additions
```rust
pub enum ParamType {
    // ... existing ...
    Rc(Box<ScalarType>),
}

pub enum Instruction {
    // ... existing ...
    RcAlloc { ty: ScalarType },
    RcClone { value: Box<Expr> },
    RcDrop { value: Box<Expr> },
    RcCount { value: Box<Expr> },
    RcGet { value: Box<Expr> },
    RcPtr { value: Box<Expr> },
}
```

## Memory Layout
```
+----------+----------+
| refcount | data ... |
+----------+----------+
  i64        T
```

## Codegen: rc-alloc
```rust
Instruction::RcAlloc { ty } => {
    // Allocate: sizeof(i64) + sizeof(T)
    let size = 8 + ty.size_of();
    let ptr = self.builder.build_call(malloc, &[size.into()], "rc_ptr");
    
    // Initialize refcount to 1
    let rc_ptr = self.builder.build_bitcast(ptr, i64_ptr_type, "rc_field");
    self.builder.build_store(1i64.into(), rc_ptr);
    
    // Return pointer (past refcount header)
    self.builder.build_gep(ptr, &[8i64.into()], "data_ptr")
}
```

## Codegen: rc-clone
```rust
Instruction::RcClone { value } => {
    let ptr = self.compile_expr(value)?;
    // Get refcount field (8 bytes before data)
    let rc_ptr = self.builder.build_gep(ptr, &[(-8i64).into()], "rc_field");
    let rc_ptr = self.builder.build_bitcast(rc_ptr, i64_ptr_type, "rc_i64");
    
    // Atomic increment
    self.builder.build_atomicrmw(
        AtomicRMWBinOp::Add, rc_ptr, 1i64.into(),
        AtomicOrdering::SeqCst, "new_rc");
    
    ptr  // Return same pointer
}
```

## Codegen: rc-drop
```rust
Instruction::RcDrop { value } => {
    let ptr = self.compile_expr(value)?;
    let rc_ptr = self.builder.build_gep(ptr, &[(-8i64).into()], "rc_field");
    let rc_ptr = self.builder.build_bitcast(rc_ptr, i64_ptr_type, "rc_i64");
    
    // Atomic decrement
    let old_rc = self.builder.build_atomicrmw(
        AtomicRMWBinOp::Sub, rc_ptr, 1i64.into(),
        AtomicOrdering::SeqCst, "old_rc");
    
    // If was 1 (now 0), free
    let was_one = self.builder.build_int_compare(
        IntPredicate::EQ, old_rc, 1i64.into(), "was_one");
    
    let free_block = self.append_block("rc_free");
    let cont_block = self.append_block("rc_cont");
    self.builder.build_conditional_branch(was_one, free_block, cont_block);
    
    self.builder.position_at_end(free_block);
    let base_ptr = self.builder.build_gep(ptr, &[(-8i64).into()], "base");
    self.builder.build_call(free, &[base_ptr], "");
    self.builder.build_unconditional_branch(cont_block);
    
    self.builder.position_at_end(cont_block);
}
```

## Implicit RC in Let Bindings
When binding an `rc` value, implicit clone:
```lisp
(let ((x (rc-alloc i64)))
  (rc-set x (i64 42))
  (let ((y x))           ; implicit rc-clone
    (rc-get y))          ; use y
  (rc-get x))            ; x still valid
; implicit rc-drop for x and y at scope end
```

## Test Cases
```gherkin
Scenario: RC allocate and read
  Given the expression (define (test-rc i64) () (block entry (let ((x (rc-alloc i64))) (store (i64 42) (rc-ptr x)) (let ((v (load i64 (rc-ptr x)))) (rc-drop x) (ret v)))))
  When I call test-rc
  Then the result is (i64 42)

Scenario: RC clone maintains value
  Given the expression (define (test-rc-clone i64) () (block entry (let ((x (rc-alloc i64))) (store (i64 42) (rc-ptr x)) (let ((y (rc-clone x))) (rc-drop x) (let ((v (load i64 (rc-ptr y)))) (rc-drop y) (ret v))))))
  When I call test-rc-clone
  Then the result is (i64 42)

Scenario: RC count
  Given the expression (define (test-rc-count i64) () (block entry (let ((x (rc-alloc i64))) (let ((y (rc-clone x))) (let ((c (rc-count x))) (rc-drop y) (rc-drop x) (ret c))))))
  When I call test-rc-count
  Then the result is (i64 2)
```

## Acceptance Criteria
- [ ] `rc T` type parses
- [ ] rc-alloc allocates with refcount 1
- [ ] rc-clone increments atomically
- [ ] rc-drop decrements, frees at zero
- [ ] rc-count returns current count
- [ ] No use-after-free (manual verification)
- [ ] Feature file passes
EOF

# -----------------------------------------------------------------------------
# Feature 5: Ownership and Borrow Checking
# -----------------------------------------------------------------------------

moth new "lIR: ownership types" -s crit --no-edit --stdin << 'EOF'
## Summary
Add ownership pointer types to lIR: own, ref, refmut.

## Types
```lisp
own T       ; Owned pointer — dropped when out of scope
ref T       ; Shared borrow — read-only, lifetime-bound
refmut T    ; Mutable borrow — exclusive, lifetime-bound
```

## AST Additions
```rust
pub enum ParamType {
    Scalar(ScalarType),
    Ptr,
    Own(Box<ScalarType>),
    Ref(Box<ScalarType>),
    RefMut(Box<ScalarType>),
    Rc(Box<ScalarType>),
}
```

## Operations
```lisp
(alloc own T)           ; Allocate owned
(borrow ref x)          ; Create shared borrow
(borrow refmut x)       ; Create mutable borrow
(drop x)                ; Explicit drop
(move x)                ; Explicit move
```

## Codegen
Ownership types compile to raw pointers at LLVM level.
The safety is enforced by the verifier, not runtime.

```rust
// own T -> ptr in LLVM
// ref T -> ptr in LLVM
// refmut T -> ptr in LLVM

// alloc own T -> alloca or malloc depending on escape analysis
// drop x -> call destructor, then free if heap-allocated
// borrow ref x -> just the pointer (no-op at runtime)
// borrow refmut x -> just the pointer (no-op at runtime)
```

## Test Cases (Parser/Codegen Only)
```gherkin
Scenario: Parse own type
  Given the expression (define (take void) ((own i64 x)) (block entry (drop x) (ret)))
  Then parsing succeeds

Scenario: Parse ref type
  Given the expression (define (peek i64) ((ref i64 x)) (block entry (ret (load i64 x))))
  Then parsing succeeds

Scenario: Parse refmut type
  Given the expression (define (poke void) ((refmut i64 x)) (block entry (store (i64 42) x) (ret)))
  Then parsing succeeds

Scenario: Alloc and drop
  Given the expression (define (test i64) () (block entry (let ((x (alloc own i64))) (store (i64 42) x) (let ((v (load i64 x))) (drop x) (ret v)))))
  When I call test
  Then the result is (i64 42)
```

## Acceptance Criteria
- [ ] own, ref, refmut types parse
- [ ] alloc own T works
- [ ] borrow ref/refmut works
- [ ] drop works
- [ ] Compiles to correct LLVM IR
- [ ] Verification is separate moth
EOF

# -----------------------------------------------------------------------------
# Feature 6: Borrow Checker (Verifier)
# -----------------------------------------------------------------------------

moth new "lIR: borrow checker verifier" -s crit --no-edit --stdin << 'EOF'
## Summary
Implement the borrow checker as a verification pass over lIR.
Runs before codegen, rejects invalid programs.

## Verification Pass
```rust
pub struct BorrowChecker {
    bindings: HashMap<String, OwnershipState>,
    borrows: HashMap<String, BorrowInfo>,
    errors: Vec<BorrowError>,
}

pub enum OwnershipState {
    Owned,
    Moved,
    Borrowed { by: Vec<String> },
    BorrowedMut { by: String },
    Dropped,
}

pub struct BorrowInfo {
    kind: BorrowKind,
    source: String,      // What is borrowed
    scope_depth: usize,  // When borrow ends
}
```

## Rules Enforced

### Rule 1: No use after move
```lisp
; REJECT
(let ((x (alloc own i64)))
  (let ((y x))          ; x moved to y
    (load i64 x)))      ; ERROR: x moved
```

### Rule 2: No use after drop
```lisp
; REJECT
(let ((x (alloc own i64)))
  (drop x)
  (load i64 x))         ; ERROR: x dropped
```

### Rule 3: Mutable borrow is exclusive
```lisp
; REJECT
(let ((x (alloc own i64)))
  (let ((a (borrow refmut x))
        (b (borrow ref x)))   ; ERROR: x already mutably borrowed
    ...))
```

### Rule 4: Cannot borrow moved value
```lisp
; REJECT
(let ((x (alloc own i64)))
  (let ((y x))          ; x moved
    (borrow ref x)))    ; ERROR: x moved
```

### Rule 5: Borrow cannot outlive owner
```lisp
; REJECT
(define (bad ref i64) ()
  (block entry
    (let ((x (alloc own i64)))
      (ret (borrow ref x)))))  ; ERROR: x dropped, borrow escapes
```

### Rule 6: All owned values must be dropped
```lisp
; REJECT (memory leak)
(define (leak void) ()
  (block entry
    (let ((x (alloc own i64)))
      (ret))))          ; ERROR: x not dropped
```

## Algorithm
```rust
impl BorrowChecker {
    pub fn check_function(&mut self, func: &Function) -> Result<(), Vec<BorrowError>> {
        // Initialize params
        for param in &func.params {
            match &param.ty {
                ParamType::Own(_) => self.bindings.insert(param.name.clone(), OwnershipState::Owned),
                ParamType::Ref(_) => self.bindings.insert(param.name.clone(), OwnershipState::Borrowed { by: vec![] }),
                // ...
            }
        }
        
        // Walk blocks in control flow order
        for block in &func.blocks {
            self.check_block(block)?;
        }
        
        // At function end, verify all owned dropped or returned
        self.verify_cleanup()?;
        
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }
    
    fn check_instruction(&mut self, inst: &Instruction) -> Result<(), BorrowError> {
        match inst {
            Instruction::Let { bindings, body } => {
                let scope_depth = self.enter_scope();
                for (name, expr) in bindings {
                    self.check_expr(expr)?;
                    // Track ownership based on expr result
                }
                for inst in body {
                    self.check_instruction(inst)?;
                }
                self.exit_scope(scope_depth)?;  // Verify borrows ended, insert drops
            }
            // ... other instructions
        }
    }
    
    fn check_use(&mut self, name: &str) -> Result<(), BorrowError> {
        match self.bindings.get(name) {
            Some(OwnershipState::Moved) => Err(BorrowError::UseAfterMove(name.to_string())),
            Some(OwnershipState::Dropped) => Err(BorrowError::UseAfterDrop(name.to_string())),
            Some(_) => Ok(()),
            None => Err(BorrowError::Undefined(name.to_string())),
        }
    }
}
```

## Error Messages
```
error: use of moved value `x`
  --> test.lir:5:10
   |
 3 |   (let ((y x))
   |            - value moved here
 4 |     ...
 5 |     (load i64 x))
   |              ^ value used here after move

error: cannot borrow `x` as immutable because it is already borrowed as mutable
  --> test.lir:4:20
   |
 3 |   (let ((a (borrow refmut x))
   |                           - mutable borrow occurs here
 4 |         (b (borrow ref x)))
   |                        ^ immutable borrow occurs here
```

## Test Cases
```gherkin
Scenario: Reject use after move
  Given the expression (define (bad void) () (block entry (let ((x (alloc own i64))) (let ((y x)) (load i64 x)) (ret))))
  Then verification fails with "use of moved value"

Scenario: Reject double mutable borrow
  Given the expression (define (bad void) () (block entry (let ((x (alloc own i64))) (let ((a (borrow refmut x)) (b (borrow refmut x))) ...) (ret))))
  Then verification fails with "already borrowed as mutable"

Scenario: Accept valid borrows
  Given the expression (define (ok void) () (block entry (let ((x (alloc own i64))) (let ((a (borrow ref x)) (b (borrow ref x))) (add (load i64 a) (load i64 b))) (drop x) (ret))))
  Then verification succeeds
```

## Acceptance Criteria
- [ ] Use after move detected
- [ ] Use after drop detected
- [ ] Mutable exclusivity enforced
- [ ] Borrow escape detected
- [ ] Missing drop detected (optional: auto-insert)
- [ ] Good error messages with locations
- [ ] All valid programs pass
EOF

echo ""
echo "Created 6 moths for lIR safety features (ADR-021):"
echo ""
echo "  1. [HIGH] Tail calls - guaranteed TCO"
echo "  2. [HIGH] Bounds-checked arrays - safe indexing"
echo "  3. [CRIT] Native closures - captures, types"
echo "  4. [HIGH] Reference counting - rc type, auto inc/dec"
echo "  5. [CRIT] Ownership types - own/ref/refmut"
echo "  6. [CRIT] Borrow checker - verification pass"
echo ""
echo "Implementation order:"
echo "  1 (tailcall) - isolated, easy win"
echo "  2 (arrays) - useful, moderate complexity"
echo "  5 (ownership types) - foundation for borrow checking"
echo "  6 (borrow checker) - the big one"
echo "  4 (rc) - builds on ownership"
echo "  3 (closures) - most complex, needs ownership"
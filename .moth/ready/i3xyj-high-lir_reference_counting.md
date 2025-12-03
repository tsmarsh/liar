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

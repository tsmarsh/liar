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

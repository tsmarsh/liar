## Summary
Add atomicrmw instruction for atomic read-modify-write operations.
Used for lock-free counters, accumulators, and RC refcounts.

## Syntax
```lisp
(atomicrmw op ordering ptr value)
```

## Operations
```lisp
xchg    ; Exchange (swap)
add     ; Add
sub     ; Subtract  
and     ; Bitwise AND
or      ; Bitwise OR
xor     ; Bitwise XOR
min     ; Signed minimum
max     ; Signed maximum
umin    ; Unsigned minimum
umax    ; Unsigned maximum
```

## Examples
```lisp
; Atomic increment (returns OLD value)
(atomicrmw add seq_cst counter (i64 1))

; Atomic swap
(atomicrmw xchg seq_cst ptr new_value)

; Atomic max
(atomicrmw max seq_cst ptr candidate)
```

## AST Addition
```rust
pub enum AtomicRMWOp {
    Xchg, Add, Sub, And, Or, Xor, Min, Max, UMin, UMax,
}

pub enum Expr {
    // ... existing ...
    AtomicRMW {
        op: AtomicRMWOp,
        ordering: MemoryOrdering,
        ptr: Box<Expr>,
        value: Box<Expr>,
    },
}
```

## Codegen
```rust
Expr::AtomicRMW { op, ordering, ptr, value } => {
    let ptr_val = self.compile_expr(ptr)?;
    let val = self.compile_expr(value)?;
    let llvm_op = match op {
        AtomicRMWOp::Xchg => AtomicRMWBinOp::Xchg,
        AtomicRMWOp::Add => AtomicRMWBinOp::Add,
        // ...
    };
    self.builder.build_atomicrmw(llvm_op, ptr_val, val, ordering.to_llvm())
}
```

## Use in RC (current impl can use this)
```lisp
; rc-clone becomes:
(atomicrmw add seq_cst refcount_ptr (i64 1))

; rc-drop becomes:
(let ((old (atomicrmw sub seq_cst refcount_ptr (i64 1))))
  (br (icmp eq old (i64 1)) free_block continue_block))
```

## Test Cases
```gherkin
Scenario: Atomic increment returns old value
  Given the expression (define (test i64) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (ret (atomicrmw add seq_cst p (i64 5))))))
  When I call test
  Then the result is (i64 10)

Scenario: Atomic increment updates memory
  Given the expression (define (test i64) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (atomicrmw add seq_cst p (i64 5)) (ret (load i64 p)))))
  When I call test
  Then the result is (i64 15)
```

## Acceptance Criteria
- [ ] atomicrmw parses with all ops and orderings
- [ ] Returns old value
- [ ] Codegen produces LLVM atomicrmw
- [ ] All 10 operations work

## Summary
Add cmpxchg (compare-and-exchange) instruction for lock-free algorithms.
This is the fundamental primitive for implementing swap! CAS loops.

## Syntax
```lisp
(cmpxchg ordering ptr expected new)
; Returns { old_value, success_flag }
```

## Semantics
```
if *ptr == expected:
    *ptr = new
    return { expected, true }
else:
    return { *ptr, false }
```

## AST Addition
```rust
pub enum Expr {
    // ... existing ...
    CmpXchg {
        ordering: MemoryOrdering,
        ptr: Box<Expr>,
        expected: Box<Expr>,
        new_value: Box<Expr>,
    },
}
```

## Return Type
CmpXchg returns a struct { T, i1 }:
- First element: the value that was in memory
- Second element: whether the exchange succeeded
```lisp
(let ((result (cmpxchg seq_cst ptr expected new)))
  (let ((old_val (extractvalue result 0))
        (success (extractvalue result 1)))
    (br success done retry)))
```

## Use for swap!
```lisp
; (swap! atom fn) compiles to CAS loop:
(block swap_loop
  (let ((old (atomic-load seq_cst i64 ptr)))
    (let ((new (call fn old)))
      (let ((result (cmpxchg seq_cst ptr old new)))
        (br (extractvalue result 1) swap_done swap_loop)))))
(block swap_done
  (ret new))
```

## Codegen
```rust
Expr::CmpXchg { ordering, ptr, expected, new_value } => {
    let ptr_val = self.compile_expr(ptr)?;
    let exp_val = self.compile_expr(expected)?;
    let new_val = self.compile_expr(new_value)?;
    
    self.builder.build_cmpxchg(
        ptr_val, exp_val, new_val,
        ordering.to_llvm(),  // success ordering
        ordering.to_llvm(),  // failure ordering
    )
}
```

## Test Cases
```gherkin
Scenario: CmpXchg success
  Given the expression (define (test i1) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (let ((result (cmpxchg seq_cst p (i64 10) (i64 20)))) (ret (extractvalue result 1))))))
  When I call test
  Then the result is (i1 1)

Scenario: CmpXchg failure
  Given the expression (define (test i1) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (let ((result (cmpxchg seq_cst p (i64 99) (i64 20)))) (ret (extractvalue result 1))))))
  When I call test
  Then the result is (i1 0)

Scenario: CmpXchg returns old value
  Given the expression (define (test i64) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (let ((result (cmpxchg seq_cst p (i64 99) (i64 20)))) (ret (extractvalue result 0))))))
  When I call test
  Then the result is (i64 10)
```

## Acceptance Criteria
- [ ] cmpxchg parses
- [ ] Returns { old_value, success }
- [ ] extractvalue works on result
- [ ] Success case: memory updated
- [ ] Failure case: memory unchanged
- [ ] Can implement CAS loop for swap!

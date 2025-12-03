## Summary
Implement Software Transactional Memory (STM) for coordinated updates
to multiple refs.

## Syntax
```lisp
(ref initial-value)      ; Create transactional ref
(dosync body...)         ; Transaction block
(ref-set ref value)      ; Set in transaction
(alter ref fn args...)   ; Update in transaction
(commute ref fn args...) ; Commutative update
```

## Semantics
- Transactions are atomic, consistent, isolated
- Conflicts cause automatic retry
- No locks, no deadlocks

## Example
```lisp
(let ((account-a (ref 100))
      (account-b (ref 200)))
  (dosync
    (let ((amount 50))
      (alter account-a - amount)
      (alter account-b + amount))))
```

## AST Additions
```rust
pub enum Expr {
    // ... existing ...
    MakeRef(Box<Spanned<Expr>>),
    Dosync(Vec<Spanned<Expr>>),
    RefSet(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Alter {
        ref_expr: Box<Spanned<Expr>>,
        fn_expr: Box<Spanned<Expr>>,
        args: Vec<Spanned<Expr>>,
    },
    Commute {
        ref_expr: Box<Spanned<Expr>>,
        fn_expr: Box<Spanned<Expr>>,
        args: Vec<Spanned<Expr>>,
    },
}
```

## Implementation Notes
- Use MVCC (Multi-Version Concurrency Control)
- Track read/write sets per transaction
- Validate at commit time
- Retry on conflict

## Acceptance Criteria
- [ ] ref creates transactional reference
- [ ] dosync provides transaction scope
- [ ] alter updates in transaction
- [ ] Automatic retry on conflict
- [ ] No deadlocks possible

## Summary
Add atomic load and store instructions with memory ordering to lIR.
These are LLVM primitives needed for implementing atoms.

## Syntax
```lisp
(atomic-load ordering type ptr)       ; Atomic load
(atomic-store ordering value ptr)     ; Atomic store
```

## Memory Orderings
```lisp
monotonic   ; Minimal ordering, no synchronization
acquire     ; Acquire semantics (loads)
release     ; Release semantics (stores)
acq_rel     ; Acquire-release (read-modify-write)
seq_cst     ; Sequential consistency (strongest)
```

## AST Additions
```rust
pub enum MemoryOrdering {
    Monotonic,
    Acquire,
    Release,
    AcqRel,
    SeqCst,
}

pub enum Expr {
    // ... existing ...
    AtomicLoad {
        ordering: MemoryOrdering,
        ty: ScalarType,
        ptr: Box<Expr>,
    },
    AtomicStore {
        ordering: MemoryOrdering,
        value: Box<Expr>,
        ptr: Box<Expr>,
    },
}
```

## Parser
```rust
// (atomic-load seq_cst i64 ptr)
"atomic-load" => {
    let ordering = self.parse_ordering()?;
    let ty = self.parse_scalar_type()?;
    let ptr = self.parse_expr()?;
    Ok(Expr::AtomicLoad { ordering, ty, ptr: Box::new(ptr) })
}

fn parse_ordering(&mut self) -> Result<MemoryOrdering> {
    match self.expect_symbol()?.as_str() {
        "monotonic" => Ok(MemoryOrdering::Monotonic),
        "acquire" => Ok(MemoryOrdering::Acquire),
        "release" => Ok(MemoryOrdering::Release),
        "acq_rel" => Ok(MemoryOrdering::AcqRel),
        "seq_cst" => Ok(MemoryOrdering::SeqCst),
        other => Err(ParseError::InvalidOrdering(other.to_string())),
    }
}
```

## Codegen
```rust
Expr::AtomicLoad { ordering, ty, ptr } => {
    let ptr_val = self.compile_expr(ptr)?;
    let llvm_ordering = match ordering {
        MemoryOrdering::Monotonic => AtomicOrdering::Monotonic,
        MemoryOrdering::Acquire => AtomicOrdering::Acquire,
        MemoryOrdering::SeqCst => AtomicOrdering::SequentiallyConsistent,
        // ...
    };
    let load = self.builder.build_load(ty.to_llvm(), ptr_val, "atomic_load");
    load.set_atomic_ordering(llvm_ordering)?;
    Ok(load)
}
```

## Use in liar Atoms
```lisp
; @atom (deref) compiles to:
(atomic-load seq_cst i64 (rc-ptr atom))

; (reset! atom val) compiles to:
(atomic-store seq_cst val (rc-ptr atom))
```

## Test Cases
```gherkin
Scenario: Atomic load and store
  Given the expression (define (test i64) () (block entry (let ((p (alloca i64))) (atomic-store seq_cst (i64 42) p) (ret (atomic-load seq_cst i64 p)))))
  When I call test
  Then the result is (i64 42)
```

## Acceptance Criteria
- [ ] atomic-load parses with all orderings
- [ ] atomic-store parses with all orderings
- [ ] Codegen produces LLVM atomic instructions
- [ ] Invalid ordering = parse error
- [ ] Feature file passes

## Summary
Implement parallel map and filter operations that execute across
multiple threads. These require Sync closures (from plet).

## Syntax
```lisp
(pmap fn collection)       ; Parallel map
(pfilter pred collection)  ; Parallel filter
(preduce fn init coll)     ; Parallel reduce
```

## Semantics
```lisp
; pmap applies fn in parallel across collection
(pmap (fn (x) (* x x)) [1 2 3 4])  ; => [1 4 9 16]

; Only Sync closures allowed
(plet ((factor 2))
  (pmap (fn (x) (* x factor)) data))  ; OK: plet closure is Sync

(let ((factor 2))
  (pmap (fn (x) (* x factor)) data))  ; ERROR: let closure is NonSync
```

## Type Checking
```rust
fn check_pmap_call(&mut self, fn_expr: &Expr, coll_expr: &Expr) {
    let fn_ty = self.infer(fn_expr)?;
    let color = self.get_closure_color(fn_expr);
    
    if color != ClosureColor::Sync && color != ClosureColor::Pure {
        self.error(TypeError::NonSyncToPmap {
            found: color,
            span: fn_expr.span,
        });
    }
}
```

## Runtime Implementation
Options:
1. **Rayon** - Work-stealing thread pool (Rust crate)
2. **pthreads** - Direct FFI to POSIX threads
3. **Custom** - Simple thread pool

## Codegen to lIR
```lisp
; (pmap f coll) conceptually becomes:
(declare pmap_impl ptr (ptr ptr))  ; runtime function

; Or inline:
(let ((results (array-alloc T (len coll)))
      (threads (array-alloc ptr (len coll))))
  ; spawn thread per element (or chunk)
  ; join all
  ; return results)
```

## Chunking Strategy
For large collections, divide into chunks:
```lisp
; Don't spawn 1M threads for 1M elements
; Chunk into (num-cores) pieces
```

## Acceptance Criteria
- [ ] pmap executes in parallel
- [ ] pfilter executes in parallel
- [ ] Only Sync/Pure closures accepted
- [ ] NonSync closure = compile error
- [ ] Proper thread pool management
- [ ] Results maintain order

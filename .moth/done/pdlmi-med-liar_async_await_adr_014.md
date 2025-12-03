## Summary
Implement async/await for non-blocking I/O operations.
Async blocks must use plet semantics (thread-safe).

## Syntax
```lisp
(async body)             ; Create future
(await future)           ; Block until complete
(.await future)          ; Alternative syntax
```

## Semantics
```lisp
; Async returns a Future<T>
(let ((f (async (fetch-url "https://..."))))
  (await f))  ; Blocks, returns result

; Parallel async
(plet ((a (async (fetch "url1")))
       (b (async (fetch "url2"))))
  (list (await a) (await b)))
```

## AST Additions
```rust
pub enum Expr {
    // ... existing ...
    Async(Box<Spanned<Expr>>),
    Await(Box<Spanned<Expr>>),
}
```

## Type System
```rust
// async creates Future<T> where T is body return type
// await takes Future<T>, returns T
```

## Closure Color Integration
- async body must be Sync (no Local closures)
- Captured state must be Send + Sync

## Codegen
Likely targets tokio or async-std runtime.
May need runtime initialization.

## Acceptance Criteria
- [ ] async creates future
- [ ] await blocks until complete
- [ ] Works with plet for parallel await
- [ ] Closure color enforced (Sync required)
- [ ] Integration with I/O runtime

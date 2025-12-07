## Summary

Add `rc-count` operation to inspect reference count for debugging.

## liar Syntax

```lisp
(let ((x (rc-alloc 42)))
  (let ((y (rc-clone x)))
    (print (rc-count x))))  ; => 2
```

## lIR Output

```lisp
(rc-count %x)  ; returns i64 refcount
```

## Use Cases

- Debugging reference counting issues
- Testing RC implementation
- Detecting leaks (count should be 1 before scope exit)

## Acceptance Criteria

- [ ] Parse rc-count operation
- [ ] Generate lIR RcCount
- [ ] Feature file testing clone/drop behavior

## Notes

Low priority - debugging only, not for production code.
lIR already supports `RcCount`.

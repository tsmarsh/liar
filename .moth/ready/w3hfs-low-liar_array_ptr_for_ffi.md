## Summary

Add `array-ptr` operation to get raw pointer from array for FFI.

## liar Syntax

```lisp
(let ((arr (array i8 256)))
  (ffi-call "write" fd (array-ptr arr) (array-len arr)))
```

## lIR Output

```lisp
(array-ptr %arr)  ; returns ptr to first element
```

## Use Cases

- Passing arrays to C functions
- Buffer operations with FFI
- Memory-mapped I/O

## Acceptance Criteria

- [ ] Parse array-ptr operation
- [ ] Generate lIR ArrayPtr
- [ ] Feature file with FFI test

## Notes

Low priority - specialized FFI use case.
lIR already supports `ArrayPtr`.

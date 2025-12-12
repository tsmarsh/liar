## Summary

Add integer width conversion operations to liar.

## liar Syntax

```lisp
(truncate i8 x)   ; truncate to smaller int
(zero-extend i64 x)  ; zero extend to larger int
(sign-extend i64 x)  ; sign extend to larger int
```

Or shorter:
```lisp
(trunc i8 x)
(zext i64 x)
(sext i64 x)
```

## lIR Output

```lisp
(trunc i8 (i32 255))
(zext i64 (i8 255))
(sext i64 (i8 -1))
```

## Use Cases

- FFI with C functions expecting specific int widths
- Bit manipulation requiring exact sizes
- Memory-efficient data structures

## Acceptance Criteria

- [ ] Parse conversion operations with target type
- [ ] Generate correct lIR conversions
- [ ] Feature file with test scenarios
- [ ] Type checking (source must be integer)

## Notes

lIR already supports these. Frontend work only.

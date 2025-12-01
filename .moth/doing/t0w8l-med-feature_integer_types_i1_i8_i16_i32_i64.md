# Feature: Integer Types (i1, i8, i16, i32, i64)

Write `cert/features/integer_types.feature` covering:

- `i1`, `i8`, `i16`, `i32`, `i64` literals
- Valid ranges for each type
- Negative values

## Implementation Notes

Created `cert/features/integer_types.feature` with scenarios for:

1. **i1 literals** - Boolean equivalent (0 and 1 only)
2. **i8 literals** - 8-bit signed: -128 to 127, overflow wraps (255 -> -1)
3. **i16 literals** - 16-bit signed: -32768 to 32767
4. **i32 literals** - 32-bit signed: -2147483648 to 2147483647
5. **i64 literals** - 64-bit signed: full range

Key design decisions:
- Results shown as signed (two's complement interpretation)
- Overflow values wrap around using modular arithmetic
- No explicit error for out-of-range literals (LLVM behavior)

# Feature: Float Types (float, double)

Write `cert/features/float_types.feature` covering:

- `float` and `double` literals
- Scientific notation
- Special values (inf, nan)

## Implementation Notes

Created `cert/features/float_types.feature` with scenarios for:

1. **float literals** - 32-bit IEEE 754 single precision
2. **double literals** - 64-bit IEEE 754 double precision
3. **Scientific notation** - e.g., 1.0e10, 1.5e-5
4. **Positive and negative zero** - IEEE 754 signed zeros
5. **Infinity** - inf and -inf
6. **NaN** - Not a Number

Key design decisions:
- Use LLVM type names exactly (float, double—not f32/f64)
- Support IEEE 754 special values
- Scientific notation with e/E exponent

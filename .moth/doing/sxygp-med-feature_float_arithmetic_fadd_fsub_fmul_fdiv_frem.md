# Feature: Float Arithmetic (fadd, fsub, fmul, fdiv, frem)

Write `cert/features/float_arithmetic.feature` covering:

- `fadd`, `fsub`, `fmul`, `fdiv`, `frem`
- Both `float` and `double`
- IEEE 754 edge cases

## Implementation Notes

Created `cert/features/float_arithmetic.feature` with scenarios for:

1. **fadd** - Floating point addition
2. **fsub** - Floating point subtraction
3. **fmul** - Floating point multiplication
4. **fdiv** - Floating point division
5. **frem** - Floating point remainder

IEEE 754 edge cases covered:
- Infinity arithmetic (inf + 1 = inf, inf - inf = nan)
- NaN propagation (any op with nan = nan)
- Signed zero handling (-0.0 behavior)
- Division by zero (1/0 = inf, -1/0 = -inf)
- Indeterminate forms (0*inf = nan, inf-inf = nan)

Key design decisions:
- All operations require matching types (float or double)
- Separate from integer ops (fadd not add)
- IEEE 754 compliant behavior

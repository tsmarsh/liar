# Feature: Integer Arithmetic (add, sub, mul, sdiv, udiv, srem, urem)

Write `cert/features/integer_arithmetic.feature` covering:

- `add`, `sub`, `mul`
- `sdiv`, `udiv` (signed/unsigned division)
- `srem`, `urem` (signed/unsigned remainder)
- All integer widths

## Implementation Notes

Created `cert/features/integer_arithmetic.feature` with scenarios for:

1. **add** - Integer addition with overflow wrapping
2. **sub** - Integer subtraction
3. **mul** - Integer multiplication with overflow wrapping
4. **sdiv** - Signed division (rounds toward zero)
5. **udiv** - Unsigned division (treats bits as unsigned)
6. **srem** - Signed remainder (sign follows dividend)
7. **urem** - Unsigned remainder

Key design decisions:
- All operations require matching types
- Overflow uses modular arithmetic (two's complement)
- Division/remainder semantics match LLVM (rounds toward zero)
- sdiv/udiv and srem/urem differ in how they interpret negative values

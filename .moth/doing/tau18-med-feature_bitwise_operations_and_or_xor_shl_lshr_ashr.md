# Feature: Bitwise Operations (and, or, xor, shl, lshr, ashr)

Write `cert/features/bitwise.feature` covering:

- `and`, `or`, `xor`
- `shl`, `lshr`, `ashr`
- Boolean logic with `i1`

## Implementation Notes

Created `cert/features/bitwise.feature` with scenarios for:

1. **and** - Bitwise AND
2. **or** - Bitwise OR
3. **xor** - Bitwise exclusive OR
4. **shl** - Shift left (multiply by 2^n)
5. **lshr** - Logical shift right (zero-fill)
6. **ashr** - Arithmetic shift right (sign-preserving)

Key design decisions:
- Boolean logic uses i1 with and/or/xor (no separate bool ops)
- lshr vs ashr: lshr fills with 0, ashr preserves sign bit
- Shift amounts are same type as value
- Binary literals (0b1100) for clarity in tests

# Feature: Integer Comparison (icmp)

Write `cert/features/icmp.feature` covering:

- All predicates: eq, ne, slt, sle, sgt, sge, ult, ule, ugt, uge
- Signed vs unsigned semantics
- All integer widths

## Implementation Notes

Created `cert/features/icmp.feature` with scenarios for all predicates:

1. **eq** - Equal
2. **ne** - Not equal
3. **slt** - Signed less than
4. **sle** - Signed less than or equal
5. **sgt** - Signed greater than
6. **sge** - Signed greater than or equal
7. **ult** - Unsigned less than
8. **ule** - Unsigned less than or equal
9. **ugt** - Unsigned greater than
10. **uge** - Unsigned greater than or equal

Key design decisions:
- Always returns i1 (not bool)
- Signed predicates (slt, sgt, etc.) interpret bits as two's complement
- Unsigned predicates (ult, ugt, etc.) treat bits as positive values
- -1 as i8 is less than 0 signed, but greater than 0 unsigned (it's 255)

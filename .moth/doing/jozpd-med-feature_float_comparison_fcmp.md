# Feature: Float Comparison (fcmp)

Write `cert/features/fcmp.feature` covering:

- Ordered predicates: oeq, one, olt, ole, ogt, oge, ord
- Unordered predicates: ueq, une, ult, ule, ugt, uge, uno
- NaN handling

## Implementation Notes

Created `cert/features/fcmp.feature` with scenarios for all predicates:

**Ordered predicates** (false if either operand is NaN):
1. **oeq** - Ordered equal
2. **one** - Ordered not equal
3. **olt** - Ordered less than
4. **ole** - Ordered less than or equal
5. **ogt** - Ordered greater than
6. **oge** - Ordered greater than or equal
7. **ord** - Ordered (neither is NaN)

**Unordered predicates** (true if either operand is NaN):
8. **ueq** - Unordered or equal
9. **une** - Unordered or not equal
10. **ult** - Unordered or less than
11. **ule** - Unordered or less than or equal
12. **ugt** - Unordered or greater than
13. **uge** - Unordered or greater than or equal
14. **uno** - Unordered (either is NaN)

Key design decisions:
- Always returns i1
- Ordered vs unordered determines NaN behavior
- Infinity compares as expected (inf > any finite number)
- Works with both float and double

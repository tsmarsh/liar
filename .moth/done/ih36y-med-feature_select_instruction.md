# Feature: Select Instruction

Write `cert/features/select.feature` covering:

- Basic conditional selection
- With icmp/fcmp conditions
- Various value types

## Implementation Notes

Created `cert/features/select.feature` with scenarios for:

1. Basic select with i1 condition (true/false literals)
2. Select with icmp conditions (integer comparisons)
3. Select with fcmp conditions (float comparisons)
4. Select with various integer types (i8, i16, i32, i64)
5. Select with float types (float, double)
6. Nested select expressions
7. Select with computed values (arithmetic results)

Syntax: `(select condition true-value false-value)`

Key design decisions:
- Condition must be i1 (from comparison or literal)
- True and false values must have matching types
- All three arguments evaluated (no short-circuit)
- Acts as ternary: condition ? true_val : false_val
- This is the only conditional construct (no if/then/else)

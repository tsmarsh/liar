# lIR Vector Arithmetic and Comparison

Extend lIR arithmetic and comparison operations to work on vector types.

## Problem

The cert tests fail because vector arithmetic and comparison aren't implemented:

```
fadd (<2 x double> 1.0 2.0) (<2 x double> 0.5 0.5)  # PENDING
icmp eq (<4 x i32> 1 2 3 4) (<4 x i32> 1 0 3 0)     # PENDING
```

Vector types and basic operations (extractelement, insertelement, shufflevector) work,
but arithmetic operations on vectors don't.

## Scope

Extend these operations to support vector types:

### Integer Arithmetic
- `add`, `sub`, `mul`, `sdiv`, `udiv`, `srem`, `urem`

### Float Arithmetic  
- `fadd`, `fsub`, `fmul`, `fdiv`, `frem`

### Comparison
- `icmp` (all predicates: eq, ne, slt, sle, sgt, sge, ult, ule, ugt, uge)
- `fcmp` (all predicates: oeq, one, olt, ole, ogt, oge, ord, uno, ueq, une, ult, ule, ugt, uge)

### Bitwise
- `and`, `or`, `xor`, `shl`, `lshr`, `ashr`

## Files

- `lir-core/src/types.rs` - Type checking for vector operands
- `lir-codegen/src/codegen/expr/` - Code generation for vector operations

## Success Criteria

All 17 currently failing cert tests pass.

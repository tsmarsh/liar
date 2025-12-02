# JIT Phase 7: Select

## Completed

1. **Select Instruction**
   - build_select(cond, true_val, false_val)
   - Works with any scalar type (i1 condition required)

2. **Tests (4 new, 56 total)**
   - test_select_true
   - test_select_false
   - test_select_with_icmp (combining select with comparison)
   - test_select_float

## Build verified
```
cargo test -p lir-codegen - 56/56 passing
```

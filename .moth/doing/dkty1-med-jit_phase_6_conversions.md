# JIT Phase 6: Conversions

## Completed

1. **Integer Conversions**
   - trunc - truncate to smaller integer
   - zext - zero extend to larger integer
   - sext - sign extend to larger integer

2. **Float Conversions**
   - fptrunc - truncate double to float
   - fpext - extend float to double

3. **Float <-> Int Conversions**
   - fptoui - float to unsigned int
   - fptosi - float to signed int
   - uitofp - unsigned int to float
   - sitofp - signed int to float

4. **Tests (9 new, 52 total)**
   - test_trunc, test_zext, test_sext
   - test_fptrunc, test_fpext
   - test_fptoui, test_fptosi, test_uitofp, test_sitofp

## Build verified
```
cargo test -p lir-codegen - 52/52 passing
```

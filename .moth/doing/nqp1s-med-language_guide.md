As this is a new language, we need an accurate language guide so that LLM and humans can complete tasks in this language.

## Implementation Notes

Created `doc/LANGUAGE.md` with comprehensive guide covering:

1. **Philosophy** - lIR as 1:1 LLVM IR mapping
2. **Types** - Integer (i1-i64), Float (float/double), Vector (<N x type>)
3. **Integer Arithmetic** - add, sub, mul, sdiv, udiv, srem, urem
4. **Float Arithmetic** - fadd, fsub, fmul, fdiv, frem
5. **Bitwise Operations** - and, or, xor, shl, lshr, ashr
6. **Integer Comparison (icmp)** - eq, ne, slt, sle, sgt, sge, ult, ule, ugt, uge
7. **Float Comparison (fcmp)** - ordered (oeq, one, ...) and unordered (ueq, une, ...)
8. **Integer Conversions** - trunc, zext, sext
9. **Float Conversions** - fptrunc, fpext
10. **Int/Float Conversions** - fptoui, fptosi, uitofp, sitofp
11. **Select** - conditional value selection
12. **Vector Operations** - extractelement, insertelement, shufflevector
13. **Type Errors** - common mistakes and strict type checking
14. **Design Principles** - no bool, no promotion, explicit ops

The guide includes examples and tables for quick reference.

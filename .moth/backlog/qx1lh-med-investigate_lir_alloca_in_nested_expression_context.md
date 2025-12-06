# Investigate lIR alloca in nested expression context

## Problem

When a struct is created inline as an argument to a function call, liar generates lIR with `alloca` nested inside the call expression:

```lisp
;; liar source
(count (Single 1))

;; Generated lIR (simplified)
(call @__Countable_Single__count
  (let ((_struct_0 (alloca i64 (i32 1))))
    (store i64 (i64 1) _struct_0)
    _struct_0))
```

This fails with error: **"alloca requires function context"**

The lIR codegen expects `alloca` to appear at the top level of a function body, not nested inside call arguments or other expressions.

## Workaround

Use let-bound variables instead of inline struct constructors:

```lisp
;; This works
(let ((s (Single 1))) (count s))

;; This doesn't work
(count (Single 1))
```

## Investigation Areas

1. **Is this a fundamental LLVM limitation?**
   - LLVM alloca must be in the entry block for reliable stack management
   - Nested allocas could cause issues with stack pointer

2. **Should liar hoist allocas?**
   - Collect all struct allocations and hoist to function entry
   - Pass pointers through the expression tree

3. **Should lIR handle this?**
   - lIR could automatically hoist nested allocas to function entry
   - This adds complexity but makes lIR more usable

4. **Alternative struct representation?**
   - Pass-by-value for small structs (avoid alloca entirely)
   - Requires different calling convention

## Related Files

- `lir/src/codegen.rs` - lIR LLVM codegen
- `liar/src/codegen.rs` - liar to lIR translation

## Test Case

```lisp
(defprotocol Countable (count [self]))
(defstruct Single (x: i64))
(extend-protocol Countable Single (count [self] 1))
(defun test () (count (Single 1)))  ; fails
```

## Discovered During

Protocol dispatch implementation (moth c3y63)

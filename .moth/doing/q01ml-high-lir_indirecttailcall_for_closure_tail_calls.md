# lIR IndirectTailCall for Closure Tail Calls

## Problem

Closures that capture and call function pointers cannot use tail call optimization.

Example:
```lisp
(defun partial1 (f a) (fn (x) (f a x)))  ; (f a x) is in tail position
```

The call `(f a x)` is correctly identified as a tail call, but `f` is a captured
function pointer, not a named function. lIR has no way to express this.

Current lIR:
- `TailCall { name, args }` - only for direct calls to named functions
- `IndirectCall { fn_ptr, ret_ty, args }` - for function pointers, but no tail call

Error: "tailcall requires block context" when trying to compile these closures.

## Solution

Add `IndirectTailCall` variant to lIR AST and implement codegen for it.

## Implementation

1. **lir-core/src/ast.rs** - Add AST variant:
   ```rust
   IndirectTailCall {
       fn_ptr: Box<Expr>,
       ret_ty: ParamType,
       args: Vec<Expr>,
   },
   ```

2. **lir-core/src/parser** - Add parser support for `(indirect-tailcall ...)`

3. **lir-codegen/src/codegen/function.rs** - Handle in block context:
   - Compile fn_ptr to get function pointer value
   - Compile args
   - Build indirect call with `set_tail_call(true)`
   - Immediately build return

4. **lir-codegen/src/codegen/expr/mod.rs** - Return error for expr context
   (same as TailCall - tail calls must be at block level)

5. **liar/src/codegen.rs** - Generate IndirectTailCall when:
   - Expression is marked as tail call
   - Target is a function pointer (captured var, not named function)

6. **Tests** - Update stdlib.feature to verify partial1/pipe work

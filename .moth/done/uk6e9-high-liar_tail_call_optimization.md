## Summary

Add guaranteed tail call optimization to liar for recursive functions.

## Problem

Even with proper `br`/`phi` control flow (moth trqhk), deeply recursive functions will overflow the stack:

```lisp
(defun sum-to (n acc)
  (if (= n 0)
      acc
      (sum-to (- n 1) (+ acc n))))  ; tail position
```

## Solution

Detect tail calls and emit lIR `tailcall` instead of `call`:

```lisp
;; lIR output
(tailcall sum-to (sub %n (i64 1)) (add %acc %n))
```

LLVM's `tail call` guarantees no stack growth.

## Detection Rules

A call is in tail position if:
1. It's the last expression before return
2. Its result is returned directly (not used in further computation)
3. No cleanup needed after the call (no destructors, no finally blocks)

## Acceptance Criteria

- [ ] Detect tail position calls
- [ ] Generate `tailcall` lIR
- [ ] Feature file testing deep recursion (10000+ iterations)
- [ ] Works with mutual recursion

## Blocked By

- trqhk (br/phi control flow) - need proper branches first

## Notes

High priority - enables idiomatic functional recursion patterns.

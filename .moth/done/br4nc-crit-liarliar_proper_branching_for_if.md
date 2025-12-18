# Implement Proper Branching for `if` in liarliar

## Summary

Replace `select`-based `if` codegen with real control flow (`br`/`phi`) to enable recursion in the self-hosted compiler. This is the critical blocker for bootstrap.

## Problem

The current `codegen-if` in `liarliar/codegen.liar` emits LLVM's `select` instruction:

```lisp
(defun codegen-if (ctx: ptr form: ptr) -> ptr
  ;; ... evaluate cond, then, else ...
  (pcons ctx (lir-select syms cond-v then-v else-v)))
```

LLVM's `select` is a conditional *value* selection, not control flow. It evaluates both operands eagerly. This breaks recursion:

```lisp
(defun length (lst)
  (if (nil? lst)
      0
      (+ 1 (length (cdr lst)))))  ;; <- evaluated even when lst is nil!
```

The recursive call happens unconditionally, causing infinite recursion / stack overflow.

## Solution

Emit actual control flow using lIR's existing `br`, `block`, and `phi` instructions:

```lisp
;; (if cond then else) should compile to:

(br cond-result %then %else)

(block %then
  <compile then-expr>
  (br %merge))

(block %else
  <compile else-expr>
  (br %merge))

(block %merge
  (phi result-type [then-result %then] [else-result %else]))
```

## Implementation Plan

### 1. Extend CodegenCtx for block management

Add fields to track:
- Current block name
- Block counter for generating unique labels
- List of emitted blocks (since we can't emit linearly anymore)

```lisp
(defstruct CodegenCtx
  (cg-temp: i64
   cg-syms: ptr
   cg-env: ptr
   cg-fns: ptr
   cg-block-counter: i64      ;; NEW: for unique block names
   cg-current-block: ptr      ;; NEW: current block we're emitting to
   cg-blocks: ptr))           ;; NEW: accumulated blocks for current function
```

### 2. Add block/label generation helpers

```lisp
;; Generate unique block label like %if.then.0, %if.else.0, %if.merge.0
(defun cg-fresh-block (ctx: ptr prefix: ptr) -> ptr
  ...)

;; Emit instruction to current block
(defun cg-emit (ctx: ptr instr: ptr) -> ptr
  ...)

;; Start a new block (sets current, adds to block list)
(defun cg-start-block (ctx: ptr name: ptr) -> ptr
  ...)

;; Terminate current block with br
(defun cg-emit-br (ctx: ptr target: ptr) -> ptr
  ...)

;; Terminate current block with conditional br
(defun cg-emit-br-cond (ctx: ptr cond: ptr then-block: ptr else-block: ptr) -> ptr
  ...)
```

### 3. Rewrite codegen-if

```lisp
(defun codegen-if (ctx: ptr form: ptr) -> ptr
  (let ((cond-form (cadr form))
        (then-form (caddr form))
        (else-form (cadddr form))
        (syms (cg-syms ctx))
        
        ;; Generate unique block names
        (then-label (cg-fresh-block ctx "if.then"))
        (else-label (cg-fresh-block ctx "if.else"))
        (merge-label (cg-fresh-block ctx "if.merge"))
        
        ;; Compile condition in current block
        (cond-result (codegen-expr ctx cond-form))
        (ctx (pcons-head cond-result))
        (cond-v (pcons-tail cond-result))
        
        ;; Emit conditional branch
        (ctx (cg-emit-br-cond ctx cond-v then-label else-label))
        
        ;; Compile then branch
        (ctx (cg-start-block ctx then-label))
        (then-result (codegen-expr ctx then-form))
        (ctx (pcons-head then-result))
        (then-v (pcons-tail then-result))
        (then-exit-block (cg-current-block ctx))  ;; may differ if then had nested ifs
        (ctx (cg-emit-br ctx merge-label))
        
        ;; Compile else branch
        (ctx (cg-start-block ctx else-label))
        (else-result (codegen-expr ctx else-form))
        (ctx (pcons-head else-result))
        (else-v (pcons-tail else-result))
        (else-exit-block (cg-current-block ctx))
        (ctx (cg-emit-br ctx merge-label))
        
        ;; Merge block with phi
        (ctx (cg-start-block ctx merge-label))
        (phi-result (lir-phi syms (sym-i64 syms)
                            then-v then-exit-block
                            else-v else-exit-block)))
    
    (pcons ctx phi-result)))
```

### 4. Update codegen-defun to collect blocks

Instead of building a single `(block entry (ret ...))`, accumulate all blocks and emit them in order:

```lisp
(defun codegen-defun (ctx: ptr form: ptr) -> ptr
  ;; ... setup ...
  
  ;; Initialize with entry block
  (fn-ctx (cg-start-block fn-ctx "entry"))
  
  ;; Compile body (may create additional blocks)
  (body-result (codegen-expr fn-ctx body))
  
  ;; Emit return in final block
  (fn-ctx (cg-emit fn-ctx (lir-ret syms body-v)))
  
  ;; Build function with all accumulated blocks
  (all-blocks (cg-blocks fn-ctx))
  (fn-def (scons (sym-define syms)
                 (scons fn-sig
                        (scons lir-params
                               all-blocks))))  ;; <- all blocks, not just entry
  ...)
```

### 5. Add lIR construction helpers

```lisp
;; Build: (br label)
(defun lir-br (syms: ptr label: ptr) -> ptr
  (scons (sym-br syms) (scons label nil)))

;; Build: (br cond then-label else-label)
(defun lir-br-cond (syms: ptr cond: ptr then-l: ptr else-l: ptr) -> ptr
  (scons (sym-br syms) (scons cond (scons then-l (scons else-l nil)))))

;; Build: (phi type [val1 label1] [val2 label2])
(defun lir-phi (syms: ptr typ: ptr v1: ptr l1: ptr v2: ptr l2: ptr) -> ptr
  (scons (sym-phi syms)
         (scons typ
                (scons (scons v1 (scons l1 nil))
                       (scons (scons v2 (scons l2 nil)) nil)))))
```

### 6. Add new symbols

```lisp
(defun sym-br (s: ptr) -> ptr     (make-sym s 98 114 0 0 0 0 0 0 2))      ;; br
(defun sym-phi (s: ptr) -> ptr    (make-sym s 112 104 105 0 0 0 0 0 3))   ;; phi
```

## Testing

Add test cases to `liarliar/tests/run-tests.sh`:

```bash
echo "Recursive functions:"
run_test "recursive-countdown" '
(defun countdown (n: i64) -> i64
  (if (<= n 0)
      0
      (countdown (- n 1))))
(defun main () -> i64 (countdown 10))' 0

run_test "recursive-sum" '
(defun sum-to (n: i64) -> i64
  (if (<= n 0)
      0
      (+ n (sum-to (- n 1)))))
(defun main () -> i64 (sum-to 10))' 55

run_test "recursive-fib" '
(defun fib (n: i64) -> i64
  (if (<= n 1)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
(defun main () -> i64 (fib 10))' 55

run_test "mutual-recursion" '
(defun is-even (n: i64) -> i64
  (if (= n 0) 1 (is-odd (- n 1))))
(defun is-odd (n: i64) -> i64
  (if (= n 0) 0 (is-even (- n 1))))
(defun main () -> i64 (is-even 10))' 1
```

## Acceptance Criteria

- [ ] `CodegenCtx` extended with block management
- [ ] `cg-fresh-block`, `cg-start-block`, `cg-emit-br`, `cg-emit-br-cond` implemented
- [ ] `lir-br`, `lir-br-cond`, `lir-phi` helpers added
- [ ] `codegen-if` rewritten to emit br/phi
- [ ] `codegen-defun` collects and emits all blocks
- [ ] All existing tests still pass
- [ ] New recursive function tests pass
- [ ] Can compile a simple recursive function and run it without stack overflow

## Impact

This unblocks:
1. **Recursive descent parsing** — liarliar can parse its own source
2. **Recursive AST traversal** — codegen can walk arbitrary depth trees
3. **Self-hosting** — the bootstrap compiler can compile itself

## Estimated Effort

~100-150 lines of changes to `codegen.liar`, plus ~20 lines of new test cases.

## Notes

The key insight is that codegen needs to shift from "emit instructions linearly" to "accumulate blocks, emit at function end." This is a common pattern in real compilers — SSA construction naturally produces a CFG, not a linear instruction stream.

The existing lIR infrastructure already supports all the instructions we need (`br`, `phi`, `block`). The 202 passing lIR test scenarios prove this works. We're just wiring it through liarliar's codegen layer.

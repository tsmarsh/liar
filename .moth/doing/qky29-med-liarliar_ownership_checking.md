# liarliar: Ownership Checking

Create `lib/liarliar/ownership.liar` - borrow checking for memory safety.

**Priority:** MEDIUM (can bootstrap without full borrow checking initially)

## Related ADRs

- [ADR 004: Lexical Scope Ownership](../../doc/adr/004-lexical-ownership.md) — Ownership follows lexical scope
- [ADR 005: Closures Own Captured State](../../doc/adr/005-closure-captures-ownership.md) — Closure capture ownership
- [ADR 003: Mutable Reference with & Sigil](../../doc/adr/003-mutable-reference-sigil.md) — Borrow syntax
- [ADR 007: Aliasing Allowed](../../doc/adr/007-aliasing-allowed.md) — When aliasing is permitted

## Overview

Track ownership state per binding. Prevent use-after-move and aliasing violations. This is what makes liar memory-safe without garbage collection.

## Ownership States

```clojure
;; States:
;; :owned - Value is owned by this binding
;; :moved - Value was moved away (with span info)
;; :borrowed - Immutably borrowed (can have multiple)
;; :borrowed-mut - Mutably borrowed (exclusive)
```

## Checker Context

```clojure
(defstruct OwnerCtx
  (ownership: ptr   ;; binding-id -> state
   borrows: ptr     ;; borrow-id -> borrow-info
   scopes: ptr))    ;; scope stack
```

## Main Checker

```clojure
(defun check-ownership (ctx form)
  (cond
    ((symbol? form) (check-use ctx form))
    ((not (cons? form)) form)
    ((let? form) (check-let ctx form))
    ((ref? form) (check-borrow ctx form))
    ((ref-mut? form) (check-borrow-mut ctx form))
    ((call? form) (check-call ctx form))
    (true (map-form (fn (x) (check-ownership ctx x)) form))))
```

## Use Checking

```clojure
(defun check-use (ctx sym)
  (let ((state (get-ownership ctx sym)))
    (cond
      ((= state :moved) (error "use after move" sym))
      ((= state :borrowed-mut) (error "used while mutably borrowed" sym))
      (true sym))))
```

## Move Tracking

```clojure
(defun check-call (ctx form)
  ;; Arguments are moved unless borrowed
  (let ((args (cdr form)))
    (for-each args
      (fn (arg)
        (if (symbol? arg)
            (mark-moved ctx arg)
            nil)))))
```

## Functions to Implement

- `check-ownership` - Main dispatcher
- `check-use` - Verify binding is usable
- `check-borrow` - Create immutable borrow
- `check-borrow-mut` - Create mutable borrow (exclusive)
- `check-call` - Track moves through calls
- `check-let` - Scope entry/exit
- `mark-moved` - Update ownership state
- `end-borrows` - Clean up at scope exit

## Invariants

1. No use-after-move
2. Mutable borrows are exclusive
3. Borrowed values cannot be moved
4. Borrows don't outlive referents

## Dependencies

- `lib/liarliar/value.liar` - Tagged value predicates
- `liar.hashmap` - Ownership state map

## Test Cases

- Use after move -> error
- Multiple immutable borrows -> OK
- Mutable + immutable borrow -> error
- Borrow escapes scope -> error
- Closure capture moves value -> original binding invalid (ADR 005)
- Aliased immutable values -> OK (ADR 007)

## Ordering

Depends on: `value.liar`, `resolve.liar`, `infer.liar`, `closures.liar`
Required by: `codegen.liar` (optional for initial bootstrap)

## Design Notes

For bootstrap, ownership checking can be simplified or even disabled. The goal is to compile the compiler — we can trust ourselves to write correct code initially. Full ownership checking can be added incrementally.

Key insight from ADR 007: aliasing of immutable values is always safe. This means most liar code (which is immutable by default per ADR 001) won't trigger ownership errors.

The tricky cases are:
1. Mutable captures in closures (ADR 005)
2. Mutable borrows (`&x`)
3. Values passed to functions (moved by default)

Consider implementing in phases:
1. Move tracking only (catch use-after-move)
2. Borrow tracking (catch lifetime errors)
3. Mutable borrow exclusivity (catch aliasing errors)

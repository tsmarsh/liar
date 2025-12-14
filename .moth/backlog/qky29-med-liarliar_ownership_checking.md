# liarliar: Ownership Checking

Create `lib/liarliar/ownership.liar` - borrow checking for memory safety.

## Overview

Track ownership state per binding. Prevent use-after-move and aliasing violations.

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

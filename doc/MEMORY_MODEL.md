# liar Memory Model Decisions

Summary of design decisions from our conversation.

## Core Principle

**Immutability by default.** Values cannot be modified unless explicitly marked. This enables safe sharing without complex ownership tracking.

## Passing Semantics

| What | How |
|------|-----|
| Compound data (cons, vectors, maps) | Pass by reference |
| Primitives (numbers, bools) | Copy |
| Default | Immutable reference |
| `&` parameter | Mutable reference |

```lisp
(defun foo (x)      ; x is immutable reference
  (add x 1))        ; OK, reading

(defun foo (x)      ; x is immutable reference  
  (append x 1))     ; ERROR: cannot mutate

(defun foo (&x)     ; x is mutable reference
  (append x 1))     ; OK, caller expects mutation
```

## Ownership

**Ownership is lexical scope.** A value lives until its binding's scope ends.

```lisp
(let ((x (cons 1 2)))    ; x born, owns cons cell
  (foo x)                 ; x seen by foo
  (bar x)                 ; x seen by bar
  x)                      ; x dies here, cons cell freed
```

Functions don't take ownership. They see values. Return values are owned by whoever binds them.

```lisp
(let ((x 5))
  (let ((z (foo x)))     ; foo returns new value, z owns it
    (add x z)))          ; x still valid
                         ; z freed here
                         ; x freed here
```

## Closures and Captured State

Closures can capture and own mutable state from their enclosing scope:

```lisp
(defun accumulate ()   
  (let ((cache []))           ; cache created here
    (fn (x) 
      (append cache x)        ; mutate cache
      (sum cache))))          ; return sum

; cache moves INTO the closure, closure owns it now

(let ((acc (accumulate)))     ; acc owns the closure, closure owns cache
  (let ((a (acc 4)))          ; cache is [4], a = 4
    (let ((b (acc 5)))        ; cache is [4,5], b = 9
      (let ((c (acc 1)))      ; cache is [4,5,1], c = 10
        (print a)             ; 4
        (print b)             ; 9
        (print c)))))         ; 10
                              ; acc dies, closure dies, cache dies
```

**Ownership rule for closures:**

| Situation | Ownership |
|-----------|-----------|
| Value in `let` | Owned by that scope |
| Returned from function | Owned by binding that receives it |
| Captured by returned closure | Moves into closure, closure owns it |

## Binding Rules

**No redefinition in same scope:**

```lisp
(let ((x 5)
      (x 6))             ; ERROR: x already bound
  ...)

(let ((x 5))
  (let ((x 6))           ; ERROR: x exists in enclosing scope
    ...))
```

**Shadowing OK across function boundary:**

```lisp
(defun foo (y)
  (let ((x (inc y)))     ; this x is local to foo
    x))

(let ((x 5))
  (foo x))               ; foo's internal x doesn't conflict
```

## Aliasing

**Allowed.** Single-threaded, evaluation order is list order.

```lisp
(defun bar (&a &b)
  (append a 1)
  (append b 2))

(let ((x [3, 4]))
  (bar x x))             ; OK: x becomes [3, 4, 1, 2]
```

No Rust-style "exclusive mutable reference" rule needed because:
- Single-threaded (no data races)
- Order is explicit (list evaluation left-to-right)

## Mutation Example

```lisp
(defun foo (&bar)        ; bar is mutable reference
  (let ((v 20)) 
    (append bar v)       ; mutates caller's data
    v))                  ; returns new value

(let ((x [3, 4]))
  (let ((y (foo x)))     
    (print x)            ; => [3, 4, 20] (mutated)
    (print y)))          ; => 20 (fresh value)
```

## Evaluation Order

Defined. List order is evaluation order.

```lisp
(let ((x [3, 4]))
  (let ((y (foo x))      ; first: foo mutates x
        (z (foo x)))     ; second: foo mutates x again
    ...))                ; x is [3, 4, 20, 20]
```

---

# Threading Model

## Two Binding Forms: `let` and `plet`

| Binding | Mutable captures | Thread safety | Use with |
|---------|------------------|---------------|----------|
| `let` | regular values | NOT thread-safe | `map`, `filter`, single-threaded ops |
| `plet` | atoms only | thread-safe | `map`, `pmap`, `pfilter`, any |

## `let` — Single-threaded Closures

```lisp
(defun accumulate ()
  (let ((cache []))           ; regular mutable binding
    (fn (x) 
      (append cache x)
      (sum cache))))

(let ((acc (accumulate))
      (foo [3 4 5]))
  (print (map acc foo)))      ; OK: sequential
; => [3, 7, 12]

(let ((acc (accumulate))
      (foo [3 4 5]))
  (print (pmap acc foo)))     ; ERROR: let-closure passed to pmap
```

## `plet` — Thread-safe Closures

```lisp
(defun accumulate ()
  (plet ((cache (atom []))    ; atom required for mutable state
         (multiplier 10))     ; constants OK without atom
    (fn (x) 
      (swap! cache (fn (c) (append c (* x multiplier))))
      (sum @cache))))

(let ((acc (accumulate))
      (foo [3 4 5]))
  (print (pmap acc foo)))     ; OK: plet-closure is thread-safe
; => [3, 7, 12] or [4, 9, 12] or [5, 8, 12] — non-deterministic order
```

## `plet` Rules

| Binding in `plet` | Read | Mutate |
|-------------------|------|--------|
| `(atom ...)` | OK via `@` | OK via `swap!` |
| regular value | OK | ERROR |

```lisp
; OK - read constant, mutate atom
(plet ((cache (atom []))
       (multipliers [2,3,4]))           
  (fn (x) 
    (swap! cache (fn (c) (append c (apply * x multipliers))))
    (sum @cache)))

; ERROR - can't mutate non-atom in plet
(plet ((cache (atom []))
       (multipliers [2,3,4]))
  (fn (x) 
    (swap! cache (fn (c) (append c (apply * x multipliers))))
    (append multipliers x)    ; ERROR: multipliers is not an atom
    (sum @cache)))
```

**Simple rule: in `plet`, if it's not an atom, it's constant.**

## Closure Color

Closures have a "color" tracked by the compiler:

| Closure type | Created by | Can pass to |
|--------------|------------|-------------|
| `let`-closure | `let` with mutable captures | `map`, `filter`, single-threaded ops |
| `plet`-closure | `plet` (atoms + constants) | `map`, `filter`, `pmap`, `pfilter`, any |
| pure | no mutable captures at all | anywhere |

The color flows through function calls:

```lisp
(defun make-unsafe ()
  (let ((n 0))
    (fn () (set! n (inc n)) n)))  ; returns let-closure

(defun make-safe ()
  (plet ((n (atom 0)))
    (fn () (swap! n inc) @n)))    ; returns plet-closure

(pmap (make-unsafe) [1 2 3])      ; ERROR: let-closure passed to pmap
(pmap (make-safe) [1 2 3])        ; OK
```

## `pmap` Semantics

| Guarantee | Behavior |
|-----------|----------|
| Result order | Preserved (position in output matches position in input) |
| Values | Non-deterministic if closure has shared mutable state |
| Completion | Joins all threads before returning |

```lisp
(plet ((cache (atom [])))
  (pmap (fn (x) (swap! cache (fn (c) (append c x))) (sum @cache))
        [3 4 5]))
; => [3, 7, 12] or [4, 9, 12] or [5, 8, 12] — depends on execution order
```

User opts into non-determinism by using `pmap` with stateful closure.

## Transactions with `dosync`

Multiple atoms need coordinated updates? Use `dosync`:

```lisp
(plet ((balance-a (atom 100))
       (balance-b (atom 100)))
  (fn (amount)
    (dosync
      (swap! balance-a (fn (a) (- a amount)))
      (swap! balance-b (fn (b) (+ b amount))))))
```

Without `dosync`, another thread could read between the two swaps and see inconsistent state. This is a programming choice — if you don't need consistency, skip `dosync` for performance.

## TOCTOU — Check Inside `swap!`

Time-of-check-time-of-use races must be handled inside `swap!`:

```lisp
; WRONG - race condition
(plet ((counter (atom 0)))
  (fn ()
    (if (< @counter 10)           ; check
        (swap! counter inc))))    ; use — counter might be 15 by now

; RIGHT - check inside swap
(plet ((counter (atom 0)))
  (fn ()
    (swap! counter (fn (c) (if (< c 10) (inc c) c)))))
```

---

# Summary Table

| Aspect | Decision |
|--------|----------|
| Default mutability | Immutable |
| Mutable reference | `&` sigil in param |
| Ownership | Lexical scope |
| Passing | By reference (primitives copy) |
| Aliasing | Allowed (single-threaded) |
| Evaluation order | List order (left-to-right) |
| Redefinition in scope | Error |
| Shadowing across fn | OK |
| Closure captures | Moved into closure, closure owns |
| Single-threaded mutable | `let` binding |
| Thread-safe mutable | `plet` binding + `atom` |
| Parallel ops | `pmap`, `pfilter`, etc. require `plet`-closure or pure |
| Coordinated updates | `dosync` for transactions |

---

---

# Unsafe and FFI

## `unsafe` Blocks

`unsafe` enables operations the compiler can't verify, but does NOT disable the type system or ownership rules.

**What `unsafe` enables:**
- FFI calls to C functions
- Raw pointer operations
- Syscalls
- Accessing raw memory

**What `unsafe` does NOT disable:**
- Ownership tracking
- Borrow checking (for `let` closures)
- Closure color tracking
- Type checking

```lisp
(unsafe
  (let ((x [1 2 3]))
    (let ((y x))              ; x moved to y
      (call @c_func x))))     ; ERROR: x was moved — still enforced
```

## Safe Wrappers

`unsafe` should be encapsulated in safe APIs:

```lisp
; STDLIB — safe API, unsafe internals
(defun print (x)
  (unsafe
    (let ((buf (to-bytes x)))
      (syscall SYS_WRITE STDOUT buf (len buf)))))

; USER CODE — no unsafe needed
(print "hello")
```

## Trust Boundary

| Layer | Responsibility |
|-------|----------------|
| `unsafe` block | Programmer guarantees no undefined behavior |
| Safe wrapper | Encapsulates unsafe, exposes safe API |
| User code | Trusts safe APIs, can't cause UB without writing `unsafe` |

## FFI Example

```lisp
; Wrapping a C library safely
(defun c-strlen (s)
  (unsafe
    (let ((cstr (to-cstring s)))      ; convert to null-terminated
      (call @strlen cstr))))           ; call C function

; User sees safe interface
(c-strlen "hello")   ; => 5
```

## Raw Pointers (if needed)

```lisp
(unsafe
  (let ((ptr (alloc 1024)))           ; raw allocation
    (write-byte ptr 0 65)             ; write 'A' at offset 0
    (let ((result (read-byte ptr 0))) ; read it back
      (free ptr)                       ; manual free
      result)))                        ; => 65
```

This is "you're on your own" territory. Compiler can't help with:
- Use after free
- Double free
- Buffer overflows
- Dangling pointers

But it still enforces liar's ownership rules for liar values.

---

# BDD Feature Scenarios

## Ownership and Scope

```gherkin
Feature: Lexical ownership

  Scenario: Value lives until scope ends
    When I evaluate:
      """
      (let ((x [1 2 3]))
        (foo x)
        (bar x)
        (len x))
      """
    Then the result should be "3"
    And x should be freed after the let block

  Scenario: Returned closure owns captured state
    When I evaluate:
      """
      (defun counter ()
        (let ((n 0))
          (fn () (set! n (inc n)) n)))
      (let ((c (counter)))
        [(c) (c) (c)])
      """
    Then the result should be "[1, 2, 3]"

  Scenario: Captured value dies with closure
    When I evaluate:
      """
      (let ((c (counter)))
        (c))
      ; c is out of scope, closure freed, captured n freed
      """
    Then the closure's captured state should be freed
```

## Mutable References

```gherkin
Feature: Mutable references with &

  Scenario: Function mutates caller's data
    When I evaluate:
      """
      (defun push-20 (&v)
        (append v 20))
      (let ((x [1 2 3]))
        (push-20 x)
        x)
      """
    Then the result should be "[1, 2, 3, 20]"

  Scenario: Cannot mutate without & sigil
    When I evaluate:
      """
      (defun bad-push (v)
        (append v 20))
      """
    Then the compiler should error "cannot mutate immutable reference"

  Scenario: Aliasing is allowed
    When I evaluate:
      """
      (defun double-append (&a &b)
        (append a 1)
        (append b 2))
      (let ((x [0]))
        (double-append x x)
        x)
      """
    Then the result should be "[0, 1, 2]"
```

## Threading with let vs plet

```gherkin
Feature: Thread safety with let and plet

  Scenario: let-closure works with map
    When I evaluate:
      """
      (defun accumulate ()
        (let ((cache []))
          (fn (x) (append cache x) (sum cache))))
      (let ((acc (accumulate)))
        (map acc [3 4 5]))
      """
    Then the result should be "[3, 7, 12]"

  Scenario: let-closure rejected by pmap
    When I evaluate:
      """
      (defun accumulate ()
        (let ((cache []))
          (fn (x) (append cache x) (sum cache))))
      (let ((acc (accumulate)))
        (pmap acc [3 4 5]))
      """
    Then the compiler should error "let-closure passed to parallel operation"

  Scenario: plet-closure works with pmap
    When I evaluate:
      """
      (defun accumulate ()
        (plet ((cache (atom [])))
          (fn (x) 
            (swap! cache (fn (c) (append c x)))
            (sum @cache))))
      (let ((acc (accumulate)))
        (pmap acc [3 4 5]))
      """
    Then the result should be one of "[3, 7, 12]" or "[4, 9, 12]" or "[5, 8, 12]"

  Scenario: Cannot mutate non-atom in plet
    When I evaluate:
      """
      (plet ((cache []))
        (fn (x) (append cache x)))
      """
    Then the compiler should error "cannot mutate non-atom in plet"

  Scenario: Constants allowed in plet without atom
    When I evaluate:
      """
      (plet ((factor 10)
             (cache (atom [])))
        (fn (x)
          (swap! cache (fn (c) (append c (* x factor))))
          (sum @cache)))
      """
    Then the compiler should succeed
```

## Closure Color Tracking

```gherkin
Feature: Closure color flows through functions

  Scenario: Function returning let-closure
    When I evaluate:
      """
      (defun make-counter ()
        (let ((n 0))
          (fn () (set! n (inc n)) n)))
      (pmap (make-counter) [1 2 3])
      """
    Then the compiler should error "let-closure passed to parallel operation"

  Scenario: Function returning plet-closure
    When I evaluate:
      """
      (defun make-counter ()
        (plet ((n (atom 0)))
          (fn () (swap! n inc) @n)))
      (pmap (make-counter) [1 2 3])
      """
    Then the result should be one of "[1, 2, 3]" or "[1, 3, 2]" etc

  Scenario: Pure closure works anywhere
    When I evaluate:
      """
      (defun make-doubler ()
        (fn (x) (* x 2)))
      (pmap (make-doubler) [1 2 3])
      """
    Then the result should be "[2, 4, 6]"
```

## Transactions

```gherkin
Feature: Coordinated updates with dosync

  Scenario: Transfer without dosync may show inconsistent state
    Given concurrent readers
    When I evaluate:
      """
      (plet ((a (atom 100))
             (b (atom 100)))
        (fn (amount)
          (swap! a (fn (x) (- x amount)))
          ; reader could see a decremented but b not yet incremented
          (swap! b (fn (x) (+ x amount)))))
      """
    Then intermediate reads may show sum != 200

  Scenario: Transfer with dosync is atomic
    Given concurrent readers
    When I evaluate:
      """
      (plet ((a (atom 100))
             (b (atom 100)))
        (fn (amount)
          (dosync
            (swap! a (fn (x) (- x amount)))
            (swap! b (fn (x) (+ x amount))))))
      """
    Then all reads should show sum == 200

  Scenario: TOCTOU handled inside swap
    When I evaluate:
      """
      (plet ((counter (atom 0)))
        (let ((inc-if-under-10 
               (fn () (swap! counter (fn (c) (if (< c 10) (inc c) c))))))
          (pmap (fn (_) (inc-if-under-10)) (range 100))
          @counter))
      """
    Then the result should be "10"
```

## Binding Rules

```gherkin
Feature: No redefinition in scope

  Scenario: Same let block
    When I evaluate:
      """
      (let ((x 5)
            (x 6))
        x)
      """
    Then the compiler should error "x already bound in this scope"

  Scenario: Nested let
    When I evaluate:
      """
      (let ((x 5))
        (let ((x 6))
          x))
      """
    Then the compiler should error "x already bound in enclosing scope"

  Scenario: Shadowing across function boundary is OK
    When I evaluate:
      """
      (defun foo (y)
        (let ((x (inc y)))
          x))
      (let ((x 5))
        (foo x))
      """
    Then the result should be "6"
    And outer x should still be "5"
```

## Unsafe and FFI

```gherkin
Feature: Unsafe blocks and FFI

  Scenario: unsafe enables FFI calls
    When I evaluate:
      """
      (defun c-strlen (s)
        (unsafe
          (call @strlen (to-cstring s))))
      (c-strlen "hello")
      """
    Then the result should be "5"

  Scenario: FFI without unsafe is error
    When I evaluate:
      """
      (defun bad-strlen (s)
        (call @strlen (to-cstring s)))
      """
    Then the compiler should error "FFI call requires unsafe block"

  Scenario: unsafe does not disable ownership
    When I evaluate:
      """
      (unsafe
        (let ((x [1 2 3]))
          (let ((y x))
            (call @some_func x))))
      """
    Then the compiler should error "x has been moved"

  Scenario: safe wrapper hides unsafe
    When I evaluate:
      """
      (defun print (x)
        (unsafe
          (syscall SYS_WRITE STDOUT (to-bytes x) (len x))))
      ; user code, no unsafe needed
      (print "hello")
      """
    Then the compiler should succeed
    And output should be "hello"

  Scenario: unsafe respects closure color
    When I evaluate:
      """
      (defun make-unsafe-closure ()
        (let ((n 0))
          (fn ()
            (unsafe (call @some_func))
            (set! n (inc n)))))
      (pmap (make-unsafe-closure) [1 2 3])
      """
    Then the compiler should error "let-closure passed to parallel operation"
```

# liar

A borrow-checked Lisp built on lIR. Rust's memory safety, Lisp's metaprogramming.

```
liar source → liar → lIR → lair → LLVM IR → native
```

## Goals

1. **Memory safe without GC** — ownership, borrowing, reference counting
2. **As fast as Rust** — zero-cost abstractions, compiles to LLVM IR via lIR
3. **As lispy as possible** — s-expressions, homoiconicity, macros, first-class functions
4. **Simpler than Rust** — no lifetime annotations, no trait soup

## Core Types

### Primitives

```lisp
; Integers (i64 default)
42              ; i64
(i32 42)        ; explicit i32
(i64 42)        ; explicit i64

; Floats (f64 default)  
3.14            ; f64
(f32 3.14)      ; explicit f32

; Overflow handling
(boxed (* BIG BIGGER))   ; promotes to biginteger, never overflows
(wrapping (* x y))       ; C-style silent wrap

; Other primitives
true false      ; boolean
"hello"         ; string (immutable, UTF-8)
\a              ; character
'foo            ; symbol (interned)
:foo            ; keyword
#[0x48 0x65]    ; byte array
#r"pattern"     ; regex (reader macro)
```

### SIMD Vectors

```lisp
<<1 2 3 4>>              ; v4 i64 (inferred)
<<1.0 2.0 3.0 4.0>>      ; v4 f64 (inferred)
<i8<1 2 3 4>>            ; v4 i8 (explicit type)
<f32<1.0 2.0 3.0 4.0>>   ; v4 f32 (explicit type)

; Operations - normal arithmetic, scalar broadcast
(+ <<1 2 3 4>> <<5 6 7 8>>)   ; => <<6 8 10 12>>
(* 2 <<1 2 3 4>>)             ; => <<2 4 6 8>>
```

### Collections

```lisp
; Persistent (default) — immutable, structural sharing
[1 2 3]         ; persistent vector
{:a 1 :b 2}     ; persistent map
'(1 2 3)        ; list (linked)

; Conventional — mutable, O(1) access
<[1 2 3]>       ; conventional vector
<{:a 1 :b 2}>   ; conventional map
```

### Functions

```lisp
(fn (x) x)      ; anonymous function (closure)
```

## Ownership Model

Every value has exactly one owner. When the owner goes out of scope, the value is dropped.

### Binding Creates Ownership

```lisp
(let ((x (cons 1 2)))   ; x owns this cons cell
  (car x))              ; x is accessed
                        ; x dropped here, cons cell freed
```

### Move Semantics (Default)

Passing a value transfers ownership:

```lisp
(let ((x (cons 1 2)))
  (let ((y x))          ; x moved to y
    (car y)))           ; y owns it now
                        ; accessing x here would be compile error
```

Function arguments move by default:

```lisp
(define (consume-pair p)
  (+ (car p) (cdr p)))

(let ((x (cons 1 2)))
  (consume-pair x)      ; x moved into function
  x)                    ; ERROR: x has been moved
```

### Borrow (Temporary Access)

Borrow grants temporary read access without transferring ownership:

```lisp
(let ((x (cons 1 2)))
  (let ((view &x))      ; view borrows x
    (car view))         ; read through borrow OK
  (car x))              ; x still valid, borrow ended
```

Multiple shared borrows allowed:

```lisp
(let ((x (cons 1 2)))
  (let ((a &x)
        (b &x))         ; two borrows OK
    (+ (car a) (car b))))
```

Borrow cannot escape owner's scope:

```lisp
(define (bad-escape)
  (let ((x (cons 1 2)))
    &x))                ; ERROR: borrow would outlive x
```

### Mutable Borrow (Exclusive Access)

Mutable borrow grants exclusive read/write access:

```lisp
(let ((x (cons 1 2)))
  (let ((m &mut x))     ; exclusive mutable borrow
    (set-car! m 99))    ; mutation through borrow
  (car x))              ; => 99, borrow ended, x accessible
```

Mutable borrow is exclusive:

```lisp
(let ((x (cons 1 2)))
  (let ((m &mut x)
        (v &x))         ; ERROR: cannot borrow while mutably borrowed
    ...))
```

```lisp
(let ((x (cons 1 2)))
  (let ((m1 &mut x)
        (m2 &mut x))    ; ERROR: cannot have two mutable borrows
    ...))
```

### Clone (Deep Copy)

Explicit deep copy creates a new owned value:

```lisp
(let ((x (cons 1 2)))
  (let ((y (clone x)))  ; y owns a copy
    (set-car! y 99)     ; mutate copy
    (cons (car x)       ; x unchanged => 1
          (car y))))    ; y changed => 99
```

### Share (Reference Counting)

Share creates a reference-counted value with multiple owners:

```lisp
(let ((x (share (cons 1 2))))   ; refcount=1
  (let ((y x))                   ; refcount=2, both own it
    (car y))                     ; access through y
  (car x))                       ; still valid, refcount=1
                                 ; refcount=0, freed
```

Shared values are immutable by default:

```lisp
(let ((x (share (cons 1 2))))
  (set-car! x 99))              ; ERROR: shared values are immutable
```

For mutable shared data, use atomic cells:

```lisp
(let ((x (share (atom 0))))     ; atomic cell
  (swap! x inc)                  ; atomic update
  @x)                            ; atomic read => 1
```

## Functions

### Named Functions

```lisp
(define (add a b)
  (+ a b))

(add 1 2)  ; => 3
```

Arguments move in by default. Use sigils to change:

```lisp
; Borrow (read-only access, caller keeps ownership)
(define (peek &p)
  (car p))

(let ((x (cons 1 2)))
  (peek x)              ; x borrowed, not moved
  (car x))              ; x still valid

; Mutable borrow (exclusive access, caller keeps ownership)
(define (poke &mut p val)
  (set-car! p val))

(let ((x (cons 1 2)))
  (poke x 99)           ; x mutably borrowed
  (car x))              ; => 99

; Move (explicit, same as default)
(define (consume ^p)
  (+ (car p) (cdr p)))
```

### Closures

Closures capture variables from their environment.

```lisp
(let ((x 10))
  (fn (y) (+ x y)))     ; x captured
```

Capture mode determines ownership:

```lisp
; Capture by move (default for owned values)
(let ((data (cons 1 2)))
  (fn ()
    (car data)))        ; data moved into closure
                        ; cannot access data after this point

; Capture by borrow (closure cannot escape)
(let ((data (cons 1 2)))
  (let ((f (fn () (car &data))))  ; borrow data
    (f)                 ; OK
    (f))                ; OK
  (car data))           ; data still valid

; Capture by clone
(let ((data (cons 1 2)))
  (fn ()
    (car (clone data))) ; closure owns a copy
  (car data))           ; original still accessible
```

### Returning Functions

Functions returning closures must not capture borrows:

```lisp
; OK: captured by move
(define (make-adder n)
  (fn (x) (+ n x)))     ; n moved into closure

; ERROR: borrow would escape
(define (bad-closure)
  (let ((x 10))
    (fn () &x)))        ; x doesn't live long enough
```

### Recursion

Named recursion just works:

```lisp
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(factorial 5)  ; => 120
```

Mutual recursion:

```lisp
(define (even? n)
  (if (= n 0) true (odd? (- n 1))))

(define (odd? n)
  (if (= n 0) false (even? (- n 1))))
```

No Y combinator needed. The environment resolves names.

## Data Structures

### Pairs and Lists

```lisp
(cons 1 2)              ; pair: (1 . 2)
(cons 1 (cons 2 nil))   ; list: (1 2)
'(1 2 3)                ; quoted list (immutable)
(list 1 2 3)            ; constructed list

(car '(1 2 3))          ; => 1
(cdr '(1 2 3))          ; => (2 3)
```

### Vectors

```lisp
[1 2 3]                 ; vector literal
(vec 1 2 3)             ; constructed vector

(get v 0)               ; index access
(set v 0 99)            ; returns new vector (immutable)
(set! &mut v 0 99)      ; mutate in place
(len v)                 ; length
```

### Maps

```lisp
{:a 1 :b 2}             ; map literal
(hash-map :a 1 :b 2)    ; constructed map

(get m :a)              ; => 1
(assoc m :c 3)          ; returns new map with :c
(dissoc m :a)           ; returns new map without :a
```

### Structs

User-defined product types:

```lisp
(defstruct Point
  x y)

(let ((p (Point 10 20)))
  (Point-x p))          ; => 10

; With mutability
(let ((p (Point 10 20)))
  (set-Point-x! &mut p 99)
  (Point-x p))          ; => 99
```

### Enums

User-defined sum types:

```lisp
(defenum Option
  (Some value)
  None)

(defenum Result
  (Ok value)
  (Err error))

(let ((x (Some 42)))
  (match x
    ((Some v) v)
    (None 0)))          ; => 42
```

## Control Flow

### Conditionals

```lisp
(if condition
    then-expr
    else-expr)

(cond
  (test1 result1)
  (test2 result2)
  (else default))

(when condition
  body...)              ; returns nil if false

(unless condition
  body...)
```

### Pattern Matching

```lisp
(match value
  (pattern1 result1)
  (pattern2 result2)
  (_ default))

; Patterns
42                      ; literal
x                       ; binding (moves value)
&x                      ; binding (borrows value)
_                       ; wildcard
(cons a b)              ; destructuring
(Point x y)             ; struct destructuring
(Some value)            ; enum variant
(Some &value)           ; enum variant, borrow inner
[a b c]                 ; vector destructuring
[first . rest]          ; vector with rest
```

Ownership in patterns:

```lisp
(let ((x (Some (cons 1 2))))
  (match x
    ((Some pair) (car pair))  ; pair moved out of x
    (None 0)))
; x is now in moved state (inner was extracted)

(let ((x (Some (cons 1 2))))
  (match x
    ((Some &pair) (car pair)) ; pair borrowed from x
    (None 0)))
; x still valid
```

### Loops

```lisp
; Functional iteration (preferred)
(map (fn (x) (* x 2)) '(1 2 3))     ; => (2 4 6)
(filter odd? '(1 2 3 4 5))          ; => (1 3 5)
(fold + 0 '(1 2 3))                 ; => 6

; Imperative loop
(loop ((i 0)
       (sum 0))
  (if (>= i 10)
      sum
      (recur (+ i 1) (+ sum i))))   ; => 45

; For comprehension
(for ((x '(1 2 3))
      (y '(a b)))
  (cons x y))                       ; => ((1 . a) (1 . b) (2 . a) ...)
```

## Macros

Macros are compile-time functions that transform code.

```lisp
(defmacro when (condition &rest body)
  `(if ,condition
       (do ,@body)
       nil))

(when (> x 0)
  (print "positive")
  x)

; Expands to:
(if (> x 0)
    (do (print "positive") x)
    nil)
```

### Quasiquoting

```lisp
`(a b c)                ; => (a b c)
`(a ,x c)               ; => (a <value-of-x> c)
`(a ,@xs c)             ; => (a <spliced-xs> c)
```

### Hygiene

Macros use gensym for internal bindings:

```lisp
(defmacro swap (a b)
  (let ((temp (gensym)))
    `(let ((,temp ,a))
       (set! ,a ,b)
       (set! ,b ,temp))))
```

### Compile-time Computation

Macros can do arbitrary computation:

```lisp
(defmacro compile-time-factorial (n)
  (let ((result (factorial n)))  ; runs at compile time
    result))

(compile-time-factorial 5)       ; compiles to literal 120
```

## Ownership Annotations Summary

| Syntax | Meaning |
|--------|---------|
| `x` | Move/own (default) |
| `&x` | Shared borrow |
| `&mut x` | Mutable borrow |
| `(clone x)` | Deep copy |
| `(share x)` | Reference counted |
| `^x` | Explicit move (in function params) |

## Error Handling

```lisp
; Result type
(define (divide a b)
  (if (= b 0)
      (Err "division by zero")
      (Ok (/ a b))))

; Propagate errors with ?
(define (compute x y)
  (let ((a (divide x y)?))      ; early return if Err
    (+ a 1)))

; Or handle explicitly
(match (divide 10 2)
  ((Ok v) v)
  ((Err e) (panic e)))
```

## Modules

```lisp
; math.cor
(module math)

(define (square x) (* x x))
(define (cube x) (* x x x))

(export square cube)

; main.cor
(use math)
(math/square 5)         ; => 25

(use math :only (square))
(square 5)              ; => 25
```

## Complete Example

```lisp
; A functional queue with amortized O(1) operations

(defstruct Queue
  inbox outbox)

(define (empty-queue)
  (Queue '() '()))

(define (enqueue &q item)
  (Queue (cons item (Queue-inbox q))
         (Queue-outbox q)))

(define (dequeue &mut q)
  (when (nil? (Queue-outbox q))
    (set-Queue-outbox! q (reverse (Queue-inbox q)))
    (set-Queue-inbox! q '()))
  (if (nil? (Queue-outbox q))
      None
      (let ((item (car (Queue-outbox q))))
        (set-Queue-outbox! q (cdr (Queue-outbox q)))
        (Some item))))

; Usage
(let ((q (empty-queue)))
  (let ((q (enqueue q 1)))
    (let ((q (enqueue q 2)))
      (let ((q (enqueue q 3)))
        (match (dequeue &mut q)
          ((Some x) x)      ; => 1
          (None 0))))))
```

## What's Not Allowed

```lisp
; Use after move
(let ((x (cons 1 2)))
  (consume x)
  x)                    ; ERROR: x has been moved

; Borrow escapes scope
(define (escape)
  (let ((x 10))
    &x))                ; ERROR: x does not live long enough

; Mutable aliasing
(let ((x (cons 1 2)))
  (let ((a &mut x)
        (b &x))         ; ERROR: cannot borrow x while mutably borrowed
    ...))

; Mutate shared data
(let ((x (share (cons 1 2))))
  (set-car! x 99))      ; ERROR: cannot mutate shared value

; Return reference to local
(define (bad)
  (let ((v [1 2 3]))
    &(get v 0)))        ; ERROR: cannot return reference to local

; Self-referential closure without share
(let ((f (fn () (f)))) ; ERROR: f captured before defined
  (f))

; Use this instead:
(define (f) (f))        ; OK: named recursion
```

## Implementation Notes

liar compiles to lIR (an S-expression assembly for LLVM IR), which compiles to native code.

The borrow checker runs as a pass over the AST before code generation. It tracks:
- Ownership state for each binding
- Active borrows and their scopes
- Move detection
- Lifetime analysis

Reference counting (for `share`) generates atomic increment/decrement operations in lIR. The compiler inserts drops at scope exits automatically.

Macros expand before the borrow checker runs, so they have full freedom to generate any code — ownership rules apply to the expanded form.

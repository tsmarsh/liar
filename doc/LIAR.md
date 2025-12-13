# liar

A Lisp that compiles to native code via LLVM. S-expressions meet systems programming.

```
liar source → liar → lIR → lair → LLVM IR → native
```

## Quick Start

```lisp
;; hello.liar
(defun main () -> i64
  (println "Hello, World!")
  0)
```

```bash
liarc hello.liar -o hello && ./hello
```

## Goals

1. **Fast** — Compiles to native code via LLVM IR
2. **Lispy** — S-expressions, macros, first-class functions
3. **Simple** — Minimal core, build abstractions in the language itself
4. **Practical** — FFI to C, real data structures, file I/O

## Types

### Primitives

```lisp
42              ; integer (i64)
3.14            ; float (double)
"hello"         ; string (null-terminated, UTF-8)
true false      ; boolean
nil             ; null pointer
:keyword        ; keyword (interned symbol)
```

### Type Annotations

Functions can have explicit type annotations:

```lisp
(defun add (a: i64 b: i64) -> i64
  (+ a b))

(defun greet (name: ptr) -> i64
  (println name))
```

Available types: `i64`, `i32`, `i16`, `i8`, `i1`, `double`, `float`, `ptr`, `void`

### Structs

User-defined product types with typed fields:

```lisp
(defstruct Point (x: i64 y: i64))

;; Constructor
(Point 10 20)

;; Field access with .
(let ((p (Point 10 20)))
  (+ (. p x) (. p y)))  ; => 30

;; Heap-allocated (for returning, storing in collections)
(share (Point 10 20))
```

## Functions

### Named Functions

```lisp
(defun square (x)
  (* x x))

(defun factorial (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
```

### Anonymous Functions (Closures)

```lisp
(fn (x) (* x 2))

;; With capture
(let ((multiplier 3))
  (fn (x) (* x multiplier)))
```

### Tail Call Optimization

Direct tail calls are optimized:

```lisp
(defun loop-forever (n)
  (loop-forever (+ n 1)))  ; no stack overflow
```

## Control Flow

### Conditionals

```lisp
(if (> x 0)
    "positive"
    "non-positive")
```

`if` always requires both branches. Both branches must have compatible types.

### Sequencing

```lisp
(do
  (println "first")
  (println "second")
  42)  ; returns last expression
```

## Bindings

### Let (Sequential)

```lisp
(let ((x 1)
      (y (+ x 1)))  ; can reference earlier bindings
  (+ x y))          ; => 3
```

### Destructuring Let

```lisp
(defstruct Point (x: i64 y: i64))

(let (((Point x y) (Point 10 20)))
  (+ x y))  ; => 30
```

### Parallel Let

```lisp
(plet ((a (expensive-computation-1))
       (b (expensive-computation-2)))
  (+ a b))
```

Bindings evaluate in parallel. Cannot reference each other.

## Macros

Macros transform code at compile time.

### Defining Macros

```lisp
(defmacro unless (cond then else)
  `(if ,cond ,else ,then))

(unless (= x 0)
  (/ 10 x)
  0)
```

### Quasiquoting

```lisp
`(a b c)        ; => (a b c)
`(a ,x c)       ; => (a <value-of-x> c)
`(a ,@xs c)     ; => (a <spliced-elements-of-xs> c)
```

### Gensym

Generate unique symbols to avoid capture:

```lisp
(defmacro with-temp (body)
  (let ((tmp (gensym "tmp")))
    `(let ((,tmp 0))
       ,body)))
```

### Macro Calling Macros

Macros can call other macros during expansion:

```lisp
(defmacro double (x) `(+ ,x ,x))
(defmacro quadruple (x) (double (double x)))

(quadruple 5)  ; => 20
```

## Protocols

Protocols define interfaces that types can implement.

### Defining Protocols

```lisp
(defprotocol Countable
  (count [self]))

(defprotocol Indexable
  (nth [self idx]))
```

### Implementing Protocols

```lisp
(defstruct MyList (data: ptr len: i64))

(extend-protocol Countable MyList
  (count [self] (. self len)))

(extend-protocol Indexable MyList
  (nth [self idx] (aget (. self data) idx)))
```

### Protocol Defaults

Types implementing one protocol can get default implementations of another:

```lisp
(defprotocol Seq
  (first [self])
  (rest [self]))

;; Any type implementing Seq automatically gets Countable
(extend-protocol-default Countable Seq
  (count [self]
    (if (nil? (rest self))
        1
        (+ 1 (count (rest self))))))
```

### Runtime Dispatch

Protocol methods dispatch based on the runtime type of `self`:

```lisp
(defun process (item)
  (count item))  ; works with any Countable type
```

## FFI (Foreign Function Interface)

Call C functions directly:

```lisp
;; Declare external function
(extern printf i32 (ptr ...))  ; varargs with ...
(extern malloc ptr (i64))
(extern strlen i64 (ptr))

;; Use it
(printf "Value: %ld\n" 42)
```

Common declarations:
```lisp
(extern open i32 (ptr i32 i32))
(extern close i32 (i32))
(extern read i64 (i32 ptr i64))
(extern write i64 (i32 ptr i64))
```

## Atoms (Thread-Safe State)

Atoms provide thread-safe mutable state:

```lisp
;; Create
(atom 0)

;; Read (deref)
@counter

;; Update atomically
(swap! counter (fn (x) (+ x 1)))

;; Set directly
(reset! counter 0)

;; Compare-and-set
(compare-and-set! counter 0 1)  ; returns 1 if successful
```

## Built-in Operations

### Arithmetic

| Op | Description |
|----|-------------|
| `+` `-` `*` `/` | Integer arithmetic |
| `rem` | Integer remainder |
| `+.` `-.` `*.` `/.` | Float arithmetic |
| `%.` | Float remainder |

### Comparison

| Op | Description |
|----|-------------|
| `=` `!=` | Equality |
| `<` `>` `<=` `>=` | Integer comparison |
| `=.` `!=.` `<.` `>.` | Float comparison |

### Boolean

| Op | Description |
|----|-------------|
| `and` `or` `not` | Logical operations |

### Bitwise

| Op | Description |
|----|-------------|
| `bit-and` `bit-or` `bit-xor` `bit-not` | Bitwise logic |
| `bit-shift-left` `bit-shift-right` | Shifts (logical) |
| `arithmetic-shift-right` | Arithmetic shift |
| `popcount` | Count set bits |

### Type Conversions

```lisp
;; Integer truncation/extension
(trunc i8 255)      ; truncate to 8 bits
(zext i64 (trunc i8 255))  ; zero-extend back
(sext i64 (trunc i8 255))  ; sign-extend back

;; Float conversions
(fptosi i64 3.7)    ; float to signed int => 3
(sitofp double 42)  ; signed int to float => 42.0
(fptrunc float 3.14159)  ; double to float
(fpext double x)    ; float to double
```

### Array Operations

```lisp
(heap-array 10)           ; allocate 10-element array
(aget arr idx)            ; get element
(aset arr idx val)        ; set element (returns val)
(array-copy size dest src) ; copy arrays
```

### Pointer Operations

```lisp
(nil? ptr)          ; check if null
(ptr+ ptr offset)   ; add byte offset to pointer
(store-byte ptr val) ; store byte at pointer
(load-byte ptr)     ; load byte from pointer
```

### I/O

```lisp
(print x)    ; print value (no newline)
(println x)  ; print value with newline
```

### Memory

```lisp
(share (StructName fields...))  ; heap-allocate struct
```

## Standard Library

The standard library lives in `lib/` and provides:

### seq.liar - Sequences

Core protocols and list operations:

```lisp
;; Protocols: Seq, Countable, Indexable, Collection,
;;            Mappable, Filterable, Reducible, etc.

;; Cons lists
(cons 1 (cons 2 (cons 3 nil)))
(list3 1 2 3)  ; same as above

;; Operations (on any Seq)
(first xs)      ; head
(rest xs)       ; tail
(count xs)      ; length
(nth xs 2)      ; element at index

;; Higher-order (note: collection-first argument order)
(map xs (fn (x) (* x 2)))
(filter xs (fn (x) (> x 0)))
(reduce xs (fn (acc x) (+ acc x)) 0)
(for-each xs println)

;; Transformations
(reverse xs)
(append xs ys)
(take 3 xs)
(drop 3 xs)

;; Predicates
(any (fn (x) (= x 0)) xs)
(all (fn (x) (> x 0)) xs)
(member 5 xs)

;; Numeric
(sum xs)
(product xs)
(minimum xs)
(maximum xs)
```

### vector.liar - Persistent Vectors

Immutable vectors with O(log32 n) operations:

```lisp
(vector)            ; empty vector
(vec (list3 1 2 3)) ; from list

(vec-count v)       ; length
(vec-nth v idx)     ; get element
(vec-conj v x)      ; append (returns new vector)
(vec-assoc v idx x) ; update (returns new vector)
(vec-first v)       ; first element
(vec-peek v)        ; last element
```

### io.liar - File I/O

```lisp
;; High-level
(slurp "/path/to/file")  ; read entire file as string
(spit "/path/to/file" "content")  ; write string to file
(spit-append "/path" "more")  ; append to file

;; Low-level
(file-open-read path)
(file-open-write path)
(file-close fd)
(read-bytes fd buf len)
(write-bytes fd buf len)

;; Console
(print-str "hello")
(println-str "hello")
(eprint "error")
(eprintln "error")
```

### hashmap.liar - Hash Maps

Persistent hash array mapped trie (integer keys only):

```lisp
(hash-map)          ; empty map
(hm-get m key)      ; get value (0 if not found)
(hm-assoc m key val) ; returns new map with key=val
(hm-contains? m key) ; 0 or 1
(hm-count m)        ; number of entries
(hm-empty? m)       ; 0 or 1
```

### hashset.liar - Hash Sets

Persistent set (integer elements only):

```lisp
(hash-set)          ; empty set
(hs-contains? s x)  ; 0 or 1
(hs-add s x)        ; returns new set with x
(hs-count s)        ; number of elements
(hs-empty? s)       ; 0 or 1
```

## Complete Example

```lisp
;; Factorial with accumulator (tail recursive)
(defun fact-acc (n acc)
  (if (= n 0)
      acc
      (fact-acc (- n 1) (* n acc))))

(defun factorial (n)
  (fact-acc n 1))

;; Using the sequence library
(defun sum-squares (xs: ptr)
  (reduce (map xs (fn (x) (* x x)))
          (fn (acc x) (+ acc x))
          0))

;; Protocol-based polymorphism
(defprotocol Describable
  (describe [self]))

(defstruct Person (name: ptr age: i64))

(extend-protocol Describable Person
  (describe [self]
    (println (. self name))))

(defun main () -> i64
  (let ((p (share (Person "Alice" 30))))
    (describe p)
    (println (factorial 10)))
  0)
```

## Implementation Notes

liar compiles through several stages:

1. **Parse** — S-expressions to AST
2. **Macro Expand** — Run compile-time transformations
3. **Resolve** — Name resolution, capture analysis
4. **Closure Convert** — Transform closures to structs + functions
5. **Codegen** — Generate lIR (S-expression assembly)
6. **LIR → LLVM** — lair compiles to LLVM IR
7. **LLVM → Native** — Standard LLVM compilation

### Not Yet Implemented

- Pattern matching / match expressions
- Algebraic data types (enums)
- Module system
- Full borrow checking
- Garbage collection (use manual memory management or share)

# lIR Language Guide

lIR (pronounced "liar") is an S-expression assembler for LLVM IR. It's a 1:1 mapping to LLVM IR using S-expression syntax. This is not a Lisp—there's no evaluation model, no macros, no special forms. Just LLVM IR in parentheses.

## Philosophy

lIR is the foundation layer for building higher-level languages. It provides:
- Exact LLVM IR semantics
- No implicit type conversions
- No operator overloading
- Predictable, explicit behavior

## Types

### Integer Types

| Type  | Bits | Signed Range | Unsigned Range |
|-------|------|--------------|----------------|
| `i1`  | 1    | 0, 1         | 0, 1           |
| `i8`  | 8    | -128 to 127  | 0 to 255       |
| `i16` | 16   | -32768 to 32767 | 0 to 65535  |
| `i32` | 32   | -2³¹ to 2³¹-1 | 0 to 2³²-1    |
| `i64` | 64   | -2⁶³ to 2⁶³-1 | 0 to 2⁶⁴-1    |

```lisp
(i1 1)         ; true
(i1 0)         ; false
(i8 42)
(i32 -1)
(i64 9223372036854775807)
```

### Float Types

| Type     | Bits | Description          |
|----------|------|----------------------|
| `float`  | 32   | IEEE 754 single      |
| `double` | 64   | IEEE 754 double      |

```lisp
(float 3.14)
(double 2.718281828)
(double 1.0e-10)     ; scientific notation
(double inf)         ; infinity
(double -inf)
(double nan)         ; not a number
```

### Vector Types

Fixed-size SIMD vectors: `<N x type>`

```lisp
(<4 x i32> 1 2 3 4)
(<2 x double> 1.0 2.0)
(<8 x i8> 0 1 2 3 4 5 6 7)
```

### Pointer Type

Opaque pointer type (LLVM 15+ style):

```lisp
ptr              ; pointer type
(ptr null)       ; null pointer literal
```

### Struct Types

Named struct types for aggregate data:

```lisp
%struct.point    ; references struct type "point"
{ (i32 1) (i32 2) }  ; anonymous struct literal
```

## Operations

### Integer Arithmetic

All operands must have matching types.

| Op      | Description              | Example |
|---------|--------------------------|---------|
| `add`   | Addition                 | `(add (i32 5) (i32 3))` → `(i32 8)` |
| `sub`   | Subtraction              | `(sub (i32 10) (i32 3))` → `(i32 7)` |
| `mul`   | Multiplication           | `(mul (i32 6) (i32 7))` → `(i32 42)` |
| `sdiv`  | Signed division          | `(sdiv (i32 -10) (i32 3))` → `(i32 -3)` |
| `udiv`  | Unsigned division        | `(udiv (i32 -1) (i32 2))` → `(i32 2147483647)` |
| `srem`  | Signed remainder         | `(srem (i32 -10) (i32 3))` → `(i32 -1)` |
| `urem`  | Unsigned remainder       | `(urem (i32 10) (i32 3))` → `(i32 1)` |

### Float Arithmetic

| Op      | Description              | Example |
|---------|--------------------------|---------|
| `fadd`  | Addition                 | `(fadd (double 1.5) (double 2.5))` → `(double 4.0)` |
| `fsub`  | Subtraction              | `(fsub (double 5.0) (double 3.0))` → `(double 2.0)` |
| `fmul`  | Multiplication           | `(fmul (double 2.5) (double 4.0))` → `(double 10.0)` |
| `fdiv`  | Division                 | `(fdiv (double 7.0) (double 2.0))` → `(double 3.5)` |
| `frem`  | Remainder                | `(frem (double 10.0) (double 3.0))` → `(double 1.0)` |

### Bitwise Operations

| Op     | Description              | Example |
|--------|--------------------------|---------|
| `and`  | Bitwise AND              | `(and (i8 0b1100) (i8 0b1010))` → `(i8 0b1000)` |
| `or`   | Bitwise OR               | `(or (i8 0b1100) (i8 0b1010))` → `(i8 0b1110)` |
| `xor`  | Bitwise XOR              | `(xor (i8 0b1100) (i8 0b1010))` → `(i8 0b0110)` |
| `shl`  | Shift left               | `(shl (i32 1) (i32 4))` → `(i32 16)` |
| `lshr` | Logical shift right      | `(lshr (i32 -1) (i32 24))` → `(i32 255)` |
| `ashr` | Arithmetic shift right   | `(ashr (i32 -1) (i32 24))` → `(i32 -1)` |

Boolean logic uses `i1` with bitwise ops:
```lisp
(and (i1 1) (i1 0))  ; => (i1 0) - logical AND
(or (i1 0) (i1 1))   ; => (i1 1) - logical OR
(xor (i1 1) (i1 1))  ; => (i1 0) - logical XOR
```

## Comparisons

### Integer Comparison (icmp)

Returns `i1`. Predicates determine signed vs unsigned interpretation.

**Signed predicates:**
```lisp
(icmp eq (i32 5) (i32 5))    ; => (i1 1) equal
(icmp ne (i32 5) (i32 6))    ; => (i1 1) not equal
(icmp slt (i32 -1) (i32 1))  ; => (i1 1) signed less than
(icmp sle (i32 5) (i32 5))   ; => (i1 1) signed less or equal
(icmp sgt (i32 1) (i32 -1))  ; => (i1 1) signed greater than
(icmp sge (i32 5) (i32 5))   ; => (i1 1) signed greater or equal
```

**Unsigned predicates:**
```lisp
(icmp ult (i32 -1) (i32 1))  ; => (i1 0) -1 as unsigned is MAX
(icmp ule (i32 1) (i32 2))   ; => (i1 1)
(icmp ugt (i32 -1) (i32 1))  ; => (i1 1) -1 as unsigned is MAX
(icmp uge (i32 2) (i32 1))   ; => (i1 1)
```

### Float Comparison (fcmp)

Returns `i1`. Ordered predicates return false if either operand is NaN.

**Ordered predicates (false if NaN):**
```lisp
(fcmp oeq (double 1.0) (double 1.0))  ; => (i1 1)
(fcmp one (double 1.0) (double 2.0))  ; => (i1 1)
(fcmp olt (double 1.0) (double 2.0))  ; => (i1 1)
(fcmp ole (double 1.0) (double 1.0))  ; => (i1 1)
(fcmp ogt (double 2.0) (double 1.0))  ; => (i1 1)
(fcmp oge (double 1.0) (double 1.0))  ; => (i1 1)
(fcmp ord (double 1.0) (double 2.0))  ; => (i1 1) both ordered (not NaN)
```

**Unordered predicates (true if NaN):**
```lisp
(fcmp ueq (double nan) (double 1.0))  ; => (i1 1)
(fcmp une (double nan) (double 1.0))  ; => (i1 1)
(fcmp ult (double nan) (double 1.0))  ; => (i1 1)
(fcmp ule (double nan) (double 1.0))  ; => (i1 1)
(fcmp ugt (double nan) (double 1.0))  ; => (i1 1)
(fcmp uge (double nan) (double 1.0))  ; => (i1 1)
(fcmp uno (double nan) (double 1.0))  ; => (i1 1) either unordered (is NaN)
```

## Conversions

### Integer Conversions

| Op      | Direction     | Example |
|---------|---------------|---------|
| `trunc` | Larger → smaller | `(trunc i8 (i32 257))` → `(i8 1)` |
| `zext`  | Smaller → larger (zero-fill) | `(zext i32 (i8 255))` → `(i32 255)` |
| `sext`  | Smaller → larger (sign-fill) | `(sext i32 (i8 -1))` → `(i32 -1)` |

Key difference:
```lisp
(zext i16 (i8 128))   ; => (i16 128)  - zero-extended
(sext i16 (i8 128))   ; => (i16 -128) - sign-extended (128 is -128 as i8)
```

### Float Conversions

| Op        | Direction         | Example |
|-----------|-------------------|---------|
| `fptrunc` | double → float    | `(fptrunc float (double 3.14159))` → `(float 3.14159)` |
| `fpext`   | float → double    | `(fpext double (float 1.5))` → `(double 1.5)` |

### Int/Float Conversions

| Op       | Conversion          | Example |
|----------|---------------------|---------|
| `fptoui` | float → unsigned int | `(fptoui i32 (double 42.9))` → `(i32 42)` |
| `fptosi` | float → signed int   | `(fptosi i32 (double -42.9))` → `(i32 -42)` |
| `uitofp` | unsigned int → float | `(uitofp double (i8 255))` → `(double 255.0)` |
| `sitofp` | signed int → float   | `(sitofp double (i8 -1))` → `(double -1.0)` |

Key difference:
```lisp
(uitofp double (i8 -1))  ; => (double 255.0) - treats -1 as 255
(sitofp double (i8 -1))  ; => (double -1.0)  - treats -1 as -1
```

## Control Flow

### Select

Conditional value selection (ternary operator):

```lisp
(select condition true-value false-value)
```

```lisp
(select (i1 1) (i32 10) (i32 20))                    ; => (i32 10)
(select (icmp slt (i32 5) (i32 10)) (i32 1) (i32 2)) ; => (i32 1)
```

Note: Both branches are evaluated. This is not short-circuit evaluation.

### Basic Blocks and Branches

Functions contain basic blocks, each with a label. Control flow uses branches.

```lisp
; Unconditional branch
(br label)

; Conditional branch
(br condition true-label false-label)
```

```lisp
(define (max i32) ((i32 a) (i32 b))
  (block entry
    (br (icmp sgt a b) a_wins b_wins))
  (block a_wins
    (br done))
  (block b_wins
    (br done))
  (block done
    (ret (phi i32 (a_wins a) (b_wins b)))))
```

### Phi Nodes

Phi nodes select a value based on which predecessor block was executed.

```lisp
(phi type (label1 value1) (label2 value2) ...)
```

Each pair is `(block-label value)` - the value to use if control came from that block.

```lisp
; Select between two values based on control flow
(phi i32 (neg (i32 0)) (pos x))

; Loop counter
(phi i32 (entry (i32 0)) (loop next-i))
```

Phi nodes must be at the start of a block (before other instructions).

### Return

```lisp
(ret value)     ; Return a value
(ret)           ; Return void
```

## Memory Operations

### alloca

Allocate stack memory:

```lisp
(alloca type)           ; allocate one element
(alloca type count)     ; allocate array (count must be integer)
```

```lisp
(alloca i32)            ; allocate space for one i32, returns ptr
(alloca i64 (i32 10))   ; allocate array of 10 i64s
(alloca ptr)            ; allocate space for a pointer
```

### load

Load a value from memory:

```lisp
(load type ptr)
```

```lisp
(load i32 p)            ; load i32 from pointer p
(load ptr p)            ; load pointer from pointer p
```

### store

Store a value to memory:

```lisp
(store value ptr)
```

```lisp
(store (i32 42) p)      ; store 42 to pointer p
(store q p)             ; store pointer q to pointer p
```

### getelementptr

Pointer arithmetic:

```lisp
(getelementptr type base-ptr indices...)
(getelementptr inbounds type base-ptr indices...)
```

```lisp
; Index into array
(getelementptr i32 arr (i64 5))          ; arr[5]

; Index into struct
(getelementptr %struct.point p (i32 0) (i32 1))  ; &p->field1
```

## Aggregate Operations

### extractvalue

Extract a field from a struct:

```lisp
(extractvalue aggregate index...)
```

```lisp
(extractvalue { (i32 10) (i32 20) } 0)   ; => (i32 10)
(extractvalue { (i32 10) (i32 20) } 1)   ; => (i32 20)
```

### insertvalue

Insert a value into a struct (returns new struct):

```lisp
(insertvalue aggregate value index...)
```

```lisp
(insertvalue { (i32 1) (i32 2) } (i32 99) 0)
; => { (i32 99) (i32 2) }
```

## Let Bindings

Name intermediate SSA values for readability:

```lisp
(let ((name value)...) body...)
```

```lisp
(let ((x (add (i32 1) (i32 2))))
  (mul x x))
; => (i32 9)

(let ((a (i32 10))
      (b (i32 20)))
  (add a b))
; => (i32 30)
```

Let bindings are lexically scoped and can reference earlier bindings.

## Vector Operations

### extractelement

Get a single element from a vector:

```lisp
(extractelement (<4 x i32> 10 20 30 40) (i32 0))  ; => (i32 10)
(extractelement (<4 x i32> 10 20 30 40) (i32 3))  ; => (i32 40)
```

### insertelement

Replace an element in a vector (returns new vector):

```lisp
(insertelement (<4 x i32> 1 2 3 4) (i32 99) (i32 0))  ; => (<4 x i32> 99 2 3 4)
```

### shufflevector

Rearrange elements from two vectors using a mask:

```lisp
; Indices 0-3 from first vector, 4-7 from second
(shufflevector (<4 x i32> 1 2 3 4) (<4 x i32> 5 6 7 8) (<4 x i32> 0 4 1 5))
; => (<4 x i32> 1 5 2 6) - interleaved

; Reverse
(shufflevector (<4 x i32> 1 2 3 4) (<4 x i32> 0 0 0 0) (<4 x i32> 3 2 1 0))
; => (<4 x i32> 4 3 2 1)

; Broadcast element 0
(shufflevector (<4 x i32> 1 2 3 4) (<4 x i32> 0 0 0 0) (<4 x i32> 0 0 0 0))
; => (<4 x i32> 1 1 1 1)
```

## Type Errors

lIR has strict type checking. Common errors:

```lisp
; Type mismatch - operands must match
(add (i8 1) (i32 2))  ; ERROR: type mismatch

; Wrong operation for type
(add (double 1.0) (double 2.0))   ; ERROR: use fadd for floats
(fadd (i32 1) (i32 2))            ; ERROR: use add for integers

; Invalid conversion direction
(trunc i64 (i32 42))  ; ERROR: cannot truncate to larger type
(zext i8 (i32 42))    ; ERROR: cannot extend to smaller type
```

## Top-Level Forms

### Function Definitions

```lisp
(define (name return-type) ((type param)...) body...)
```

```lisp
(define (add-one i32) ((i32 x))
  (block entry
    (ret (add x (i32 1)))))

(define (sum ptr) ((ptr a) (ptr b))
  ...)
```

### External Declarations

```lisp
(declare name return-type (param-types...) [varargs])
```

```lisp
(declare printf i32 (ptr) ...)     ; varargs function
(declare malloc ptr (i64))
(declare exit void (i32))
```

### Global Variables

```lisp
(global name type value)           ; mutable global
(constant name type value)         ; immutable constant
```

```lisp
(global counter i32 (i32 0))
(constant pi double (double 3.14159))
(constant message ptr (string "hello\n"))
```

### Struct Definitions

```lisp
(defstruct name (field-types...))
```

```lisp
(defstruct point (double double))
(defstruct person (ptr i32 double))
```

## Function Calls

```lisp
(call @function-name args...)
```

```lisp
(call @add-one (i32 5))
(call @printf fmt arg1 arg2)
```

## Design Principles

1. **No bool**: Use `i1` for boolean values
2. **No f32/f64**: Use `float` and `double`
3. **No promotion**: `(add (i8 1) (i32 2))` is an error, not promotion
4. **Explicit ops**: Integer `add` vs float `fadd`
5. **LLVM semantics**: Everything behaves exactly like LLVM IR

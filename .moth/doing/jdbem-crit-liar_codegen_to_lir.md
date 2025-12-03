## Summary
Generate lIR from typed, ownership-checked AST.

## Code generation context
```rust
pub struct CodeGen {
    items: Vec<lir_core::Item>,
    current_function: Option<FunctionBuilder>,
    locals: HashMap<BindingId, String>,  // binding → lIR variable name
    label_counter: usize,
    temp_counter: usize,
}

struct FunctionBuilder {
    name: String,
    params: Vec<(String, lir_core::ParamType)>,
    return_type: lir_core::ReturnType,
    blocks: Vec<BlockBuilder>,
    current_block: usize,
}
```

## Translation rules

### Primitives
```lisp
; liar                → lIR
42                   → (i64 42)
3.14                 → (double 3.14)
true                 → (i1 1)
false                → (i1 0)
```

### Arithmetic
```lisp
; liar                → lIR
(+ a b)              → (add a b)      ; integers
(+ a b)              → (fadd a b)     ; floats
(- a b)              → (sub a b)
(* a b)              → (mul a b)
(/ a b)              → (sdiv a b)     ; signed
(rem a b)            → (srem a b)
```

### Comparison
```lisp
; liar                → lIR
(= a b)              → (icmp eq a b)
(< a b)              → (icmp slt a b)
(> a b)              → (icmp sgt a b)
(<= a b)             → (icmp sle a b)
(>= a b)             → (icmp sge a b)
```

### Conditionals
```lisp
; liar
(if cond then else)

; lIR
(block entry
  (br cond then_label else_label))
(block then_label
  ... then code ...
  (br merge))
(block else_label
  ... else code ...
  (br merge))
(block merge
  (ret (phi T (then_label then_val) (else_label else_val))))
```

### Let bindings
```lisp
; liar
(let ((x 10) (y 20))
  (+ x y))

; lIR
(let ((x (i64 10))
      (y (i64 20)))
  (add x y))
```

### Functions
```lisp
; liar
(define (add a b)
  (+ a b))

; lIR
(define (add i64) ((i64 a) (i64 b))
  (block entry
    (ret (add a b))))
```

### Closures
Closures become a struct (environment) + function:

```lisp
; liar
(let ((x 10))
  (fn (y) (+ x y)))

; lIR
(defstruct closure_env_0 (i64))  ; captured x

(define (closure_fn_0 i64) ((ptr env) (i64 y))
  (block entry
    (let ((x (load i64 (getelementptr %struct.closure_env_0 env (i64 0) (i32 0)))))
      (ret (add x y)))))

; At closure creation site:
(let ((env (alloca i64)))
  (store (i64 10) env)
  ; return {env, closure_fn_0} as closure object
  ...)
```

### Structs
```lisp
; liar
(defstruct Point x y)
(Point 10 20)
(Point-x p)

; lIR
(defstruct Point (i64 i64))

; Constructor - allocate and populate
(let ((p (alloca i64 (i32 2))))
  (store (i64 10) (getelementptr %struct.Point p (i64 0) (i32 0)))
  (store (i64 20) (getelementptr %struct.Point p (i64 0) (i32 1)))
  p)

; Accessor
(load i64 (getelementptr %struct.Point p (i64 0) (i32 0)))
```

### Pattern matching
Compiles to nested conditionals:

```lisp
; liar
(match x
  (0 "zero")
  (1 "one")
  (_ "many"))

; lIR
(block entry
  (br (icmp eq x (i64 0)) case_0 check_1))
(block check_1
  (br (icmp eq x (i64 1)) case_1 case_default))
(block case_0
  (br merge))
(block case_1
  (br merge))
(block case_default
  (br merge))
(block merge
  (ret (phi ptr (case_0 str_zero) (case_1 str_one) (case_default str_many))))
```

### Memory management
Insert drops at scope exits:

```lisp
; liar
(let ((x (make-big-thing)))
  (use x))
; x dropped here

; lIR
(let ((x (call @make_big_thing)))
  (call @use x)
  (call @drop_big_thing x))  ; inserted by compiler
```

## Acceptance criteria
- [ ] All expression types compile
- [ ] Functions compile with correct signatures
- [ ] Closures compile to struct + function
- [ ] Structs compile to lIR structs
- [ ] Pattern matching compiles to branches
- [ ] Drop calls inserted at scope exits
- [ ] Output is valid lIR (parseable by lir-core)

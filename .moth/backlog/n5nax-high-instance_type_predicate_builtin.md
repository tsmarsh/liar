# instance? Type Predicate Builtin

Add `(instance? x Type)` builtin to check if a value is an instance of a struct type at runtime.

**Priority:** HIGH (required for stdlib flatten, mapcat, etc.)

## Context

Every struct allocated via `share` already stores a type ID at field 0:
```
[type_id | field1 | field2 | ...]
```

Protocol dispatch uses this for runtime polymorphism. We need to expose it for type predicates.

## Design

### Syntax
```lisp
(instance? x Cons)           ;; => true if x is a Cons
(instance? x PersistentVector) ;; => true if x is a PersistentVector
(instance? x HashMap)        ;; => true if x is a HashMap
```

### Compilation

The compiler knows the type ID for each struct. `(instance? x Cons)` compiles to:
```lisp
;; Pseudocode lIR
(icmp eq (load (getelementptr x 0)) (i64 CONS_TYPE_ID))
```

Where `CONS_TYPE_ID` is the compile-time constant for Cons's type ID.

### Edge Cases

- `(instance? nil Cons)` => false (nil has type ID 0, or check for nil first)
- `(instance? x UnknownType)` => compile error if type doesn't exist

## Implementation

1. Add `instance?` to the parser as a builtin form
2. In codegen, emit:
   - Load field 0 (type ID) from the pointer
   - Compare against the known type ID for the struct
   - Return i1 result (0 or 1)

### Parser Changes (liar/src/parser.rs)

Add `Instance` variant to expression enum, parse `(instance? expr Type)`.

### Codegen Changes (liar/src/codegen/)

```rust
Expr::Instance { value, type_name } => {
    let type_id = ctx.get_struct_type_id(&type_name)
        .ok_or_else(|| CompileError::unknown_type(&type_name))?;

    // Load type_id from field 0
    let gep = lir::Expr::GetElementPtr { ... field 0 ... };
    let loaded = lir::Expr::Load { ptr: gep };

    // Compare against known type_id
    lir::Expr::Icmp {
        pred: "eq",
        lhs: loaded,
        rhs: lir::Expr::Literal(type_id)
    }
}
```

## Sugar Macros

Once `instance?` works, add convenience predicates in stdlib:

```lisp
;; In lib/liar.seq.liar
(defmacro cons? (x) `(instance? ,x Cons))
(defmacro list? (x) `(instance? ,x Cons))  ;; alias

;; In lib/liar.vector.liar
(defmacro vector? (x) `(instance? ,x PersistentVector))

;; In lib/liar.hashmap.liar
(defmacro map? (x) `(instance? ,x HashMap))

;; In lib/liar.hashset.liar
(defmacro set? (x) `(instance? ,x HashSet))
```

## Test Cases

```lisp
;; Basic type checking
(instance? (Cons 1 nil) Cons)           ;; => true
(instance? (Cons 1 nil) PersistentVector) ;; => false
(instance? nil Cons)                     ;; => false

;; With sugar
(cons? (cons 1 nil))                     ;; => true
(cons? (vector))                         ;; => false
(vector? (vector1 1))                    ;; => true

;; In conditionals
(if (cons? x)
    (first x)
    (vec-first x))
```

## Ordering

Required by: f7k2m (stdlib data transformation - flatten, mapcat need cons?)

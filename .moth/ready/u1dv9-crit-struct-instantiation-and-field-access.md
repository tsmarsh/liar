# Struct Instantiation and Field Access

**Priority:** P0 (Critical - blocks async I/O)
**Category:** liar/codegen
**Dependencies:** none

## Summary

Implement struct instantiation (`(Point 10 20)`) and field access (`(. p x)`) in liar codegen. Currently these are stubs that return placeholder strings.

## Current State

In `liar/src/codegen.rs`:
- `Expr::Struct` returns `StringLit("struct:Name")` placeholder
- `Expr::Field` returns `StringLit("field:name")` placeholder

## Requirements

### Struct Instantiation
- `(Point 10 20)` should allocate and return a struct pointer
- Field values must match the struct definition order
- Named field syntax: `(Point x: 10 y: 20)`

### Field Access
- `(. p x)` should do GEP (getelementptr) + load
- Must resolve field name to index from struct definition
- Returns the field value

## Implementation

**Location:** `liar/src/codegen.rs`

### For Struct Instantiation:
1. Look up struct definition by name
2. Allocate space for struct (alloca or heap)
3. Store each field value at correct offset via GEP
4. Return pointer to struct

### For Field Access:
1. Look up struct type from expression
2. Resolve field name to field index
3. Generate GEP instruction to get field pointer
4. Load and return value

### lIR Generation:
```lisp
;; (Point 10 20) becomes:
(let ((p (alloca Point)))
  (store (i64 10) (getelementptr Point p 0))
  (store (i64 20) (getelementptr Point p 1))
  p)

;; (. p x) becomes:
(load i64 (getelementptr Point p 0))
```

## Tests

```lisp
;; Basic instantiation and access
(defstruct Point (x: i64) (y: i64))
(let ((p (Point 10 20)))
  (. p x))  ;; => 10

;; Named field syntax
(let ((p (Point y: 20 x: 10)))
  (. p y))  ;; => 20

;; Nested access
(defstruct Line (start: Point) (end: Point))
(let ((line (Line (Point 0 0) (Point 10 10))))
  (. (. line end) x))  ;; => 10
```

## Acceptance Criteria

- [ ] `(let ((p (Point 1 2))) (. p x))` returns `1`
- [ ] `(let ((p (Point 1 2))) (. p y))` returns `2`
- [ ] Named field syntax works
- [ ] Nested struct access works
- [ ] Tests added to liar-cert

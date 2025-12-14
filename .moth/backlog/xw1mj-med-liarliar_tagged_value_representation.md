# liarliar: Tagged Value Representation

Create `lib/liarliar/value.liar` - the foundation for homoiconic data representation.

## Overview

Use tagged i64 values that fit in Cons.head. Low 3 bits encode type, remaining bits hold value or pointer.

## Tag Scheme

| Tag | Type | Payload |
|-----|------|---------|
| 0 | Integer | Value in high 61 bits |
| 1 | Symbol | Pointer to interned string |
| 2 | String | Pointer to heap string |
| 3 | Keyword | Pointer to string (with :) |
| 4 | Cons | Pointer to Cons struct |
| 5 | Nil | Sentinel value |
| 6 | Vector | Pointer to PersistentVector |
| 7 | Map | Pointer to HashMap |

## Functions to Implement

```clojure
;; Tag extraction
(defun tag-of (v) (bit-and v 7))
(defun ptr-of (v) (bit-and v -8))  ;; Clear low 3 bits

;; Constructors
(defun make-int (n) (bit-or (shl n 3) 0))
(defun make-ptr (p tag) (bit-or p tag))

;; Value extraction
(defun int-value (v) (ashr v 3))

;; Type predicates
(defun int? (v) (= (tag-of v) 0))
(defun symbol? (v) (= (tag-of v) 1))
(defun string? (v) (= (tag-of v) 2))
(defun keyword? (v) (= (tag-of v) 3))
(defun cons? (v) (= (tag-of v) 4))
(defun nil? (v) (= (tag-of v) 5))
(defun vector? (v) (= (tag-of v) 6))
(defun map? (v) (= (tag-of v) 7))

;; Cons operations with tagged values
(defun car (v) (. (ptr-of v) head))
(defun cdr (v) (. (ptr-of v) tail))
(defun make-cons (head tail) ...)
```

## Dependencies

- `liar.seq` - Uses existing Cons struct
- Builtins: `bit-and`, `bit-or`, `shl`, `ashr`

## Test Cases

- Create and extract integers
- Create tagged cons cells
- Type predicates work correctly
- car/cdr operate on tagged values

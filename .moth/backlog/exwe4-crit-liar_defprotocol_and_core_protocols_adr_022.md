## Summary
Implement defprotocol for defining abstractions and the core protocols
that collections and types implement.

## defprotocol Syntax
```lisp
(defprotocol Name
  "Optional docstring"
  (method-name [self arg1 arg2] "method doc"))
```

## extend-protocol Syntax  
```lisp
(extend-protocol Seq
  PersistentVector
  (first [self] (nth self 0))
  (rest [self] (subvec self 1)))
```

## Core Protocols (ADR-022)

### Sequence Access
```lisp
(defprotocol Seqable
  (seq [self] "Returns a Seq, or nil if empty."))

(defprotocol Seq
  (first [self] "Returns the first element.")
  (rest [self] "Returns a Seq of remaining elements."))

(defprotocol Counted
  (count [self] "Returns the number of elements."))

(defprotocol Indexed
  (nth [self n] "Returns element at index n."))
```

### Collection Operations
```lisp
(defprotocol Collection
  (conj [self x] "Returns collection with x added.")
  (empty [self] "Returns an empty collection of the same type."))

(defprotocol Associative
  (get [self k] "Returns value for key, or nil.")
  (assoc [self k v] "Returns collection with k mapped to v.")
  (dissoc [self k] "Returns collection without key k.")
  (has-key? [self k] "Returns true if key exists."))

(defprotocol Set
  (contains? [self x] "Returns true if x is a member.")
  (disj [self x] "Returns set without x."))
```

### Reduction
```lisp
(defprotocol Reducible
  (reduce [self f init] "Reduces collection with f."))

(defprotocol KVReducible
  (reduce-kv [self f init] "Reduces with (f acc key val)."))
```

### Iteration
```lisp
(defprotocol Iter
  (next [self] "Returns {:some [value next-iter]} or :none."))
```

### Reference Types
```lisp
(defprotocol Deref
  (deref [self] "Returns the current value."))
```

### Comparison & Hashing
```lisp
(defprotocol Eq
  (eq [self other] "Returns true if equal."))

(defprotocol Ord
  (cmp [self other] "Returns :lt, :eq, or :gt."))

(defprotocol Hash
  (hash [self] "Returns hash code as i64."))
```

### Display
```lisp
(defprotocol Show
  (str [self] "Returns string representation."))
```

## AST Additions
```rust
pub enum Item {
    // ... existing ...
    Defprotocol(Defprotocol),
    ExtendProtocol(ExtendProtocol),
}

pub struct Defprotocol {
    pub name: Spanned<String>,
    pub doc: Option<String>,
    pub methods: Vec<ProtocolMethod>,
}

pub struct ProtocolMethod {
    pub name: Spanned<String>,
    pub params: Vec<Spanned<String>>,  // includes self
    pub doc: Option<String>,
}
```

## contains? Fixed
Unlike Clojure, `contains?` checks membership:
```lisp
(contains? [1 2 3] 2)   ; => true (value 2 is present)
(contains? [1 2 3] 99)  ; => false

;; has-key? for index/key existence
(has-key? [1 2 3] 0)    ; => true (index exists)
(has-key? {:a 1} :a)    ; => true (key exists)
```

## Acceptance Criteria
- [ ] defprotocol parses and registers protocol
- [ ] extend-protocol parses and registers impl
- [ ] Protocol methods callable on implementing types
- [ ] All 12 core protocols defined
- [ ] Built-in types implement appropriate protocols
- [ ] contains? works on membership
- [ ] has-key? works on index/key existence

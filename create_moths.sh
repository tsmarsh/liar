# Remove native closures - we decided against it
moth rm z8vek

# Add the 5 missing moths with full descriptions:

moth new "liar: defprotocol and core protocols (ADR-022)" -s crit --no-edit --stdin << 'EOF'
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
EOF

moth new "liar: iterator protocol (Iter)" -s high --no-edit --stdin << 'EOF'
## Summary
Implement Rust-style iterators instead of pervasive laziness.
Iterators are explicit, have clear ownership, and avoid space leaks.

## Why Not Laziness?
- Unpredictable performance (when does work happen?)
- Space leaks (hold head, keep whole seq in memory)
- Ownership complexity (who owns the thunk?)
- Rust rejected it for good reasons

## Iterator Protocol
```lisp
(defprotocol Iter
  (next [self] "Returns {:some [value next-iter]} or :none."))
```

## Usage
```lisp
; Create iterator from collection
(-> [1 2 3 4 5]
    (iter)                    ; Create iterator
    (iter/map square)         ; Lazy transform
    (iter/filter even?)       ; Lazy filter  
    (iter/take 3)             ; Lazy take
    (collect))                ; Materialize HERE

; Infinite sequences via iterator
(-> (iter/range)              ; 0, 1, 2, ...
    (iter/map square)
    (iter/take 10)
    (collect))                ; [0 1 4 9 16 25 36 49 64 81]
```

## Core Iterator Functions
```lisp
;; Creation
(iter coll)                   ; Collection -> Iter
(iter/range)                  ; Infinite: 0, 1, 2, ...
(iter/range n)                ; 0 to n-1
(iter/range start end)        ; start to end-1
(iter/repeat x)               ; Infinite: x, x, x, ...
(iter/iterate f x)            ; x, (f x), (f (f x)), ...

;; Transforms (lazy, return new iterator)
(iter/map f it)
(iter/filter pred it)
(iter/take n it)
(iter/drop n it)
(iter/take-while pred it)
(iter/drop-while pred it)
(iter/enumerate it)           ; (0, a), (1, b), ...
(iter/zip it1 it2)
(iter/chain it1 it2)
(iter/flatten it)             ; Iter<Iter<T>> -> Iter<T>
(iter/flat-map f it)          ; map then flatten

;; Consumers (force evaluation)
(collect it)                  ; -> vector
(iter/reduce f init it)
(iter/for-each f it)          ; side effects
(iter/count it)
(iter/sum it)
(iter/any? pred it)
(iter/all? pred it)
(iter/find pred it)
(iter/nth it n)
(iter/last it)
```

## Ownership Semantics
```lisp
; Iterator is consumed on use
(let ((it (iter [1 2 3])))
  (let ((it2 (iter/map square it)))  ; it moved into it2
    (collect it2)))                   ; it2 consumed

; Can't use iterator twice
(let ((it (iter [1 2 3])))
  (collect it)
  (collect it))  ; ERROR: use after move
```

## Comparison: Lazy vs Iterator
```lisp
;; Lazy (NOT doing this)
(take 5 (map square (naturals)))  ; When computed? Who knows.

;; Iterator (doing this)
(-> (iter/range)
    (iter/map square)
    (iter/take 5)
    (collect))  ; Computed HERE. Clear.
```

## Integration with Seq
Iterators are NOT seqs. They're consumed on use.
Seq functions work on collections directly (eager).
Iterator functions are in `iter/` namespace (lazy).
```lisp
; Seq (eager, on collections)
(map f [1 2 3])        ; Returns new vector immediately

; Iter (lazy, explicit)
(-> [1 2 3]
    (iter)
    (iter/map f)
    (collect))         ; Returns vector when collected
```

## Acceptance Criteria
- [ ] Iter protocol defined
- [ ] iter creates iterator from collection
- [ ] iter/map, filter, take, drop work
- [ ] collect materializes to vector
- [ ] iter/range for infinite sequences
- [ ] Proper ownership (consumed on use)
- [ ] No space leaks
EOF

moth new "lIR: atomic-load/store" -s high --no-edit --stdin << 'EOF'
## Summary
Add atomic load and store instructions with memory ordering to lIR.
These are LLVM primitives needed for implementing atoms.

## Syntax
```lisp
(atomic-load ordering type ptr)       ; Atomic load
(atomic-store ordering value ptr)     ; Atomic store
```

## Memory Orderings
```lisp
monotonic   ; Minimal ordering, no synchronization
acquire     ; Acquire semantics (loads)
release     ; Release semantics (stores)
acq_rel     ; Acquire-release (read-modify-write)
seq_cst     ; Sequential consistency (strongest)
```

## AST Additions
```rust
pub enum MemoryOrdering {
    Monotonic,
    Acquire,
    Release,
    AcqRel,
    SeqCst,
}

pub enum Expr {
    // ... existing ...
    AtomicLoad {
        ordering: MemoryOrdering,
        ty: ScalarType,
        ptr: Box<Expr>,
    },
    AtomicStore {
        ordering: MemoryOrdering,
        value: Box<Expr>,
        ptr: Box<Expr>,
    },
}
```

## Parser
```rust
// (atomic-load seq_cst i64 ptr)
"atomic-load" => {
    let ordering = self.parse_ordering()?;
    let ty = self.parse_scalar_type()?;
    let ptr = self.parse_expr()?;
    Ok(Expr::AtomicLoad { ordering, ty, ptr: Box::new(ptr) })
}

fn parse_ordering(&mut self) -> Result<MemoryOrdering> {
    match self.expect_symbol()?.as_str() {
        "monotonic" => Ok(MemoryOrdering::Monotonic),
        "acquire" => Ok(MemoryOrdering::Acquire),
        "release" => Ok(MemoryOrdering::Release),
        "acq_rel" => Ok(MemoryOrdering::AcqRel),
        "seq_cst" => Ok(MemoryOrdering::SeqCst),
        other => Err(ParseError::InvalidOrdering(other.to_string())),
    }
}
```

## Codegen
```rust
Expr::AtomicLoad { ordering, ty, ptr } => {
    let ptr_val = self.compile_expr(ptr)?;
    let llvm_ordering = match ordering {
        MemoryOrdering::Monotonic => AtomicOrdering::Monotonic,
        MemoryOrdering::Acquire => AtomicOrdering::Acquire,
        MemoryOrdering::SeqCst => AtomicOrdering::SequentiallyConsistent,
        // ...
    };
    let load = self.builder.build_load(ty.to_llvm(), ptr_val, "atomic_load");
    load.set_atomic_ordering(llvm_ordering)?;
    Ok(load)
}
```

## Use in liar Atoms
```lisp
; @atom (deref) compiles to:
(atomic-load seq_cst i64 (rc-ptr atom))

; (reset! atom val) compiles to:
(atomic-store seq_cst val (rc-ptr atom))
```

## Test Cases
```gherkin
Scenario: Atomic load and store
  Given the expression (define (test i64) () (block entry (let ((p (alloca i64))) (atomic-store seq_cst (i64 42) p) (ret (atomic-load seq_cst i64 p)))))
  When I call test
  Then the result is (i64 42)
```

## Acceptance Criteria
- [ ] atomic-load parses with all orderings
- [ ] atomic-store parses with all orderings
- [ ] Codegen produces LLVM atomic instructions
- [ ] Invalid ordering = parse error
- [ ] Feature file passes
EOF

moth new "lIR: atomicrmw (read-modify-write)" -s high --no-edit --stdin << 'EOF'
## Summary
Add atomicrmw instruction for atomic read-modify-write operations.
Used for lock-free counters, accumulators, and RC refcounts.

## Syntax
```lisp
(atomicrmw op ordering ptr value)
```

## Operations
```lisp
xchg    ; Exchange (swap)
add     ; Add
sub     ; Subtract  
and     ; Bitwise AND
or      ; Bitwise OR
xor     ; Bitwise XOR
min     ; Signed minimum
max     ; Signed maximum
umin    ; Unsigned minimum
umax    ; Unsigned maximum
```

## Examples
```lisp
; Atomic increment (returns OLD value)
(atomicrmw add seq_cst counter (i64 1))

; Atomic swap
(atomicrmw xchg seq_cst ptr new_value)

; Atomic max
(atomicrmw max seq_cst ptr candidate)
```

## AST Addition
```rust
pub enum AtomicRMWOp {
    Xchg, Add, Sub, And, Or, Xor, Min, Max, UMin, UMax,
}

pub enum Expr {
    // ... existing ...
    AtomicRMW {
        op: AtomicRMWOp,
        ordering: MemoryOrdering,
        ptr: Box<Expr>,
        value: Box<Expr>,
    },
}
```

## Codegen
```rust
Expr::AtomicRMW { op, ordering, ptr, value } => {
    let ptr_val = self.compile_expr(ptr)?;
    let val = self.compile_expr(value)?;
    let llvm_op = match op {
        AtomicRMWOp::Xchg => AtomicRMWBinOp::Xchg,
        AtomicRMWOp::Add => AtomicRMWBinOp::Add,
        // ...
    };
    self.builder.build_atomicrmw(llvm_op, ptr_val, val, ordering.to_llvm())
}
```

## Use in RC (current impl can use this)
```lisp
; rc-clone becomes:
(atomicrmw add seq_cst refcount_ptr (i64 1))

; rc-drop becomes:
(let ((old (atomicrmw sub seq_cst refcount_ptr (i64 1))))
  (br (icmp eq old (i64 1)) free_block continue_block))
```

## Test Cases
```gherkin
Scenario: Atomic increment returns old value
  Given the expression (define (test i64) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (ret (atomicrmw add seq_cst p (i64 5))))))
  When I call test
  Then the result is (i64 10)

Scenario: Atomic increment updates memory
  Given the expression (define (test i64) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (atomicrmw add seq_cst p (i64 5)) (ret (load i64 p)))))
  When I call test
  Then the result is (i64 15)
```

## Acceptance Criteria
- [ ] atomicrmw parses with all ops and orderings
- [ ] Returns old value
- [ ] Codegen produces LLVM atomicrmw
- [ ] All 10 operations work
EOF

moth new "lIR: cmpxchg (compare-and-swap)" -s high --no-edit --stdin << 'EOF'
## Summary
Add cmpxchg (compare-and-exchange) instruction for lock-free algorithms.
This is the fundamental primitive for implementing swap! CAS loops.

## Syntax
```lisp
(cmpxchg ordering ptr expected new)
; Returns { old_value, success_flag }
```

## Semantics
```
if *ptr == expected:
    *ptr = new
    return { expected, true }
else:
    return { *ptr, false }
```

## AST Addition
```rust
pub enum Expr {
    // ... existing ...
    CmpXchg {
        ordering: MemoryOrdering,
        ptr: Box<Expr>,
        expected: Box<Expr>,
        new_value: Box<Expr>,
    },
}
```

## Return Type
CmpXchg returns a struct { T, i1 }:
- First element: the value that was in memory
- Second element: whether the exchange succeeded
```lisp
(let ((result (cmpxchg seq_cst ptr expected new)))
  (let ((old_val (extractvalue result 0))
        (success (extractvalue result 1)))
    (br success done retry)))
```

## Use for swap!
```lisp
; (swap! atom fn) compiles to CAS loop:
(block swap_loop
  (let ((old (atomic-load seq_cst i64 ptr)))
    (let ((new (call fn old)))
      (let ((result (cmpxchg seq_cst ptr old new)))
        (br (extractvalue result 1) swap_done swap_loop)))))
(block swap_done
  (ret new))
```

## Codegen
```rust
Expr::CmpXchg { ordering, ptr, expected, new_value } => {
    let ptr_val = self.compile_expr(ptr)?;
    let exp_val = self.compile_expr(expected)?;
    let new_val = self.compile_expr(new_value)?;
    
    self.builder.build_cmpxchg(
        ptr_val, exp_val, new_val,
        ordering.to_llvm(),  // success ordering
        ordering.to_llvm(),  // failure ordering
    )
}
```

## Test Cases
```gherkin
Scenario: CmpXchg success
  Given the expression (define (test i1) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (let ((result (cmpxchg seq_cst p (i64 10) (i64 20)))) (ret (extractvalue result 1))))))
  When I call test
  Then the result is (i1 1)

Scenario: CmpXchg failure
  Given the expression (define (test i1) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (let ((result (cmpxchg seq_cst p (i64 99) (i64 20)))) (ret (extractvalue result 1))))))
  When I call test
  Then the result is (i1 0)

Scenario: CmpXchg returns old value
  Given the expression (define (test i64) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (let ((result (cmpxchg seq_cst p (i64 99) (i64 20)))) (ret (extractvalue result 0))))))
  When I call test
  Then the result is (i64 10)
```

## Acceptance Criteria
- [ ] cmpxchg parses
- [ ] Returns { old_value, success }
- [ ] extractvalue works on result
- [ ] Success case: memory updated
- [ ] Failure case: memory unchanged
- [ ] Can implement CAS loop for swap!
EOF
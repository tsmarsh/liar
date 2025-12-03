#!/bin/bash

# =============================================================================
# Moths for missing liar language features
# =============================================================================

# -----------------------------------------------------------------------------
# THREADING FEATURES
# -----------------------------------------------------------------------------

moth new "liar: closure color tracking (ADR-010)" -s high --no-edit --stdin << 'EOF'
## Summary
Track closure "color" to determine thread-safety. Colors propagate through
nesting and affect where closures can be used.

## Colors
```
Pure     - No captures, can go anywhere
Local    - Captures borrows, cannot escape scope
Sync     - Captures only Send+Sync values, thread-safe
NonSync  - Captures non-Send values, single-threaded only
```

## Color Rules
- Pure + anything = that thing
- Local + anything = Local
- Sync + Sync = Sync
- NonSync + anything = NonSync

## AST Additions
```rust
// In closures.rs
pub enum ClosureColor {
    Pure,
    Local,
    Sync,
    NonSync,
}

pub struct ClosureInfo {
    pub captures: Vec<Capture>,
    pub color: ClosureColor,
}
```

## Analysis
```rust
impl ClosureAnalyzer {
    fn compute_color(&self, captures: &[Capture]) -> ClosureColor {
        if captures.is_empty() {
            return ClosureColor::Pure;
        }
        
        let mut color = ClosureColor::Pure;
        for cap in captures {
            let cap_color = match cap.mode {
                CaptureMode::Borrow => ClosureColor::Local,
                CaptureMode::Move => {
                    if self.is_send_sync(&cap.ty) {
                        ClosureColor::Sync
                    } else {
                        ClosureColor::NonSync
                    }
                }
                CaptureMode::Clone => ClosureColor::Sync, // Cloned = owned copy
            };
            color = self.combine_colors(color, cap_color);
        }
        color
    }
}
```

## Verification
- plet body must be Sync or Pure
- async block must be Sync or Pure
- spawn requires Sync or Pure
- Local closures cannot escape defining scope

## Acceptance Criteria
- [ ] ClosureColor enum added
- [ ] Color computed for all closures
- [ ] Colors propagate through nesting
- [ ] plet rejects non-Sync closures
- [ ] Error messages indicate color violations
EOF

# -----------------------------------------------------------------------------

moth new "liar: atoms for shared state (ADR-011)" -s high --no-edit --stdin << 'EOF'
## Summary
Implement atoms for thread-safe mutable state following Clojure's model.

## Syntax
```lisp
(atom initial-value)     ; Create atomic cell
(swap! atom fn)          ; Atomic update: atom = fn(current)
(reset! atom value)      ; Atomic set
@atom                    ; Atomic read (deref)
(compare-and-set! atom old new)  ; CAS
```

## AST Additions
```rust
pub enum Expr {
    // ... existing ...
    Atom(Box<Spanned<Expr>>),           // (atom x)
    Swap(Box<Spanned<Expr>>, Box<Spanned<Expr>>),  // (swap! atom fn)
    Reset(Box<Spanned<Expr>>, Box<Spanned<Expr>>), // (reset! atom val)
    AtomDeref(Box<Spanned<Expr>>),      // @atom
    CompareAndSet {                      // (compare-and-set! atom old new)
        atom: Box<Spanned<Expr>>,
        old: Box<Spanned<Expr>>,
        new: Box<Spanned<Expr>>,
    },
}
```

## Lexer Additions
```rust
'@' => TokenKind::At,        // For @atom deref
"swap!" => TokenKind::Swap,
"reset!" => TokenKind::Reset,
"compare-and-set!" => TokenKind::CAS,
```

## Type Checking
```rust
// atom creates Atom<T>
// swap! takes Atom<T> and (T -> T), returns T
// reset! takes Atom<T> and T, returns T
// @atom returns T
// compare-and-set! takes Atom<T>, T, T, returns bool
```

## Codegen to lIR
```lisp
; (atom 0) compiles to:
(rc-alloc i64)  ; Atoms are ref-counted for sharing

; (swap! a inc) compiles to:
; CAS loop:
(block swap_loop
  (let ((current (load i64 (rc-ptr a))))
    (let ((new (call @inc current)))
      (br (cmpxchg a current new) swap_done swap_loop))))

; @atom compiles to:
(load i64 (rc-ptr a))
```

## Test Cases
```lisp
; Basic atom
(let ((a (atom 0)))
  (swap! a inc)
  @a)  ; => 1

; In plet
(let ((counter (atom 0)))
  (plet ((a (swap! counter inc))
         (b (swap! counter inc)))
    @counter))  ; => 2 (both swaps complete)
```

## Acceptance Criteria
- [ ] atom creates atomic cell
- [ ] swap! performs atomic update
- [ ] reset! performs atomic set
- [ ] @atom reads atomically
- [ ] compare-and-set! works
- [ ] Thread-safe in plet
EOF

# -----------------------------------------------------------------------------

moth new "liar: plet verification (ADR-009)" -s high --no-edit --stdin << 'EOF'
## Summary
plet is a compile-time construct that marks closures as thread-safe.
It executes sequentially like let, but enforces that only atoms are
used for mutable state. This enables passing plet-closures to pmap/pfilter.

## Semantics
```lisp
; plet marks closure as Sync (thread-safe)
; Mutable state must use atoms
; Non-atom bindings are treated as constants

(plet ((cache (atom []))    ; mutable — atom required
       (multiplier 10))     ; immutable — constant
  (fn (x) 
    (swap! cache ...)       ; OK: atom
    (set! multiplier 20)))  ; ERROR: not an atom
```

## Verification Rules

### In plet body:
1. `set!` on non-atom binding = ERROR
2. Mutation through non-atom reference = ERROR
3. Capturing non-atom mutable state = ERROR

### Closure color:
- let-closure with mutable captures → NonSync
- plet-closure → Sync (verified safe)

## Implementation
```rust
// In ownership.rs or new plet_check.rs

pub struct PletChecker {
    atom_bindings: HashSet<String>,
    errors: Vec<PletError>,
}

impl PletChecker {
    pub fn check_plet(&mut self, bindings: &[LetBinding], body: &Expr) {
        // Identify which bindings are atoms
        for binding in bindings {
            if self.is_atom_expr(&binding.value) {
                self.atom_bindings.insert(binding.name.clone());
            }
        }
        
        // Check body for non-atom mutations
        self.check_mutations(body);
    }
    
    fn check_mutations(&mut self, expr: &Expr) {
        match expr {
            Expr::Set(name, _) => {
                if !self.atom_bindings.contains(&name.value) {
                    self.errors.push(PletError::NonAtomMutation(name.clone()));
                }
            }
            // ... recurse into sub-expressions
        }
    }
}
```

## Error Messages
```
error: cannot mutate non-atom binding in plet
  --> src/main.liar:10:5
   |
 8 | (plet ((counter 0))
   |        ------- binding is not an atom
 9 |   (fn ()
10 |     (set! counter (+ counter 1))))
   |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ mutation requires atom
   |
   = help: use (atom 0) instead of 0
```

## Codegen
plet compiles identically to let — the safety is compile-time only:
```lisp
; (plet ((x (atom 0))) body)
; compiles to same as:
; (let ((x (atom 0))) body)
```

## Integration with pmap
```lisp
; let-closure: NonSync color
(let ((cache []))
  (fn (x) (append cache x)))

(pmap let-closure data)  ; ERROR: NonSync closure to parallel op

; plet-closure: Sync color
(plet ((cache (atom [])))
  (fn (x) (swap! cache (fn (c) (append c x)))))

(pmap plet-closure data)  ; OK: Sync closure
```

## Acceptance Criteria
- [ ] plet parses (already done)
- [ ] Non-atom mutation in plet body = error
- [ ] plet closures marked as Sync color
- [ ] let closures with mutable captures marked NonSync
- [ ] pmap rejects NonSync closures
- [ ] Good error messages with suggestions
EOF

# -----------------------------------------------------------------------------

moth new "liar: pmap/pfilter parallel execution" -s med --no-edit --stdin << 'EOF'
## Summary
Implement parallel map and filter operations that execute across
multiple threads. These require Sync closures (from plet).

## Syntax
```lisp
(pmap fn collection)       ; Parallel map
(pfilter pred collection)  ; Parallel filter
(preduce fn init coll)     ; Parallel reduce
```

## Semantics
```lisp
; pmap applies fn in parallel across collection
(pmap (fn (x) (* x x)) [1 2 3 4])  ; => [1 4 9 16]

; Only Sync closures allowed
(plet ((factor 2))
  (pmap (fn (x) (* x factor)) data))  ; OK: plet closure is Sync

(let ((factor 2))
  (pmap (fn (x) (* x factor)) data))  ; ERROR: let closure is NonSync
```

## Type Checking
```rust
fn check_pmap_call(&mut self, fn_expr: &Expr, coll_expr: &Expr) {
    let fn_ty = self.infer(fn_expr)?;
    let color = self.get_closure_color(fn_expr);
    
    if color != ClosureColor::Sync && color != ClosureColor::Pure {
        self.error(TypeError::NonSyncToPmap {
            found: color,
            span: fn_expr.span,
        });
    }
}
```

## Runtime Implementation
Options:
1. **Rayon** - Work-stealing thread pool (Rust crate)
2. **pthreads** - Direct FFI to POSIX threads
3. **Custom** - Simple thread pool

## Codegen to lIR
```lisp
; (pmap f coll) conceptually becomes:
(declare pmap_impl ptr (ptr ptr))  ; runtime function

; Or inline:
(let ((results (array-alloc T (len coll)))
      (threads (array-alloc ptr (len coll))))
  ; spawn thread per element (or chunk)
  ; join all
  ; return results)
```

## Chunking Strategy
For large collections, divide into chunks:
```lisp
; Don't spawn 1M threads for 1M elements
; Chunk into (num-cores) pieces
```

## Acceptance Criteria
- [ ] pmap executes in parallel
- [ ] pfilter executes in parallel
- [ ] Only Sync/Pure closures accepted
- [ ] NonSync closure = compile error
- [ ] Proper thread pool management
- [ ] Results maintain order
EOF

# -----------------------------------------------------------------------------

moth new "liar: dosync transactions (ADR-012)" -s med --no-edit --stdin << 'EOF'
## Summary
Implement Software Transactional Memory (STM) for coordinated updates
to multiple refs.

## Syntax
```lisp
(ref initial-value)      ; Create transactional ref
(dosync body...)         ; Transaction block
(ref-set ref value)      ; Set in transaction
(alter ref fn args...)   ; Update in transaction
(commute ref fn args...) ; Commutative update
```

## Semantics
- Transactions are atomic, consistent, isolated
- Conflicts cause automatic retry
- No locks, no deadlocks

## Example
```lisp
(let ((account-a (ref 100))
      (account-b (ref 200)))
  (dosync
    (let ((amount 50))
      (alter account-a - amount)
      (alter account-b + amount))))
```

## AST Additions
```rust
pub enum Expr {
    // ... existing ...
    MakeRef(Box<Spanned<Expr>>),
    Dosync(Vec<Spanned<Expr>>),
    RefSet(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    Alter {
        ref_expr: Box<Spanned<Expr>>,
        fn_expr: Box<Spanned<Expr>>,
        args: Vec<Spanned<Expr>>,
    },
    Commute {
        ref_expr: Box<Spanned<Expr>>,
        fn_expr: Box<Spanned<Expr>>,
        args: Vec<Spanned<Expr>>,
    },
}
```

## Implementation Notes
- Use MVCC (Multi-Version Concurrency Control)
- Track read/write sets per transaction
- Validate at commit time
- Retry on conflict

## Acceptance Criteria
- [ ] ref creates transactional reference
- [ ] dosync provides transaction scope
- [ ] alter updates in transaction
- [ ] Automatic retry on conflict
- [ ] No deadlocks possible
EOF

# -----------------------------------------------------------------------------

moth new "liar: async/await (ADR-014)" -s med --no-edit --stdin << 'EOF'
## Summary
Implement async/await for non-blocking I/O operations.
Async blocks must use plet semantics (thread-safe).

## Syntax
```lisp
(async body)             ; Create future
(await future)           ; Block until complete
(.await future)          ; Alternative syntax
```

## Semantics
```lisp
; Async returns a Future<T>
(let ((f (async (fetch-url "https://..."))))
  (await f))  ; Blocks, returns result

; Parallel async
(plet ((a (async (fetch "url1")))
       (b (async (fetch "url2"))))
  (list (await a) (await b)))
```

## AST Additions
```rust
pub enum Expr {
    // ... existing ...
    Async(Box<Spanned<Expr>>),
    Await(Box<Spanned<Expr>>),
}
```

## Type System
```rust
// async creates Future<T> where T is body return type
// await takes Future<T>, returns T
```

## Closure Color Integration
- async body must be Sync (no Local closures)
- Captured state must be Send + Sync

## Codegen
Likely targets tokio or async-std runtime.
May need runtime initialization.

## Acceptance Criteria
- [ ] async creates future
- [ ] await blocks until complete
- [ ] Works with plet for parallel await
- [ ] Closure color enforced (Sync required)
- [ ] Integration with I/O runtime
EOF

# -----------------------------------------------------------------------------
# COLLECTIONS
# -----------------------------------------------------------------------------

moth new "liar: persistent collections (ADR-018)" -s high --no-edit --stdin << 'EOF'
## Summary
Implement persistent (immutable, structural sharing) collections:
vector, map, list.

## Syntax
```lisp
[1 2 3]           ; Persistent vector
{:a 1 :b 2}       ; Persistent map
'(1 2 3)          ; List (linked)
```

## Operations
```lisp
; Vector
(conj [1 2] 3)         ; => [1 2 3]
(get [1 2 3] 0)        ; => 1
(assoc [1 2 3] 1 99)   ; => [1 99 3]
(count [1 2 3])        ; => 3
(subvec [1 2 3 4] 1 3) ; => [2 3]

; Map
(assoc {:a 1} :b 2)    ; => {:a 1 :b 2}
(dissoc {:a 1 :b 2} :a); => {:b 2}
(get {:a 1} :a)        ; => 1
(keys {:a 1 :b 2})     ; => (:a :b)
(vals {:a 1 :b 2})     ; => (1 2)

; List
(cons 0 '(1 2))        ; => (0 1 2)
(first '(1 2 3))       ; => 1
(rest '(1 2 3))        ; => (2 3)
```

## AST Additions
```rust
pub enum Expr {
    // ... existing ...
    Vector(Vec<Spanned<Expr>>),
    Map(Vec<(Spanned<Expr>, Spanned<Expr>)>),
    List(Vec<Spanned<Expr>>),
    Keyword(String),  // :foo
}
```

## Lexer Additions
```rust
'[' => TokenKind::LBracket,
']' => TokenKind::RBracket,
'{' => TokenKind::LBrace,
'}' => TokenKind::RBrace,
':' followed by symbol => TokenKind::Keyword(s)
```

## Implementation Options
1. Use im-rs crate (Rust persistent collections)
2. Build custom HAMTs/RRB-trees
3. Start simple (copy-on-write), optimize later

## Type System
```rust
Ty::Vector(Box<Ty>),      // [T]
Ty::Map(Box<Ty>, Box<Ty>), // {K V}
Ty::List(Box<Ty>),         // '(T)
Ty::Keyword,               // :keyword
```

## Acceptance Criteria
- [ ] Vector literal parses and evaluates
- [ ] Map literal parses and evaluates  
- [ ] List literal parses and evaluates
- [ ] Keywords parse (:foo)
- [ ] conj, get, assoc work
- [ ] Structural sharing (verify no deep copies)
EOF

# -----------------------------------------------------------------------------

moth new "liar: conventional (mutable) collections (ADR-018)" -s med --no-edit --stdin << 'EOF'
## Summary
Implement conventional (mutable, O(1) access) collections alongside
persistent ones.

## Syntax
```lisp
<[1 2 3]>         ; Conventional vector
<{:a 1 :b 2}>     ; Conventional map
```

## Operations
```lisp
; Mutable operations (with !)
(push! vec item)       ; Append in place
(set! vec idx val)     ; Update in place
(put! map key val)     ; Insert in place
(remove! map key)      ; Remove in place

; Same read operations as persistent
(get vec idx)
(count vec)
```

## AST Additions
```rust
pub enum Expr {
    // ... existing ...
    ConvVector(Vec<Spanned<Expr>>),
    ConvMap(Vec<(Spanned<Expr>, Spanned<Expr>)>),
}
```

## Lexer
```rust
// <[ starts conventional vector
// <{ starts conventional map
// Need lookahead for < followed by [ or {
```

## Ownership
- Conventional collections are owned
- Mutations require &mut
- Cannot be shared without atoms/rc

## Type System
```rust
Ty::ConvVector(Box<Ty>),    // <[T]>
Ty::ConvMap(Box<Ty>, Box<Ty>), // <{K V}>
```

## Implementation
Use Rust's Vec and HashMap under the hood.

## Acceptance Criteria
- [ ] <[...]> syntax parses
- [ ] <{...}> syntax parses
- [ ] Mutable operations work
- [ ] Ownership rules apply
- [ ] Performance: O(1) access
EOF

# -----------------------------------------------------------------------------
# SIMD
# -----------------------------------------------------------------------------

moth new "liar: SIMD vectors (ADR-016)" -s med --no-edit --stdin << 'EOF'
## Summary
Implement SIMD vector literals and operations for data parallelism.

## Syntax
```lisp
<<1 2 3 4>>              ; v4 i64 (inferred)
<<1.0 2.0 3.0 4.0>>      ; v4 f64 (inferred)
<i8<1 2 3 4>>            ; v4 i8 (explicit type)
<f32<1.0 2.0 3.0 4.0>>   ; v4 f32 (explicit type)
```

## Operations
```lisp
; Arithmetic broadcasts to all lanes
(+ <<1 2 3 4>> <<5 6 7 8>>)   ; => <<6 8 10 12>>
(* 2 <<1 2 3 4>>)             ; => <<2 4 6 8>>

; Lane access
(lane vec idx)                 ; Extract single lane
(with-lane vec idx val)        ; Return vec with lane updated

; Horizontal operations
(hsum <<1 2 3 4>>)            ; => 10 (horizontal sum)
(hmin <<1 2 3 4>>)            ; => 1
(hmax <<1 2 3 4>>)            ; => 4
```

## AST Additions
```rust
pub enum Expr {
    // ... existing ...
    SimdVector(Vec<Spanned<Expr>>),           // <<...>>
    SimdVectorTyped(String, Vec<Spanned<Expr>>), // <i8<...>>
}
```

## Lexer
```rust
"<<" => TokenKind::DoubleLAngle,
">>" => TokenKind::DoubleRAngle,
// <i8< needs careful parsing
```

## Type System
```rust
Ty::SimdVector(Box<Ty>, usize),  // <N x T>
```

## Codegen to lIR
```lisp
; <<1 2 3 4>> compiles to:
(vector <4 x i64> (i64 1) (i64 2) (i64 3) (i64 4))

; (+ a b) on vectors compiles to:
(add a b)  ; lIR add works on vectors
```

## Constraints
- Vector width must be power of 2
- All elements same type
- Operations maintain lane correspondence

## Acceptance Criteria
- [ ] <<...>> syntax parses
- [ ] <type<...>> syntax parses
- [ ] Arithmetic works on vectors
- [ ] Scalar broadcast works
- [ ] Horizontal operations work
- [ ] Maps to lIR vector types
EOF

# -----------------------------------------------------------------------------
# NUMERIC
# -----------------------------------------------------------------------------

moth new "liar: numeric overflow handling (ADR-017)" -s low --no-edit --stdin << 'EOF'
## Summary
Implement explicit overflow handling modes: boxed (auto-promote),
wrapping (C-style), and checked (default).

## Syntax
```lisp
; Default: checked (panic on overflow)
(+ x y)

; Boxed: promotes to biginteger, never overflows
(boxed (* BIG BIGGER))

; Wrapping: C-style silent wrap
(wrapping (* x y))
```

## AST Additions
```rust
pub enum Expr {
    // ... existing ...
    Boxed(Box<Spanned<Expr>>),
    Wrapping(Box<Spanned<Expr>>),
}
```

## Codegen

### Checked (default)
```lisp
; Uses LLVM's overflow intrinsics
(let ((result (call.overflow @llvm.sadd.with.overflow.i64 a b)))
  (br (extractvalue result 1) overflow_handler continue))
```

### Wrapping
```lisp
; Just regular add, wraps naturally
(add a b)
```

### Boxed
```lisp
; Check for potential overflow, promote if needed
; Requires bigint library integration
```

## Implementation Notes
- Boxed requires bigint library (num-bigint, rug, or custom)
- Checked needs runtime panic handler
- Wrapping is simplest (current behavior)

## Acceptance Criteria
- [ ] boxed syntax parses
- [ ] wrapping syntax parses  
- [ ] Checked arithmetic panics on overflow
- [ ] Wrapping arithmetic wraps silently
- [ ] Boxed promotes to bigint
EOF

# -----------------------------------------------------------------------------
# OTHER
# -----------------------------------------------------------------------------

moth new "liar: share and clone (ADR ownership)" -s high --no-edit --stdin << 'EOF'
## Summary
Implement share (reference counting) and clone (deep copy) operations.

## Syntax
```lisp
(share x)    ; Create reference-counted value
(clone x)    ; Deep copy, returns owned value
```

## Semantics

### share
```lisp
(let ((x (share (cons 1 2))))   ; refcount=1
  (let ((y x))                   ; refcount=2 (implicit clone of rc)
    (car y))                     ; access through y
  (car x))                       ; still valid, refcount=1
                                 ; refcount=0 here, freed
```

### clone
```lisp
(let ((x (cons 1 2)))
  (let ((y (clone x)))  ; y owns a deep copy
    (set-car! y 99)     ; mutate copy
    (cons (car x)       ; x unchanged => 1
          (car y))))    ; y changed => 99
```

## AST Additions
```rust
pub enum Expr {
    // ... existing ...
    Share(Box<Spanned<Expr>>),
    Clone(Box<Spanned<Expr>>),
}
```

## Type System
```rust
// share: T -> Rc<T>
// clone: T -> T (where T: Clone)
```

## Codegen to lIR
```lisp
; (share x) compiles to:
(rc-alloc <type>)
(store x (rc-ptr result))

; (clone x) compiles to:
; Deep copy - depends on type
; For primitives: just copy
; For structs: allocate new + copy fields
; For rc: rc-clone (refcount++)
```

## Ownership Implications
- share creates Rc<T>, multiple owners
- clone creates fresh owned value
- Shared values immutable (use atoms for mutation)

## Acceptance Criteria
- [ ] share creates reference-counted value
- [ ] clone creates deep copy
- [ ] Shared values are immutable
- [ ] Proper refcount management
- [ ] Works with borrow checker
EOF

# -----------------------------------------------------------------------------

moth new "liar: byte arrays and regex (syntax)" -s low --no-edit --stdin << 'EOF'
## Summary
Implement byte array literals and regex reader macro.

## Syntax
```lisp
#[0x48 0x65 0x6c 0x6c 0x6f]  ; Byte array: "Hello"
#r"pattern"                   ; Regex literal
#r"foo.*bar"i                 ; Regex with flags
```

## AST Additions
```rust
pub enum Expr {
    // ... existing ...
    ByteArray(Vec<u8>),
    Regex(String, RegexFlags),
}

pub struct RegexFlags {
    pub case_insensitive: bool,
    pub multiline: bool,
    pub dotall: bool,
}
```

## Lexer
```rust
// #[ starts byte array
// #r" starts regex
'#' => match self.peek() {
    Some('[') => self.lex_byte_array(),
    Some('r') => self.lex_regex(),
    _ => error,
}
```

## Byte Array Operations
```lisp
(byte-array 72 101 108 108 111)  ; From integers
(bytes "Hello")                   ; From string
(get bytes 0)                     ; => 72
(len bytes)                       ; => 5
(slice bytes 1 3)                 ; => #[101 108]
```

## Regex Operations
```lisp
(match? #r"foo" "foobar")        ; => true
(find #r"(\d+)" "abc123")        ; => "123"
(replace #r"foo" "bar" "foobar") ; => "barbar"
```

## Implementation Notes
- Byte arrays: Vec<u8> or &[u8]
- Regex: Use regex crate, compile at parse time

## Acceptance Criteria
- [ ] #[...] byte array syntax parses
- [ ] #r"..." regex syntax parses
- [ ] Byte array operations work
- [ ] Regex matching works
- [ ] Regex compiled at parse time (perf)
EOF

# =============================================================================
# lIR ATOMIC PRIMITIVES
# =============================================================================

moth new "lIR: atomic load/store" -s high --no-edit --stdin << 'EOF'
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

// (atomic-store seq_cst value ptr)
"atomic-store" => {
    let ordering = self.parse_ordering()?;
    let value = self.parse_expr()?;
    let ptr = self.parse_expr()?;
    Ok(Expr::AtomicStore { ordering, value: Box::new(value), ptr: Box::new(ptr) })
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
        MemoryOrdering::Release => AtomicOrdering::Release,
        MemoryOrdering::AcqRel => AtomicOrdering::AcquireRelease,
        MemoryOrdering::SeqCst => AtomicOrdering::SequentiallyConsistent,
    };
    let load = self.builder.build_load(ty.to_llvm(self.ctx), ptr_val, "atomic_load");
    load.set_atomic_ordering(llvm_ordering)?;
    Ok(load)
}
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

# -----------------------------------------------------------------------------

moth new "lIR: atomicrmw (atomic read-modify-write)" -s high --no-edit --stdin << 'EOF'
## Summary
Add atomicrmw instruction for atomic read-modify-write operations.
Used for lock-free counters, accumulators, etc.

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
; Atomic increment
(atomicrmw add seq_cst counter (i64 1))  ; returns old value

; Atomic swap
(atomicrmw xchg seq_cst ptr new_value)   ; returns old value

; Atomic max
(atomicrmw max seq_cst ptr candidate)    ; returns old value
```

## AST Addition
```rust
pub enum AtomicRMWOp {
    Xchg,
    Add,
    Sub,
    And,
    Or,
    Xor,
    Min,
    Max,
    UMin,
    UMax,
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
        AtomicRMWOp::Sub => AtomicRMWBinOp::Sub,
        // ... etc
    };
    self.builder.build_atomicrmw(llvm_op, ptr_val, val, ordering.to_llvm())
}
```

## Use in RC (refactor)
Current RC implementation can use this explicitly:
```lisp
; rc-clone becomes:
(atomicrmw add seq_cst refcount_ptr (i64 1))

; rc-drop becomes:
(let ((old (atomicrmw sub seq_cst refcount_ptr (i64 1))))
  (br (icmp eq old (i64 1)) free_block continue_block))
```

## Test Cases
```gherkin
Scenario: Atomic increment
  Given the expression (define (test i64) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (let ((old (atomicrmw add seq_cst p (i64 5)))) (ret old)))))
  When I call test
  Then the result is (i64 10)

Scenario: Atomic increment - verify new value
  Given the expression (define (test i64) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (atomicrmw add seq_cst p (i64 5)) (ret (load i64 p)))))
  When I call test
  Then the result is (i64 15)
```

## Acceptance Criteria
- [ ] atomicrmw parses with all ops and orderings
- [ ] Returns old value
- [ ] Codegen produces LLVM atomicrmw
- [ ] All operations work (add, sub, xchg, etc.)
EOF

# -----------------------------------------------------------------------------

moth new "lIR: cmpxchg (compare-and-swap)" -s high --no-edit --stdin << 'EOF'
## Summary
Add cmpxchg (compare-and-exchange) instruction for lock-free algorithms.
This is the fundamental primitive for implementing swap! loops.

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
    
    // LLVM cmpxchg has success and failure orderings
    self.builder.build_cmpxchg(
        ptr_val,
        exp_val, 
        new_val,
        ordering.to_llvm(),  // success ordering
        ordering.to_llvm(),  // failure ordering (often can be weaker)
    )
}
```

## Test Cases
```gherkin
Scenario: CmpXchg success
  Given the expression (define (test i64) () (block entry (let ((p (alloca i64))) (store (i64 10) p) (let ((result (cmpxchg seq_cst p (i64 10) (i64 20)))) (ret (extractvalue result 0))))))
  When I call test
  Then the result is (i64 10)

Scenario: CmpXchg failure
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
- [ ] Can implement CAS loop
EOF

# -----------------------------------------------------------------------------

moth new "lIR: fence (memory barrier)" -s low --no-edit --stdin << 'EOF'
## Summary
Add fence instruction for explicit memory barriers.
Usually not needed (atomic ops have implicit fences), but useful for
advanced lock-free algorithms.

## Syntax
```lisp
(fence ordering)
```

## Semantics
- Prevents reordering of memory operations across the fence
- No return value (void)

## AST Addition
```rust
pub enum Expr {
    // ... existing ...
    Fence {
        ordering: MemoryOrdering,
    },
}
```

## Codegen
```rust
Expr::Fence { ordering } => {
    self.builder.build_fence(ordering.to_llvm(), "");
    // Returns void/unit
}
```

## Use Cases
- Implementing spin locks
- Memory barriers for non-atomic data
- Synchronizing with hardware

## Test Cases
```gherkin
Scenario: Fence compiles
  Given the expression (define (test void) () (block entry (fence seq_cst) (ret)))
  Then compilation succeeds
```

## Acceptance Criteria
- [ ] fence parses with all orderings
- [ ] Codegen produces LLVM fence
- [ ] No return value
EOF

echo ""
echo "Created 16 moths for missing liar/lIR features:"
echo ""
echo "THREADING (6):"
echo "  [HIGH] Closure color tracking (ADR-010)"
echo "  [HIGH] Atoms for shared state (ADR-011)"
echo "  [HIGH] plet verification (ADR-009)"
echo "  [MED]  pmap/pfilter parallel execution"
echo "  [MED]  dosync transactions (ADR-012)"
echo "  [MED]  async/await (ADR-014)"
echo ""
echo "COLLECTIONS (2):"
echo "  [HIGH] Persistent collections (ADR-018)"
echo "  [MED]  Conventional collections (ADR-018)"
echo ""
echo "SIMD (1):"
echo "  [MED]  SIMD vectors (ADR-016)"
echo ""
echo "NUMERIC (1):"
echo "  [LOW]  Numeric overflow handling (ADR-017)"
echo ""
echo "OTHER (2):"
echo "  [HIGH] share and clone"
echo "  [LOW]  Byte arrays and regex"
echo ""
echo "lIR ATOMICS (4):"
echo "  [HIGH] atomic-load/store"
echo "  [HIGH] atomicrmw (read-modify-write)"
echo "  [HIGH] cmpxchg (compare-and-swap)"
echo "  [LOW]  fence (memory barrier)"
echo ""
echo "Suggested implementation order:"
echo "  1. lIR atomics (foundation for atoms)"
echo "  2. share/clone (RC semantics)"
echo "  3. Closure color (thread safety)"
echo "  4. Atoms (swap!, @, etc.)"
echo "  5. plet verification (uses color)"
echo "  6. Persistent collections"
echo "  7. pmap/pfilter (parallel execution)"
echo "  8. SIMD, dosync, async as needed"
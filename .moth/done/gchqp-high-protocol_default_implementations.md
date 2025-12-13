# Protocol Default Implementations (Protocol-to-Protocol Extension)

## Summary

Add the ability to extend a protocol with a default implementation that applies to any type implementing another protocol. This enables composable abstractions where, for example, any `Seq` automatically gets a default `map` implementation without each type needing to implement it individually.

## Motivation

Currently `extend-protocol` only works with concrete types:

```lisp
(extend-protocol Mappable PersistentVector
  (map [self f] ...))
```

There's no way to provide a default implementation for all types implementing a protocol:

```lisp
;; NOT CURRENTLY POSSIBLE
(extend-protocol Mappable Seq
  (map [self f]
    (if (nil? (seq self))
        nil
        (cons (f (first self)) (map (rest self) f)))))
```

This means every type must implement every protocol method individually, preventing the kind of composable abstractions that make Clojure/Haskell ergonomic.

## Proposed Syntax

```lisp
;; Extend protocol with default for any Seq implementor
(extend-protocol-default Mappable Seq
  (map [self f]
    (if (nil? (seq self))
        nil
        (cons (f (first self)) (map (rest self) f)))))

;; Concrete types can still override
(extend-protocol Mappable PersistentVector
  (map [self f]
    (vec-map-fast self f)))  ; O(n) instead of O(n²)
```

Alternative syntax options (pick one):
- `(extend-protocol Mappable (protocol Seq) ...)` — explicit marker
- `(extend-protocol-default Mappable Seq ...)` — separate keyword (recommended)

## Compile-Time Resolution

This is **entirely compile-time** — no runtime dispatch overhead for the default case.

When the compiler sees `(map f some-vector)` and knows `some-vector: PersistentVector`:

1. Look for direct `Mappable` impl for `PersistentVector` → if found, emit call to `__Mappable_PersistentVector__map`
2. Otherwise, check if `PersistentVector` implements `Seq` → yes
3. Look for `Mappable` default for `Seq` → if found, emit call to `__Mappable_Seq__map`
4. Otherwise, compile error

Runtime dispatch (type_id switch) only occurs when the type is genuinely unknown at compile time.

## Implementation Plan

### 1. Parser Changes (`liar/src/parser.rs`)

Add parsing for `extend-protocol-default`:

```rust
TokenKind::ExtendProtocolDefault => {
    self.advance();
    Item::ExtendProtocolDefault(self.parse_extend_protocol_default()?)
}
```

New AST node in `ast.rs`:

```rust
pub struct ExtendProtocolDefault {
    pub protocol: Spanned<String>,      // Protocol being extended (e.g., Mappable)
    pub source_protocol: Spanned<String>, // Protocol providing the constraint (e.g., Seq)
    pub implementations: Vec<MethodImpl>,
}
```

### 2. CodegenContext Changes (`liar/src/codegen/context.rs`)

Add tracking for:

```rust
/// Which protocols each type implements: type_name -> [protocol_names]
type_protocols: HashMap<String, Vec<String>>,

/// Default implementations: (target_protocol, method) -> (source_protocol, impl_fn_name)
protocol_defaults: HashMap<(String, String), (String, String)>,
```

New methods:

```rust
/// Record that a type implements a protocol
pub fn register_type_protocol(&mut self, type_name: &str, protocol: &str)

/// Record a default implementation
pub fn register_protocol_default(
    &mut self,
    target_protocol: &str,
    source_protocol: &str, 
    method: &str,
    impl_fn: &str
)

/// Get protocols implemented by a type
pub fn get_type_protocols(&self, type_name: &str) -> &[String]

/// Look up default impl for a method
pub fn lookup_protocol_default(&self, protocol: &str, method: &str) -> Option<(&str, &str)>
```

### 3. Protocol Resolution (`liar/src/codegen/protocols.rs`)

Update `generate_protocol_call` to use hierarchical lookup:

```rust
fn resolve_protocol_impl(
    ctx: &CodegenContext,
    type_name: &str,
    method_name: &str,
) -> Option<String> {
    // 1. Direct implementation?
    if let Some(impl_fn) = ctx.lookup_protocol_impl(type_name, method_name) {
        return Some(impl_fn.clone());
    }
    
    // 2. Default via implemented protocol?
    for protocol in ctx.get_type_protocols(type_name) {
        // Find which target protocol has this method with a default for `protocol`
        if let Some((_, impl_fn)) = ctx.lookup_protocol_default_for_source(protocol, method_name) {
            return Some(impl_fn.clone());
        }
    }
    
    None
}
```

### 4. Codegen for Defaults (`liar/src/codegen/protocols.rs`)

Add `generate_extend_protocol_default`:

```rust
pub fn generate_extend_protocol_default(
    ctx: &mut CodegenContext,
    extend: &ExtendProtocolDefault,
) -> Result<Vec<lir::FunctionDef>> {
    let target_protocol = &extend.protocol.node;
    let source_protocol = &extend.source_protocol.node;
    
    let mut functions = Vec::new();
    
    for method_impl in &extend.implementations {
        let method_name = &method_impl.name.node;
        
        // Function name: __Mappable_Seq__map (target_source_method)
        let fn_name = format!("__{}_{}__{}", target_protocol, source_protocol, method_name);
        
        // Register the default
        ctx.register_protocol_default(target_protocol, source_protocol, method_name, &fn_name);
        
        // Generate the function (self is ptr, uses source protocol methods)
        // ... similar to existing extend-protocol codegen
    }
    
    Ok(functions)
}
```

### 5. Track Protocol Implementation

When processing `extend-protocol Seq Cons`, also call:

```rust
ctx.register_type_protocol("Cons", "Seq");
```

## Acceptance Criteria

- [ ] `extend-protocol-default` parses correctly
- [ ] Default implementations generate correct lIR functions
- [ ] Concrete type impls take precedence over defaults
- [ ] Types automatically get default when they implement the source protocol
- [ ] Compile error when no impl found (direct or default)
- [ ] Runtime dispatch still works for unknown types (falls back to type_id chain, including defaults)
- [ ] Feature file with test scenarios

## Test Scenarios

```gherkin
Feature: Protocol default implementations

  Scenario: Default implementation via protocol
    Given the program:
      """
      (defprotocol Seq
        (first [self])
        (rest [self]))
      
      (defprotocol Showable
        (show [self]))
      
      (extend-protocol-default Showable Seq
        (show [self] 
          (if (nil? self) 0 (+ 1 (show (rest self))))))
      
      (defstruct Cons (head: i64 tail: ptr))
      
      (extend-protocol Seq Cons
        (first [self] (. self head))
        (rest [self] (. self tail)))
      
      (defun main ()
        (let ((xs (Cons 1 (Cons 2 (Cons 3 nil)))))
          (show xs)))
      """
    When I compile and run it
    Then the result should be 3

  Scenario: Concrete impl overrides default
    Given the program:
      """
      (defprotocol Seq (first [self]) (rest [self]))
      (defprotocol Countable (count [self]))
      
      (extend-protocol-default Countable Seq
        (count [self]
          (if (nil? self) 0 (+ 1 (count (rest self))))))
      
      (defstruct FastList (len: i64 data: ptr))
      
      (extend-protocol Seq FastList
        (first [self] 0)
        (rest [self] nil))
      
      ;; Override with O(1) implementation
      (extend-protocol Countable FastList
        (count [self] (. self len)))
      
      (defun main ()
        (let ((xs (FastList 42 nil)))
          (count xs)))
      """
    When I compile and run it
    Then the result should be 42
```

## Notes

- This is foundational for the stdlib — `map`, `filter`, `reduce` can all be defaults on `Seq`/`Reducible`
- Enables ADR-022 Core Protocols vision
- All resolution is compile-time; runtime dispatch unchanged except for truly dynamic cases
- Consider: should defaults be transitive? (If A extends B, and B has default for C, does A get it?) — probably yes, but could defer

## References

- ADR-022: Core Protocols
- `liar/src/codegen/protocols.rs` — current protocol dispatch
- `lib/seq.liar` — current manual implementations

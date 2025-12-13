# ADR 023: Compile-Time Reflection

## Status

Proposed

## Context

We want structs to be composable like maps, implementing protocols like `Associative`. This requires generating protocol implementations that know about struct fields:

```lisp
(defstruct Point (x: i64 y: i64))
(derive Associative Point)  ;; needs to know Point has fields x, y
```

For `derive` to work as a macro, it must interrogate struct definitions at compile time. Currently, macros operate purely on syntax with no access to semantic information like struct field names.

### Options Considered

1. **Two-phase compilation**: Scan all structs first, then expand macros
   - Pro: Order-independent (derive can come before defstruct)
   - Con: More complex, slower compilation

2. **Single-pass with intrinsics**: Process top-to-bottom, intrinsics query what's been defined
   - Pro: Simpler, natural ordering
   - Con: Requires defstruct before derive

3. **Built-in derive in defstruct**: `(defstruct Point ... :derive (Associative))`
   - Pro: No reflection needed
   - Con: Less flexible, can't define custom derive macros

## Decision

**Single-pass with reflection intrinsics** (Option 2).

Structs must be defined before macros that reference them. This is natural - you define a type before you extend it.

### Reflection Intrinsics

These special forms are evaluated at macro-expansion time, not runtime:

```lisp
(struct-fields Point)           ;; => (x y)
(struct-field-type Point x)     ;; => i64
(struct? Point)                 ;; => true
```

### Usage in Macros

```lisp
(defmacro derive-associative (struct-name)
  (let ((fields (struct-fields struct-name)))
    `(extend-protocol Associative ,struct-name
       (assoc [self key val]
         (cond
           ,@(map (fn (f) `((= key ,(keyword f)) (set-field! self ,f val)))
                  fields))
         self))))
```

### Implementation

1. **Struct registry in Expander**: Track struct definitions as they're processed
2. **Intrinsic evaluation**: When expanding macros, recognize and evaluate reflection forms
3. **Single pass**: Items processed in source order

```
defstruct Point → register Point{x: i64, y: i64}
derive Associative Point → expand macro, (struct-fields Point) returns (x y)
```

## Consequences

### Positive

- **Extensible**: Users can write custom derive macros
- **Simple model**: Single-pass, order matters (like most languages)
- **Composable**: Reflection results are normal liar values in macro context
- **Low overhead**: No runtime cost, all resolved at compile time

### Negative

- **Order dependent**: Must define struct before deriving for it
- **Limited scope**: Only struct metadata, not arbitrary compile-time computation

### Dependencies

This ADR requires:
- `set-field!` primitive for struct field mutation
- `cond` macro for multi-way conditionals

## Related

- ADR 001: Immutability by Default (structs are mutable records)
- ADR 022: Core Protocols (Associative protocol)

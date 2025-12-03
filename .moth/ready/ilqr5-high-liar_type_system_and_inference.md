## Summary
Implement type representation and Hindley-Milner style inference.
No explicit type annotations required (but allowed).

## Type representation
```rust
pub enum Type {
    // Primitives
    Int(IntSize),       // i8, i16, i32, i64
    Float(FloatSize),   // f32, f64
    Bool,
    Char,
    String,
    Unit,               // ()
    
    // References
    Ref(Box<Type>),     // &T
    RefMut(Box<Type>),  // &mut T
    
    // Collections
    List(Box<Type>),
    Vector(Box<Type>),
    Map(Box<Type>, Box<Type>),
    SimdVector(Box<Type>, usize),  // <T x N>
    
    // Functions
    Fn {
        params: Vec<Type>,
        ret: Box<Type>,
    },
    
    // User-defined
    Struct(String),
    Enum(String),
    
    // Inference
    Var(TypeVarId),     // Unresolved type variable
    
    // Special
    Never,              // ! (diverges)
    Error,              // Type error placeholder
}
```

## Inference algorithm
```rust
pub struct Inferencer {
    substitutions: HashMap<TypeVarId, Type>,
    constraints: Vec<Constraint>,
    next_var: TypeVarId,
}

pub enum Constraint {
    Equal(Type, Type),              // T1 = T2
    Subtype(Type, Type),            // T1 <: T2 (for numeric promotion)
}

impl Inferencer {
    pub fn fresh_var(&mut self) -> Type;
    pub fn unify(&mut self, t1: &Type, t2: &Type) -> Result<(), TypeError>;
    pub fn infer_expr(&mut self, expr: &Expr, env: &TypeEnv) -> Result<Type, TypeError>;
    pub fn solve(&mut self) -> Result<Substitution, TypeError>;
}
```

## Type rules
- Literals: `42` → `i64`, `3.14` → `f64`
- Arithmetic: operands must match, result is same type
- Comparison: operands must match, result is `Bool`
- If: branches must have same type
- Let: binding type inferred from initializer
- Fn: param types from usage, return type from body
- Call: function type must match arg types

## Numeric promotion (ADR-017)
- Integer literals polymorphic until constrained
- Explicit type wins: `(i32 42)` is `i32`
- Mixed operations: error (no implicit promotion)
- Explicit promotion: `(as i64 x)`

## Acceptance criteria
- [ ] All types representable
- [ ] Literal types inferred
- [ ] Arithmetic types unified
- [ ] Function types inferred from usage
- [ ] Unification algorithm works
- [ ] Good error messages for type mismatches

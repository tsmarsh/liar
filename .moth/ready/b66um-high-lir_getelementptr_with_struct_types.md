## Summary
`GetElementPtr` currently only accepts `ScalarType` for the element type. For
struct field access (critical for closure environments), it needs to support
named struct types.

## Current limitation
```rust
GetElementPtr {
    ty: ScalarType,     // Can't reference %struct.point
    ptr: Box<Expr>,
    indices: Vec<Expr>,
    inbounds: bool,
}
```

## Required for closures
```lisp
(defstruct adder_env (i64))

; Access field 0 of the struct
(getelementptr %struct.adder_env %env (i32 0) (i32 0))
```

## Proposed fix
```rust
// New enum for GEP pointee types
enum GepType {
    Scalar(ScalarType),
    Struct(String),  // Named struct like "point" -> %struct.point
}

GetElementPtr {
    ty: GepType,
    ptr: Box<Expr>,
    indices: Vec<Expr>,
    inbounds: bool,
}
```

## Parser changes
```rust
fn parse_getelementptr(&mut self, _: bool) -> Result<Expr, ParseError> {
    // ... handle inbounds ...
    
    // Parse type - could be scalar or %struct.name
    let ty = match self.lexer.peek()? {
        Some(Token::Ident(s)) if s.starts_with("%struct.") => {
            self.lexer.next_token_peeked()?;
            GepType::Struct(s[8..].to_string())  // strip %struct. prefix
        }
        Some(Token::Ident(s)) => {
            self.lexer.next_token_peeked()?;
            GepType::Scalar(self.type_from_name(&s)?)
        }
        ...
    };
    ...
}
```

## Codegen changes
Need to look up struct type from module's type registry when generating GEP.

## Acceptance Criteria
- [ ] Parse `(getelementptr %struct.name ptr indices...)`
- [ ] Generate correct LLVM GEP for struct field access
- [ ] integration_milestone.feature struct scenarios pass

## Notes
- This is required for the closure simulation milestone test
- Struct types are defined with `defstruct` and registered in the module

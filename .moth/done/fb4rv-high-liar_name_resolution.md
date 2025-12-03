## Summary
Resolve all names to their definitions. Detect undefined variables,
duplicate definitions, and build scope information.

## Scope tracking
```rust
pub struct Scope {
    bindings: HashMap<String, BindingInfo>,
    parent: Option<Box<Scope>>,
}

pub struct BindingInfo {
    pub kind: BindingKind,
    pub span: Span,
    pub id: BindingId,  // Unique ID for later passes
}

pub enum BindingKind {
    Local,
    Parameter,
    Function,
    Struct,
    Enum,
    EnumVariant,
    Macro,
}
```

## Resolution pass
```rust
pub struct Resolver {
    scopes: Vec<Scope>,
    current_function: Option<String>,
    errors: Vec<ResolveError>,
}

impl Resolver {
    pub fn resolve(&mut self, program: &[TopLevel]) -> Result<ResolvedProgram, Vec<ResolveError>>;
}
```

## What gets resolved
- Variable references → binding site
- Function calls → function definition
- Struct constructors → struct definition
- Enum variants → enum definition
- Forward references (mutual recursion)

## Errors detected
- Undefined variable
- Undefined function
- Duplicate definition in same scope
- Use of variable in own initializer (except recursion)

## Acceptance criteria
- [ ] All names resolved to binding sites
- [ ] Scopes correctly nested
- [ ] Forward references work for functions
- [ ] Undefined variable detected
- [ ] Duplicate definition detected

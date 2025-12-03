## Summary
Parse tokens into liar AST. S-expression based with special forms.

## AST types
```rust
pub enum Expr {
    // Literals
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Char(char),
    Symbol(String),
    Keyword(String),
    Nil,
    
    // Collections
    List(Vec<Expr>),            // '(1 2 3)
    Vector(Vec<Expr>),          // [1 2 3]
    Map(Vec<(Expr, Expr)>),     // {:a 1 :b 2}
    SimdVector(Vec<Expr>),      // <<1 2 3 4>>
    
    // References
    Borrow(Box<Expr>),          // &x
    BorrowMut(Box<Expr>),       // &mut x
    Move(Box<Expr>),            // ^x
    
    // Special forms
    If {
        cond: Box<Expr>,
        then_: Box<Expr>,
        else_: Option<Box<Expr>>,
    },
    Let {
        bindings: Vec<(Pattern, Expr)>,
        body: Vec<Expr>,
    },
    Fn {
        params: Vec<Param>,
        body: Vec<Expr>,
    },
    Do(Vec<Expr>),
    Quote(Box<Expr>),
    Quasiquote(Box<Expr>),
    Unquote(Box<Expr>),
    UnquoteSplice(Box<Expr>),
    
    // Application
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    
    // Pattern matching
    Match {
        expr: Box<Expr>,
        arms: Vec<(Pattern, Expr)>,
    },
    
    // Loops
    Loop {
        bindings: Vec<(String, Expr)>,
        body: Vec<Expr>,
    },
    Recur(Vec<Expr>),
}

pub enum Pattern {
    Wildcard,                   // _
    Binding(String),            // x
    BorrowBinding(String),      // &x
    Literal(Expr),              // 42, "foo"
    Constructor {               // (Some x), (Point x y)
        name: String,
        fields: Vec<Pattern>,
    },
    Vector(Vec<Pattern>),       // [a b c]
    VectorRest {                // [a b . rest]
        init: Vec<Pattern>,
        rest: String,
    },
}

pub struct Param {
    pub name: String,
    pub mode: ParamMode,
    pub ty: Option<Type>,       // optional type annotation
}

pub enum ParamMode {
    Move,       // default
    Borrow,     // &
    BorrowMut,  // &mut
}
```

## Top-level forms
```rust
pub enum TopLevel {
    Define {
        name: String,
        params: Vec<Param>,
        body: Vec<Expr>,
    },
    DefStruct {
        name: String,
        fields: Vec<(String, Option<Type>)>,
    },
    DefEnum {
        name: String,
        variants: Vec<Variant>,
    },
    DefMacro {
        name: String,
        params: Vec<String>,
        body: Expr,
    },
    Expr(Expr),
}
```

## Parser API
```rust
pub struct Parser<'a> {
    tokens: &'a [SpannedToken],
    pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [SpannedToken]) -> Self;
    pub fn parse_expr(&mut self) -> Result<Expr, ParseError>;
    pub fn parse_top_level(&mut self) -> Result<TopLevel, ParseError>;
    pub fn parse_program(&mut self) -> Result<Vec<TopLevel>, ParseError>;
}
```

## Acceptance criteria
- [ ] All expression types parsed
- [ ] Special forms recognized (if, let, fn, do, match, loop)
- [ ] Patterns parsed
- [ ] Top-level forms parsed (define, defstruct, defenum, defmacro)
- [ ] Error messages include spans

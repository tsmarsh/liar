## Summary
Tokenize liar source into tokens with spans for error reporting.

## Token types
```rust
pub enum Token {
    // Delimiters
    LParen,           // (
    RParen,           // )
    LBracket,         // [
    RBracket,         // ]
    LBrace,           // {
    RBrace,           // }
    
    // Literals
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    
    // Identifiers and keywords
    Symbol(String),   // foo, bar, +, -, etc.
    Keyword(String),  // :foo
    
    // Special
    Quote,            // '
    Quasiquote,       // `
    Unquote,          // ,
    UnquoteSplice,    // ,@
    Ampersand,        // &
    AmpersandMut,     // &mut
    Caret,            // ^
    
    // SIMD vector delimiters
    DoubleLAngle,     // <<
    DoubleRAngle,     // >>
}
```

## Span tracking
```rust
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub file: Option<String>,
}

pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

pub type SpannedToken = Spanned<Token>;
```

## Lexer API
```rust
pub struct Lexer<'a> {
    source: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self;
    pub fn next_token(&mut self) -> Result<Option<SpannedToken>, LexError>;
    pub fn tokenize_all(&mut self) -> Result<Vec<SpannedToken>, LexError>;
}
```

## Special cases
- Comments: `;` to end of line
- Nested comments: `#| ... |#` (optional)
- String escapes: `\n`, `\t`, `\\`, `\"`, `\xHH`
- Character literals: `\a`, `\newline`, `\space`
- Numeric literals: `42`, `-42`, `3.14`, `0xFF`, `0b1010`

## Acceptance criteria
- [ ] All token types recognized
- [ ] Spans are accurate
- [ ] Comments skipped
- [ ] String escapes handled
- [ ] Hex/binary literals work

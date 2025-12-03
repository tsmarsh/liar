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

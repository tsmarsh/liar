//! Lexer - tokenization of liar source

use crate::error::{CompileError, Result};
use crate::span::Span;

/// Token type
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Delimiters
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,

    // Literals
    Int(i64),
    Float(f64),
    String(String),
    Symbol(String),
    Keyword(String), // :foo

    // Keywords
    Defun,
    Def,
    Defstruct,
    Let,
    Plet,
    Fn,
    If,
    Do,
    Set,
    Ref,
    RefMut,
    Deref,
    Match,
    Unsafe,
    True,
    False,
    Nil,

    // Overflow handling (ADR-017)
    Boxed,    // boxed
    Wrapping, // wrapping

    // Operators/Punctuation
    Quote,    // '
    Backtick, // ` (quasiquote)
    Comma,    // , (unquote)
    CommaAt,  // ,@ (unquote-splicing)
    Colon,    // :
    Amp,      // &
    Dot,      // .
    Arrow,    // ->
    At,       // @ (atom deref)

    // Atom keywords
    Atom,          // atom
    Swap,          // swap!
    Reset,         // reset!
    CompareAndSet, // compare-and-set!

    // Async keywords
    Async, // async
    Await, // await

    // STM keywords (ADR-012)
    Dosync,  // dosync
    RefSet,  // ref-set
    Alter,   // alter
    Commute, // commute

    // Protocol keywords (ADR-022)
    Defprotocol,    // defprotocol
    ExtendProtocol, // extend-protocol

    // Macro keywords
    Defmacro, // defmacro
    Gensym,   // gensym

    // Iterator keywords
    Iter,    // iter (create iterator)
    Collect, // collect (materialize iterator)

    // Conventional collections
    LAngleBracket, // <[ (start conventional vector)
    LAngleBrace,   // <{ (start conventional map)
    RBracketAngle, // ]> (end conventional vector)
    RBraceAngle,   // }> (end conventional map)

    // SIMD vectors (ADR-016)
    DoubleLAngle, // << (start SIMD vector)
    DoubleRAngle, // >> (end SIMD vector)

    // Byte arrays and regex
    HashLBracket,          // #[ (start byte array)
    Regex(String, String), // #r"pattern"flags (regex literal)

    // Special
    Eof,
}

/// A token with its span
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

/// Lexer state
pub struct Lexer<'a> {
    #[allow(dead_code)]
    source: &'a str,
    chars: std::iter::Peekable<std::str::CharIndices<'a>>,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
            pos: 0,
        }
    }

    /// Get the next token
    pub fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespace_and_comments();

        let start = self.pos;

        let Some((pos, ch)) = self.chars.next() else {
            return Ok(Token::new(TokenKind::Eof, Span::new(start, start)));
        };

        self.pos = pos + ch.len_utf8();

        let kind = match ch {
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '[' => TokenKind::LBracket,
            ']' => {
                // Check for ]> (end conventional vector)
                if self.peek_char() == Some('>') {
                    self.advance();
                    TokenKind::RBracketAngle
                } else {
                    TokenKind::RBracket
                }
            }
            '{' => TokenKind::LBrace,
            '}' => {
                // Check for }> (end conventional map)
                if self.peek_char() == Some('>') {
                    self.advance();
                    TokenKind::RBraceAngle
                } else {
                    TokenKind::RBrace
                }
            }
            '\'' => TokenKind::Quote,
            '`' => TokenKind::Backtick,
            ',' => {
                // Check for ,@ (unquote-splicing)
                if self.peek_char() == Some('@') {
                    self.advance();
                    TokenKind::CommaAt
                } else {
                    TokenKind::Comma
                }
            }

            // Conventional mutable collections (ADR-018) and SIMD vectors (ADR-016)
            '<' => match self.peek_char() {
                Some('<') => {
                    // SIMD vector: <<...>>
                    self.advance();
                    TokenKind::DoubleLAngle
                }
                Some('[') => {
                    self.advance();
                    TokenKind::LAngleBracket
                }
                Some('{') => {
                    self.advance();
                    TokenKind::LAngleBrace
                }
                _ => {
                    // Bare '<' is a symbol character, treat as symbol start
                    return Ok(Token::new(
                        self.lex_symbol(ch, start),
                        Span::new(start, self.pos),
                    ));
                }
            },
            '>' => {
                // Check for >> (end SIMD vector)
                if self.peek_char() == Some('>') {
                    self.advance();
                    TokenKind::DoubleRAngle
                } else {
                    // Bare '>' is a symbol character, treat as symbol start
                    return Ok(Token::new(
                        self.lex_symbol(ch, start),
                        Span::new(start, self.pos),
                    ));
                }
            }
            ':' => {
                // Check if this is a keyword (:foo) or just a colon
                if let Some(c) = self.peek_char() {
                    if is_symbol_start(c) || c.is_alphabetic() {
                        // It's a keyword like :foo
                        return Ok(Token::new(
                            self.lex_keyword(start),
                            Span::new(start, self.pos),
                        ));
                    }
                }
                TokenKind::Colon
            }
            '&' => TokenKind::Amp,
            '.' => TokenKind::Dot,

            '-' if self.peek_char() == Some('>') => {
                self.advance();
                TokenKind::Arrow
            }

            '@' => TokenKind::At,

            // Hash-prefixed literals (byte arrays and regex)
            '#' => match self.peek_char() {
                Some('[') => {
                    self.advance();
                    TokenKind::HashLBracket
                }
                Some('r') => {
                    self.advance();
                    self.lex_regex(start)?
                }
                _ => {
                    return Err(CompileError::lex(
                        Span::new(start, self.pos),
                        "expected '[' or 'r' after '#'",
                    ))
                }
            },

            '"' => self.lex_string(start)?,

            c if c.is_ascii_digit() => self.lex_number(c, start)?,
            '-' if self
                .peek_char()
                .map(|c| c.is_ascii_digit())
                .unwrap_or(false) =>
            {
                self.lex_number(ch, start)?
            }

            c if is_symbol_start(c) => self.lex_symbol(c, start),

            _ => {
                return Err(CompileError::lex(
                    Span::new(start, self.pos),
                    format!("unexpected character: '{}'", ch),
                ))
            }
        };

        Ok(Token::new(kind, Span::new(start, self.pos)))
    }

    /// Tokenize the entire source
    pub fn tokenize(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token()?;
            let is_eof = token.kind == TokenKind::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        Ok(tokens)
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.peek_char() {
                Some(c) if c.is_whitespace() => {
                    self.advance();
                }
                Some(';') => {
                    // Line comment
                    while self.peek_char().map(|c| c != '\n').unwrap_or(false) {
                        self.advance();
                    }
                }
                _ => break,
            }
        }
    }

    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| *c)
    }

    fn advance(&mut self) -> Option<char> {
        self.chars.next().map(|(pos, c)| {
            self.pos = pos + c.len_utf8();
            c
        })
    }

    fn lex_string(&mut self, start: usize) -> Result<TokenKind> {
        let mut value = String::new();

        loop {
            match self.advance() {
                Some('"') => break,
                Some('\\') => {
                    let escaped = match self.advance() {
                        Some('n') => '\n',
                        Some('r') => '\r',
                        Some('t') => '\t',
                        Some('\\') => '\\',
                        Some('"') => '"',
                        Some(c) => {
                            return Err(CompileError::lex(
                                Span::new(start, self.pos),
                                format!("invalid escape sequence: \\{}", c),
                            ))
                        }
                        None => {
                            return Err(CompileError::lex(
                                Span::new(start, self.pos),
                                "unterminated string",
                            ))
                        }
                    };
                    value.push(escaped);
                }
                Some(c) => value.push(c),
                None => {
                    return Err(CompileError::lex(
                        Span::new(start, self.pos),
                        "unterminated string",
                    ))
                }
            }
        }

        Ok(TokenKind::String(value))
    }

    fn lex_number(&mut self, first: char, _start: usize) -> Result<TokenKind> {
        let mut s = String::new();
        s.push(first);

        while let Some(c) = self.peek_char() {
            if c.is_ascii_digit() || c == '.' || c == 'e' || c == 'E' || c == '-' || c == '+' {
                // Check for double dots or invalid number patterns
                if c == '.' && s.contains('.') {
                    break;
                }
                s.push(self.advance().unwrap());
            } else {
                break;
            }
        }

        if s.contains('.') || s.contains('e') || s.contains('E') {
            Ok(TokenKind::Float(s.parse().unwrap()))
        } else {
            Ok(TokenKind::Int(s.parse().unwrap()))
        }
    }

    fn lex_keyword(&mut self, _start: usize) -> TokenKind {
        // We've already consumed the ':', now read the symbol part
        let mut s = String::new();

        while let Some(c) = self.peek_char() {
            if is_symbol_continue(c) || c.is_alphabetic() {
                s.push(self.advance().unwrap());
            } else {
                break;
            }
        }

        TokenKind::Keyword(s)
    }

    fn lex_symbol(&mut self, first: char, _start: usize) -> TokenKind {
        let mut s = String::new();
        s.push(first);

        while let Some(c) = self.peek_char() {
            if is_symbol_continue(c) {
                s.push(self.advance().unwrap());
            } else {
                break;
            }
        }

        // Check for keywords
        match s.as_str() {
            "defun" => TokenKind::Defun,
            "def" => TokenKind::Def,
            "defstruct" => TokenKind::Defstruct,
            "let" => TokenKind::Let,
            "plet" => TokenKind::Plet,
            "fn" => TokenKind::Fn,
            "if" => TokenKind::If,
            "do" => TokenKind::Do,
            "set!" => TokenKind::Set,
            "ref" => TokenKind::Ref,
            "ref-mut" => TokenKind::RefMut,
            "deref" => TokenKind::Deref,
            "match" => TokenKind::Match,
            "unsafe" => TokenKind::Unsafe,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "nil" => TokenKind::Nil,
            // Overflow handling keywords
            "boxed" => TokenKind::Boxed,
            "wrapping" => TokenKind::Wrapping,
            // Atom keywords
            "atom" => TokenKind::Atom,
            "swap!" => TokenKind::Swap,
            "reset!" => TokenKind::Reset,
            "compare-and-set!" => TokenKind::CompareAndSet,
            // Async keywords
            "async" => TokenKind::Async,
            "await" => TokenKind::Await,
            // STM keywords
            "dosync" => TokenKind::Dosync,
            "ref-set" => TokenKind::RefSet,
            "alter" => TokenKind::Alter,
            "commute" => TokenKind::Commute,
            // Protocol keywords
            "defprotocol" => TokenKind::Defprotocol,
            "extend-protocol" => TokenKind::ExtendProtocol,
            // Macro keywords
            "defmacro" => TokenKind::Defmacro,
            "gensym" => TokenKind::Gensym,
            // Iterator keywords
            "iter" => TokenKind::Iter,
            "collect" => TokenKind::Collect,
            _ => TokenKind::Symbol(s),
        }
    }

    /// Lex a regex literal: #r"pattern"flags
    fn lex_regex(&mut self, start: usize) -> Result<TokenKind> {
        // Expect opening quote
        match self.advance() {
            Some('"') => {}
            _ => {
                return Err(CompileError::lex(
                    Span::new(start, self.pos),
                    "expected '\"' after #r",
                ))
            }
        }

        // Read pattern until closing quote
        let mut pattern = String::new();
        loop {
            match self.advance() {
                Some('"') => break,
                Some('\\') => {
                    // Escape sequences in regex - pass through literally
                    match self.advance() {
                        Some(c) => {
                            pattern.push('\\');
                            pattern.push(c);
                        }
                        None => {
                            return Err(CompileError::lex(
                                Span::new(start, self.pos),
                                "unterminated regex pattern",
                            ))
                        }
                    }
                }
                Some(c) => pattern.push(c),
                None => {
                    return Err(CompileError::lex(
                        Span::new(start, self.pos),
                        "unterminated regex pattern",
                    ))
                }
            }
        }

        // Read optional flags (i, m, s, x, etc.)
        let mut flags = String::new();
        while let Some(c) = self.peek_char() {
            if c.is_ascii_alphabetic() {
                flags.push(self.advance().unwrap());
            } else {
                break;
            }
        }

        Ok(TokenKind::Regex(pattern, flags))
    }
}

fn is_symbol_start(c: char) -> bool {
    c.is_alphabetic() || matches!(c, '_' | '+' | '-' | '*' | '/' | '=' | '<' | '>' | '!' | '?')
}

fn is_symbol_continue(c: char) -> bool {
    is_symbol_start(c) || c.is_ascii_digit()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_tokens() {
        let mut lexer = Lexer::new("(+ 1 2)");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 6); // ( + 1 2 ) Eof
        assert_eq!(tokens[0].kind, TokenKind::LParen);
        assert_eq!(tokens[1].kind, TokenKind::Symbol("+".to_string()));
        assert_eq!(tokens[2].kind, TokenKind::Int(1));
        assert_eq!(tokens[3].kind, TokenKind::Int(2));
        assert_eq!(tokens[4].kind, TokenKind::RParen);
        assert_eq!(tokens[5].kind, TokenKind::Eof);
    }

    #[test]
    fn test_byte_array_tokens() {
        let mut lexer = Lexer::new("#[1 2 3]");
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 6); // #[ 1 2 3 ] Eof
        assert_eq!(tokens[0].kind, TokenKind::HashLBracket);
        assert_eq!(tokens[1].kind, TokenKind::Int(1));
        assert_eq!(tokens[2].kind, TokenKind::Int(2));
        assert_eq!(tokens[3].kind, TokenKind::Int(3));
        assert_eq!(tokens[4].kind, TokenKind::RBracket);
        assert_eq!(tokens[5].kind, TokenKind::Eof);
    }

    #[test]
    fn test_regex_token() {
        let mut lexer = Lexer::new(r#"#r"hello""#);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 2); // regex Eof
        assert_eq!(
            tokens[0].kind,
            TokenKind::Regex("hello".to_string(), "".to_string())
        );
    }

    #[test]
    fn test_regex_token_with_flags() {
        let mut lexer = Lexer::new(r#"#r"pattern"im"#);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 2);
        assert_eq!(
            tokens[0].kind,
            TokenKind::Regex("pattern".to_string(), "im".to_string())
        );
    }

    #[test]
    fn test_regex_token_with_escapes() {
        let mut lexer = Lexer::new(r#"#r"\d+\s*""#);
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens.len(), 2);
        assert_eq!(
            tokens[0].kind,
            TokenKind::Regex(r"\d+\s*".to_string(), "".to_string())
        );
    }
}

//! LDN Lexer
//!
//! Tokenizes LDN source into a stream of tokens.

use crate::error::{LexError, LexResult};
use std::iter::Peekable;
use std::str::CharIndices;

/// Token types for LDN
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    Keyword(String),
    Symbol(String),

    // Delimiters
    LBracket,  // [
    RBracket,  // ]
    LBrace,    // {
    RBrace,    // }
    HashBrace, // #{

    // End of file
    Eof,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Nil => write!(f, "nil"),
            Token::Bool(b) => write!(f, "{}", b),
            Token::Int(n) => write!(f, "{}", n),
            Token::Float(n) => write!(f, "{}", n),
            Token::String(s) => write!(f, "\"{}\"", s),
            Token::Keyword(k) => write!(f, ":{}", k),
            Token::Symbol(s) => write!(f, "{}", s),
            Token::LBracket => write!(f, "["),
            Token::RBracket => write!(f, "]"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::HashBrace => write!(f, "#{{"),
            Token::Eof => write!(f, "EOF"),
        }
    }
}

/// Lexer for LDN source
pub struct Lexer<'a> {
    #[allow(dead_code)]
    source: &'a str,
    chars: Peekable<CharIndices<'a>>,
    pos: usize,
}

impl<'a> Lexer<'a> {
    /// Create a new lexer for the given source
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.char_indices().peekable(),
            pos: 0,
        }
    }

    /// Tokenize the entire source, returning a vector of tokens
    pub fn tokenize(&mut self) -> LexResult<Vec<Token>> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token()?;
            if matches!(token, Token::Eof) {
                tokens.push(token);
                break;
            }
            tokens.push(token);
        }
        Ok(tokens)
    }

    /// Get the next token
    pub fn next_token(&mut self) -> LexResult<Token> {
        self.skip_whitespace_and_comments();

        let Some(&(pos, ch)) = self.chars.peek() else {
            return Ok(Token::Eof);
        };
        self.pos = pos;

        match ch {
            '[' => {
                self.advance();
                Ok(Token::LBracket)
            }
            ']' => {
                self.advance();
                Ok(Token::RBracket)
            }
            '{' => {
                self.advance();
                Ok(Token::LBrace)
            }
            '}' => {
                self.advance();
                Ok(Token::RBrace)
            }
            '#' => {
                self.advance();
                // Check for #{
                if self.peek_char() == Some('{') {
                    self.advance();
                    Ok(Token::HashBrace)
                } else {
                    Err(LexError::new(
                        "expected '{' after '#' for set literal",
                        self.pos,
                    ))
                }
            }
            '"' => self.scan_string(),
            ':' => self.scan_keyword(),
            '-' => {
                // Could be negative number or symbol
                self.advance();
                if self
                    .peek_char()
                    .map(|c| c.is_ascii_digit())
                    .unwrap_or(false)
                {
                    self.scan_number(true)
                } else {
                    // It's a symbol starting with -
                    self.scan_symbol_continue("-")
                }
            }
            c if c.is_ascii_digit() => {
                self.advance();
                self.scan_number_from(c, false)
            }
            c if is_symbol_start(c) => self.scan_symbol(),
            c => Err(LexError::new(format!("unexpected character: '{}'", c), pos)),
        }
    }

    /// Advance and return the current character
    fn advance(&mut self) -> Option<char> {
        self.chars.next().map(|(_, c)| c)
    }

    /// Peek at the next character without consuming
    fn peek_char(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| *c)
    }

    /// Skip whitespace and comments
    fn skip_whitespace_and_comments(&mut self) {
        loop {
            match self.chars.peek() {
                Some(&(_, c)) if c.is_whitespace() || c == ',' => {
                    self.advance();
                }
                Some(&(_, ';')) => {
                    // Comment - skip to end of line
                    while let Some(&(_, c)) = self.chars.peek() {
                        self.advance();
                        if c == '\n' {
                            break;
                        }
                    }
                }
                _ => break,
            }
        }
    }

    /// Scan a string literal
    fn scan_string(&mut self) -> LexResult<Token> {
        self.advance(); // consume opening "
        let mut s = String::new();

        loop {
            match self.advance() {
                Some('"') => return Ok(Token::String(s)),
                Some('\\') => {
                    // Escape sequence
                    match self.advance() {
                        Some('n') => s.push('\n'),
                        Some('r') => s.push('\r'),
                        Some('t') => s.push('\t'),
                        Some('\\') => s.push('\\'),
                        Some('"') => s.push('"'),
                        Some(c) => {
                            return Err(LexError::new(
                                format!("invalid escape sequence: \\{}", c),
                                self.pos,
                            ))
                        }
                        None => return Err(LexError::new("unterminated string", self.pos)),
                    }
                }
                Some(c) => s.push(c),
                None => return Err(LexError::new("unterminated string", self.pos)),
            }
        }
    }

    /// Scan a keyword (starts with :)
    fn scan_keyword(&mut self) -> LexResult<Token> {
        self.advance(); // consume :
        let mut name = String::new();

        while let Some(&(_, c)) = self.chars.peek() {
            if is_symbol_continue(c) {
                name.push(c);
                self.advance();
            } else {
                break;
            }
        }

        if name.is_empty() {
            Err(LexError::new("expected keyword name after ':'", self.pos))
        } else {
            Ok(Token::Keyword(name))
        }
    }

    /// Scan a symbol
    fn scan_symbol(&mut self) -> LexResult<Token> {
        let mut name = String::new();

        while let Some(&(_, c)) = self.chars.peek() {
            if is_symbol_continue(c) {
                name.push(c);
                self.advance();
            } else {
                break;
            }
        }

        // Check for reserved words
        match name.as_str() {
            "nil" => Ok(Token::Nil),
            "true" => Ok(Token::Bool(true)),
            "false" => Ok(Token::Bool(false)),
            _ => Ok(Token::Symbol(name)),
        }
    }

    /// Continue scanning a symbol with a prefix already consumed
    fn scan_symbol_continue(&mut self, prefix: &str) -> LexResult<Token> {
        let mut name = prefix.to_string();

        while let Some(&(_, c)) = self.chars.peek() {
            if is_symbol_continue(c) {
                name.push(c);
                self.advance();
            } else {
                break;
            }
        }

        Ok(Token::Symbol(name))
    }

    /// Scan a number (negative sign already consumed if present)
    fn scan_number(&mut self, negative: bool) -> LexResult<Token> {
        let first = self.advance().unwrap();
        self.scan_number_from(first, negative)
    }

    /// Scan a number with first digit already consumed
    fn scan_number_from(&mut self, first: char, negative: bool) -> LexResult<Token> {
        let mut s = String::new();
        if negative {
            s.push('-');
        }
        s.push(first);

        let mut is_float = false;

        // Consume digits
        while let Some(&(_, c)) = self.chars.peek() {
            if c.is_ascii_digit() {
                s.push(c);
                self.advance();
            } else if c == '.' {
                // Check if this is a decimal point or method call
                // Look ahead to see if there's a digit after
                self.advance();
                if self
                    .peek_char()
                    .map(|c| c.is_ascii_digit())
                    .unwrap_or(false)
                {
                    is_float = true;
                    s.push('.');
                    // Consume fractional digits
                    while let Some(&(_, c)) = self.chars.peek() {
                        if c.is_ascii_digit() {
                            s.push(c);
                            self.advance();
                        } else {
                            break;
                        }
                    }
                } else {
                    // Not a float - we consumed the dot but it's not part of the number
                    // This is an error in LDN (no method calls)
                    return Err(LexError::new(
                        "invalid number: decimal point must be followed by digits",
                        self.pos,
                    ));
                }
            } else if c == 'e' || c == 'E' {
                is_float = true;
                s.push(c);
                self.advance();
                // Optional sign
                if let Some(&(_, sign)) = self.chars.peek() {
                    if sign == '+' || sign == '-' {
                        s.push(sign);
                        self.advance();
                    }
                }
                // Exponent digits
                while let Some(&(_, c)) = self.chars.peek() {
                    if c.is_ascii_digit() {
                        s.push(c);
                        self.advance();
                    } else {
                        break;
                    }
                }
            } else {
                break;
            }
        }

        if is_float {
            s.parse::<f64>()
                .map(Token::Float)
                .map_err(|_| LexError::new(format!("invalid float: {}", s), self.pos))
        } else {
            s.parse::<i64>()
                .map(Token::Int)
                .map_err(|_| LexError::new(format!("invalid integer: {}", s), self.pos))
        }
    }
}

/// Check if a character can start a symbol
fn is_symbol_start(c: char) -> bool {
    c.is_alphabetic() || matches!(c, '_' | '+' | '*' | '/' | '!' | '?' | '<' | '>' | '=' | '&')
}

/// Check if a character can continue a symbol
fn is_symbol_continue(c: char) -> bool {
    is_symbol_start(c) || c.is_ascii_digit() || matches!(c, '-' | '.')
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(source: &str) -> Vec<Token> {
        Lexer::new(source).tokenize().unwrap()
    }

    fn lex_one(source: &str) -> Token {
        let tokens = lex(source);
        assert_eq!(tokens.len(), 2); // token + EOF
        tokens.into_iter().next().unwrap()
    }

    #[test]
    fn test_nil() {
        assert_eq!(lex_one("nil"), Token::Nil);
    }

    #[test]
    fn test_bool() {
        assert_eq!(lex_one("true"), Token::Bool(true));
        assert_eq!(lex_one("false"), Token::Bool(false));
    }

    #[test]
    fn test_int() {
        assert_eq!(lex_one("42"), Token::Int(42));
        assert_eq!(lex_one("-17"), Token::Int(-17));
        assert_eq!(lex_one("0"), Token::Int(0));
    }

    #[test]
    fn test_float() {
        assert_eq!(lex_one("3.5"), Token::Float(3.5));
        assert_eq!(lex_one("-2.5"), Token::Float(-2.5));
        assert_eq!(lex_one("1e10"), Token::Float(1e10));
        assert_eq!(lex_one("1.5e-3"), Token::Float(1.5e-3));
    }

    #[test]
    fn test_string() {
        assert_eq!(lex_one("\"hello\""), Token::String("hello".to_string()));
        assert_eq!(
            lex_one("\"hello\\nworld\""),
            Token::String("hello\nworld".to_string())
        );
        assert_eq!(
            lex_one("\"tab\\there\""),
            Token::String("tab\there".to_string())
        );
    }

    #[test]
    fn test_keyword() {
        assert_eq!(lex_one(":foo"), Token::Keyword("foo".to_string()));
        assert_eq!(lex_one(":my-key"), Token::Keyword("my-key".to_string()));
        assert_eq!(
            lex_one(":namespaced/key"),
            Token::Keyword("namespaced/key".to_string())
        );
    }

    #[test]
    fn test_symbol() {
        assert_eq!(lex_one("foo"), Token::Symbol("foo".to_string()));
        assert_eq!(lex_one("foo.bar"), Token::Symbol("foo.bar".to_string()));
        assert_eq!(lex_one("+"), Token::Symbol("+".to_string()));
    }

    #[test]
    fn test_delimiters() {
        assert_eq!(lex_one("["), Token::LBracket);
        assert_eq!(lex_one("]"), Token::RBracket);
        assert_eq!(lex_one("{"), Token::LBrace);
        assert_eq!(lex_one("}"), Token::RBrace);
        assert_eq!(lex_one("#{"), Token::HashBrace);
    }

    #[test]
    fn test_vector() {
        let tokens = lex("[1 2 3]");
        assert_eq!(
            tokens,
            vec![
                Token::LBracket,
                Token::Int(1),
                Token::Int(2),
                Token::Int(3),
                Token::RBracket,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_map() {
        let tokens = lex("{:a 1 :b 2}");
        assert_eq!(
            tokens,
            vec![
                Token::LBrace,
                Token::Keyword("a".to_string()),
                Token::Int(1),
                Token::Keyword("b".to_string()),
                Token::Int(2),
                Token::RBrace,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_set() {
        let tokens = lex("#{1 2 3}");
        assert_eq!(
            tokens,
            vec![
                Token::HashBrace,
                Token::Int(1),
                Token::Int(2),
                Token::Int(3),
                Token::RBrace,
                Token::Eof,
            ]
        );
    }

    #[test]
    fn test_comments() {
        let tokens = lex("; this is a comment\n42");
        assert_eq!(tokens, vec![Token::Int(42), Token::Eof]);
    }

    #[test]
    fn test_commas_as_whitespace() {
        let tokens = lex("[1, 2, 3]");
        assert_eq!(
            tokens,
            vec![
                Token::LBracket,
                Token::Int(1),
                Token::Int(2),
                Token::Int(3),
                Token::RBracket,
                Token::Eof,
            ]
        );
    }
}

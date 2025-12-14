//! LDN Parser
//!
//! Parses a stream of tokens into LDN values.

use crate::error::{ParseError, ParseResult};
use crate::lexer::Token;
use crate::value::Value;

/// Parser for LDN tokens
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    /// Create a new parser for the given tokens
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    /// Parse a single value
    pub fn parse(&mut self) -> ParseResult<Value> {
        let value = self.parse_value()?;

        // Ensure we've consumed all non-EOF tokens
        if !self.is_eof() {
            return Err(ParseError::unexpected(&self.peek().to_string(), self.pos));
        }

        Ok(value)
    }

    /// Parse a value
    fn parse_value(&mut self) -> ParseResult<Value> {
        match self.peek() {
            Token::Nil => {
                self.advance();
                Ok(Value::Nil)
            }
            Token::Bool(b) => {
                let b = *b;
                self.advance();
                Ok(Value::Bool(b))
            }
            Token::Int(n) => {
                let n = *n;
                self.advance();
                Ok(Value::Int(n))
            }
            Token::Float(f) => {
                let f = *f;
                self.advance();
                Ok(Value::Float(f))
            }
            Token::String(s) => {
                let s = s.clone();
                self.advance();
                Ok(Value::String(s))
            }
            Token::Keyword(k) => {
                let k = k.clone();
                self.advance();
                Ok(Value::Keyword(k))
            }
            Token::Symbol(s) => {
                let s = s.clone();
                self.advance();
                Ok(Value::Symbol(s))
            }
            Token::LBracket => self.parse_vector(),
            Token::LBrace => self.parse_map(),
            Token::HashBrace => self.parse_set(),
            Token::Eof => Err(ParseError::unexpected_eof(self.pos)),
            token => Err(ParseError::unexpected(&token.to_string(), self.pos)),
        }
    }

    /// Parse a vector: [elem1 elem2 ...]
    fn parse_vector(&mut self) -> ParseResult<Value> {
        self.expect_token(Token::LBracket)?;
        let mut elements = Vec::new();

        while !self.check(&Token::RBracket) {
            if self.is_eof() {
                return Err(ParseError::new("unterminated vector", self.pos));
            }
            elements.push(self.parse_value()?);
        }

        self.expect_token(Token::RBracket)?;
        Ok(Value::Vector(elements))
    }

    /// Parse a map: {key1 val1 key2 val2 ...}
    fn parse_map(&mut self) -> ParseResult<Value> {
        self.expect_token(Token::LBrace)?;
        let mut pairs = Vec::new();

        while !self.check(&Token::RBrace) {
            if self.is_eof() {
                return Err(ParseError::new("unterminated map", self.pos));
            }
            let key = self.parse_value()?;
            if self.is_eof() || self.check(&Token::RBrace) {
                return Err(ParseError::new(
                    "map must have even number of elements",
                    self.pos,
                ));
            }
            let value = self.parse_value()?;
            pairs.push((key, value));
        }

        self.expect_token(Token::RBrace)?;
        Ok(Value::Map(pairs))
    }

    /// Parse a set: #{elem1 elem2 ...}
    fn parse_set(&mut self) -> ParseResult<Value> {
        self.expect_token(Token::HashBrace)?;
        let mut elements = Vec::new();

        while !self.check(&Token::RBrace) {
            if self.is_eof() {
                return Err(ParseError::new("unterminated set", self.pos));
            }
            elements.push(self.parse_value()?);
        }

        self.expect_token(Token::RBrace)?;
        Ok(Value::Set(elements))
    }

    // Helper methods

    /// Peek at the current token
    fn peek(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(&Token::Eof)
    }

    /// Advance to the next token, returning the current one
    fn advance(&mut self) -> &Token {
        let token = self.peek();
        if !matches!(token, Token::Eof) {
            self.pos += 1;
        }
        self.tokens.get(self.pos - 1).unwrap_or(&Token::Eof)
    }

    /// Check if the current token matches (without consuming)
    fn check(&self, expected: &Token) -> bool {
        std::mem::discriminant(self.peek()) == std::mem::discriminant(expected)
    }

    /// Expect a specific token type and consume it
    fn expect_token(&mut self, expected: Token) -> ParseResult<()> {
        if self.check(&expected) {
            self.advance();
            Ok(())
        } else {
            Err(ParseError::expected(
                &expected.to_string(),
                &self.peek().to_string(),
                self.pos,
            ))
        }
    }

    /// Check if we're at the end of input
    fn is_eof(&self) -> bool {
        matches!(self.peek(), Token::Eof)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    fn parse(source: &str) -> ParseResult<Value> {
        let tokens = Lexer::new(source).tokenize().unwrap();
        Parser::new(tokens).parse()
    }

    #[test]
    fn test_parse_nil() {
        assert_eq!(parse("nil").unwrap(), Value::Nil);
    }

    #[test]
    fn test_parse_bool() {
        assert_eq!(parse("true").unwrap(), Value::Bool(true));
        assert_eq!(parse("false").unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_parse_int() {
        assert_eq!(parse("42").unwrap(), Value::Int(42));
        assert_eq!(parse("-17").unwrap(), Value::Int(-17));
    }

    #[test]
    fn test_parse_float() {
        assert_eq!(parse("3.5").unwrap(), Value::Float(3.5));
    }

    #[test]
    fn test_parse_string() {
        assert_eq!(
            parse("\"hello\"").unwrap(),
            Value::String("hello".to_string())
        );
    }

    #[test]
    fn test_parse_keyword() {
        assert_eq!(parse(":foo").unwrap(), Value::Keyword("foo".to_string()));
    }

    #[test]
    fn test_parse_symbol() {
        assert_eq!(parse("foo").unwrap(), Value::Symbol("foo".to_string()));
    }

    #[test]
    fn test_parse_vector() {
        assert_eq!(
            parse("[1 2 3]").unwrap(),
            Value::Vector(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
        );
    }

    #[test]
    fn test_parse_empty_vector() {
        assert_eq!(parse("[]").unwrap(), Value::Vector(vec![]));
    }

    #[test]
    fn test_parse_map() {
        assert_eq!(
            parse("{:a 1 :b 2}").unwrap(),
            Value::Map(vec![
                (Value::Keyword("a".to_string()), Value::Int(1)),
                (Value::Keyword("b".to_string()), Value::Int(2)),
            ])
        );
    }

    #[test]
    fn test_parse_empty_map() {
        assert_eq!(parse("{}").unwrap(), Value::Map(vec![]));
    }

    #[test]
    fn test_parse_set() {
        assert_eq!(
            parse("#{1 2 3}").unwrap(),
            Value::Set(vec![Value::Int(1), Value::Int(2), Value::Int(3)])
        );
    }

    #[test]
    fn test_parse_empty_set() {
        assert_eq!(parse("#{}").unwrap(), Value::Set(vec![]));
    }

    #[test]
    fn test_parse_nested() {
        let result = parse("{:project {:name \"my-app\" :version \"0.1.0\"}}").unwrap();
        assert!(matches!(result, Value::Map(_)));

        let name = result.get_in(&["project", "name"]);
        assert_eq!(name, Some(&Value::String("my-app".to_string())));
    }

    #[test]
    fn test_parse_complex() {
        let source = r#"
        {:project {:name "my-app"
                   :version "0.1.0"}
         :paths ["src" "lib"]
         :deps {foo.bar {:path "../foo"}
                http.client {:git "https://example.com" :sha "abc123"}}}
        "#;
        let result = parse(source).unwrap();

        assert_eq!(
            result.get_in(&["project", "name"]),
            Some(&Value::String("my-app".to_string()))
        );

        let paths = result.get_keyword("paths").unwrap();
        assert!(matches!(paths, Value::Vector(_)));
    }

    #[test]
    fn test_unterminated_vector() {
        assert!(parse("[1 2").is_err());
    }

    #[test]
    fn test_unterminated_map() {
        assert!(parse("{:a 1").is_err());
    }

    #[test]
    fn test_odd_map_elements() {
        assert!(parse("{:a}").is_err());
    }
}

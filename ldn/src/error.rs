//! LDN error types
//!
//! Error handling for lexing and parsing.

use std::fmt;

/// LDN error - wraps lexer and parser errors
#[derive(Debug)]
pub enum Error {
    /// Lexer error
    Lex(LexError),
    /// Parser error
    Parse(ParseError),
    /// IO error (for file operations)
    Io(std::io::Error),
}

/// Lexer error with position information
#[derive(Debug, Clone)]
pub struct LexError {
    /// Error message
    pub message: String,
    /// Position in source (byte offset)
    pub pos: usize,
}

/// Parser error with position information
#[derive(Debug, Clone)]
pub struct ParseError {
    /// Error message
    pub message: String,
    /// Token position in token stream
    pub pos: usize,
}

impl LexError {
    /// Create a new lexer error
    pub fn new(message: impl Into<String>, pos: usize) -> Self {
        Self {
            message: message.into(),
            pos,
        }
    }
}

impl ParseError {
    /// Create a new parser error
    pub fn new(message: impl Into<String>, pos: usize) -> Self {
        Self {
            message: message.into(),
            pos,
        }
    }

    /// Create an "unexpected token" error
    pub fn unexpected(found: &str, pos: usize) -> Self {
        Self::new(format!("unexpected token: {}", found), pos)
    }

    /// Create an "unexpected end of input" error
    pub fn unexpected_eof(pos: usize) -> Self {
        Self::new("unexpected end of input", pos)
    }

    /// Create an "expected X" error
    pub fn expected(expected: &str, found: &str, pos: usize) -> Self {
        Self::new(format!("expected {}, found {}", expected, found), pos)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Lex(e) => write!(f, "lexer error at position {}: {}", e.pos, e.message),
            Error::Parse(e) => write!(f, "parse error at position {}: {}", e.pos, e.message),
            Error::Io(e) => write!(f, "io error: {}", e),
        }
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "at position {}: {}", self.pos, self.message)
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "at position {}: {}", self.pos, self.message)
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::Io(e) => Some(e),
            _ => None,
        }
    }
}

impl std::error::Error for LexError {}
impl std::error::Error for ParseError {}

impl From<LexError> for Error {
    fn from(e: LexError) -> Self {
        Error::Lex(e)
    }
}

impl From<ParseError> for Error {
    fn from(e: ParseError) -> Self {
        Error::Parse(e)
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::Io(e)
    }
}

/// Result type alias for LDN operations
pub type Result<T> = std::result::Result<T, Error>;

/// Result type alias for lexer operations
pub type LexResult<T> = std::result::Result<T, LexError>;

/// Result type alias for parser operations
pub type ParseResult<T> = std::result::Result<T, ParseError>;

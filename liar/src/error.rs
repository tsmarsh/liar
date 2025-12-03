//! Compiler error types

use crate::span::Span;

/// A compiler error with source location
#[derive(Debug, Clone)]
pub struct CompileError {
    pub kind: ErrorKind,
    pub span: Span,
    pub message: String,
}

impl CompileError {
    pub fn new(kind: ErrorKind, span: Span, message: impl Into<String>) -> Self {
        Self {
            kind,
            span,
            message: message.into(),
        }
    }

    pub fn lex(span: Span, message: impl Into<String>) -> Self {
        Self::new(ErrorKind::Lex, span, message)
    }

    pub fn parse(span: Span, message: impl Into<String>) -> Self {
        Self::new(ErrorKind::Parse, span, message)
    }

    pub fn resolve(span: Span, message: impl Into<String>) -> Self {
        Self::new(ErrorKind::Resolve, span, message)
    }

    pub fn type_error(span: Span, message: impl Into<String>) -> Self {
        Self::new(ErrorKind::Type, span, message)
    }

    pub fn borrow(span: Span, message: impl Into<String>) -> Self {
        Self::new(ErrorKind::Borrow, span, message)
    }

    pub fn codegen(span: Span, message: impl Into<String>) -> Self {
        Self::new(ErrorKind::Codegen, span, message)
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: {} (at {}..{})",
            self.kind, self.message, self.span.start, self.span.end
        )
    }
}

impl std::error::Error for CompileError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    Lex,
    Parse,
    Resolve,
    Type,
    Borrow,
    Codegen,
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::Lex => write!(f, "lexer error"),
            ErrorKind::Parse => write!(f, "parse error"),
            ErrorKind::Resolve => write!(f, "name resolution error"),
            ErrorKind::Type => write!(f, "type error"),
            ErrorKind::Borrow => write!(f, "borrow error"),
            ErrorKind::Codegen => write!(f, "codegen error"),
        }
    }
}

/// Result type for compiler operations
pub type Result<T> = std::result::Result<T, CompileError>;

/// Collect multiple errors
#[derive(Debug, Default)]
pub struct Errors {
    errors: Vec<CompileError>,
}

impl Errors {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    pub fn push(&mut self, error: CompileError) {
        self.errors.push(error);
    }

    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn len(&self) -> usize {
        self.errors.len()
    }

    pub fn into_result<T>(self, value: T) -> std::result::Result<T, Vec<CompileError>> {
        if self.errors.is_empty() {
            Ok(value)
        } else {
            Err(self.errors)
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &CompileError> {
        self.errors.iter()
    }
}

impl IntoIterator for Errors {
    type Item = CompileError;
    type IntoIter = std::vec::IntoIter<CompileError>;

    fn into_iter(self) -> Self::IntoIter {
        self.errors.into_iter()
    }
}

//! Compiler error types

use crate::span::Span;

/// A compiler error with source location
#[derive(Debug, Clone)]
pub struct CompileError {
    pub kind: ErrorKind,
    pub span: Span,
    pub message: String,
    /// Optional file path where error occurred
    pub file: Option<String>,
    /// Optional source text for formatting
    pub source: Option<String>,
}

impl CompileError {
    pub fn new(kind: ErrorKind, span: Span, message: impl Into<String>) -> Self {
        Self {
            kind,
            span,
            message: message.into(),
            file: None,
            source: None,
        }
    }

    /// Add file path context to error
    pub fn with_file(mut self, file: impl Into<String>) -> Self {
        self.file = Some(file.into());
        self
    }

    /// Add source text context to error
    pub fn with_source(mut self, source: impl Into<String>) -> Self {
        self.source = Some(source.into());
        self
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

    pub fn thread_safety(span: Span, message: impl Into<String>) -> Self {
        Self::new(ErrorKind::ThreadSafety, span, message)
    }

    pub fn macro_error(span: Span, message: impl Into<String>) -> Self {
        Self::new(ErrorKind::Macro, span, message)
    }
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Convert byte offset to line:column if we have source
        let location = if let Some(ref source) = self.source {
            let (line, col) = byte_offset_to_line_col(source, self.span.start);
            format!("{}:{}", line, col)
        } else {
            format!("{}..{}", self.span.start, self.span.end)
        };

        // Include file path if available
        let file_prefix = match &self.file {
            Some(f) => format!("{}:", f),
            None => String::new(),
        };

        write!(
            f,
            "{}{}: {} {}",
            file_prefix, location, self.kind, self.message
        )?;

        // Show source snippet if available
        if let Some(ref source) = self.source {
            let snippet = format_source_snippet(source, self.span.start);
            if !snippet.is_empty() {
                write!(f, "\n{}", snippet)?;
            }
        }

        Ok(())
    }
}

/// Convert byte offset to (line, column), 1-indexed
fn byte_offset_to_line_col(source: &str, offset: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;
    for (i, c) in source.char_indices() {
        if i >= offset {
            break;
        }
        if c == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line, col)
}

/// Format a source snippet with caret pointing to error
fn format_source_snippet(source: &str, offset: usize) -> String {
    // Find the line containing the offset
    let mut line_start = 0;
    let mut line_num = 1;
    for (i, c) in source.char_indices() {
        if i >= offset {
            break;
        }
        if c == '\n' {
            line_start = i + 1;
            line_num += 1;
        }
    }

    // Find the end of this line
    let line_end = source[line_start..]
        .find('\n')
        .map(|i| line_start + i)
        .unwrap_or(source.len());

    let line_text = &source[line_start..line_end];
    let col = offset.saturating_sub(line_start);

    // Format with line number and caret
    let line_num_str = format!("{:>4} | ", line_num);
    let caret_padding = " ".repeat(line_num_str.len() + col);

    format!("{}{}\n{}^", line_num_str, line_text, caret_padding)
}

impl std::error::Error for CompileError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    Lex,
    Parse,
    Resolve,
    Type,
    Borrow,
    ThreadSafety,
    Codegen,
    Macro,
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::Lex => write!(f, "lexer error"),
            ErrorKind::Parse => write!(f, "parse error"),
            ErrorKind::Resolve => write!(f, "name resolution error"),
            ErrorKind::Type => write!(f, "type error"),
            ErrorKind::Borrow => write!(f, "borrow error"),
            ErrorKind::ThreadSafety => write!(f, "thread safety error"),
            ErrorKind::Codegen => write!(f, "codegen error"),
            ErrorKind::Macro => write!(f, "macro expansion error"),
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

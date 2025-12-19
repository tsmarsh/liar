//! Linting for liar source code

use std::collections::HashSet;

use crate::ast::{
    Defmacro, Defprotocol, Defstruct, Defun, ExtendProtocol, ExtendProtocolDefault, Item, Program,
    WhenTarget,
};
use crate::error::Result;
use crate::parser::Parser;
use crate::span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LintKind {
    TrailingWhitespace,
    TabIndentation,
    MissingNamespace,
    DuplicateParam,
    DuplicateField,
    DuplicateProtocolMethod,
}

#[derive(Debug, Clone)]
pub struct LintWarning {
    pub kind: LintKind,
    pub span: Span,
    pub message: String,
}

impl LintWarning {
    pub fn new(kind: LintKind, span: Span, message: impl Into<String>) -> Self {
        Self {
            kind,
            span,
            message: message.into(),
        }
    }
}

impl std::fmt::Display for LintKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LintKind::TrailingWhitespace => write!(f, "trailing-whitespace"),
            LintKind::TabIndentation => write!(f, "tab-indentation"),
            LintKind::MissingNamespace => write!(f, "missing-namespace"),
            LintKind::DuplicateParam => write!(f, "duplicate-param"),
            LintKind::DuplicateField => write!(f, "duplicate-field"),
            LintKind::DuplicateProtocolMethod => write!(f, "duplicate-protocol-method"),
        }
    }
}

pub fn lint_source(source: &str) -> Result<Vec<LintWarning>> {
    let mut warnings = Vec::new();

    lint_trailing_whitespace(source, &mut warnings);
    lint_tabs(source, &mut warnings);

    let mut parser = Parser::new(source)?;
    let program = parser.parse_program()?;

    lint_program(&program, &mut warnings);

    Ok(warnings)
}

fn lint_program(program: &Program, warnings: &mut Vec<LintWarning>) {
    let has_ns = program
        .items
        .iter()
        .any(|item| matches!(item.node, Item::Namespace(_)));
    if !has_ns {
        warnings.push(LintWarning::new(
            LintKind::MissingNamespace,
            Span::new(0, 0),
            "missing (ns ...) declaration",
        ));
    }

    for item in &program.items {
        lint_item(item, warnings);
    }
}

fn lint_item(item: &crate::span::Spanned<Item>, warnings: &mut Vec<LintWarning>) {
    match &item.node {
        Item::Defun(defun) => lint_defun(defun, warnings),
        Item::Defmacro(defmacro) => lint_defmacro(defmacro, warnings),
        Item::Defstruct(defstruct) => lint_defstruct(defstruct, warnings),
        Item::Defprotocol(defprotocol) => lint_defprotocol(defprotocol, warnings),
        Item::ExtendProtocol(extend) => lint_extend_protocol(extend, warnings),
        Item::ExtendProtocolDefault(extend) => lint_extend_protocol_default(extend, warnings),
        Item::WhenTarget(when_target) => lint_when_target(when_target, warnings),
        _ => {}
    }
}

fn lint_defun(defun: &Defun, warnings: &mut Vec<LintWarning>) {
    let mut seen = HashSet::new();
    for param in &defun.params {
        let name = &param.name.node;
        if !seen.insert(name.clone()) {
            warnings.push(LintWarning::new(
                LintKind::DuplicateParam,
                param.name.span,
                format!("duplicate parameter name '{}'", name),
            ));
        }
    }
}

fn lint_defmacro(defmacro: &Defmacro, warnings: &mut Vec<LintWarning>) {
    let mut seen = HashSet::new();
    for param in &defmacro.params {
        let name = &param.node;
        if !seen.insert(name.clone()) {
            warnings.push(LintWarning::new(
                LintKind::DuplicateParam,
                param.span,
                format!("duplicate macro parameter '{}'", name),
            ));
        }
    }
    if let Some(rest) = &defmacro.rest_param {
        let name = &rest.node;
        if !seen.insert(name.clone()) {
            warnings.push(LintWarning::new(
                LintKind::DuplicateParam,
                rest.span,
                format!("duplicate macro parameter '{}'", name),
            ));
        }
    }
}

fn lint_defstruct(defstruct: &Defstruct, warnings: &mut Vec<LintWarning>) {
    let mut seen = HashSet::new();
    for field in &defstruct.fields {
        let name = &field.name.node;
        if !seen.insert(name.clone()) {
            warnings.push(LintWarning::new(
                LintKind::DuplicateField,
                field.name.span,
                format!("duplicate struct field '{}'", name),
            ));
        }
    }
}

fn lint_defprotocol(defprotocol: &Defprotocol, warnings: &mut Vec<LintWarning>) {
    let mut seen = HashSet::new();
    for method in &defprotocol.methods {
        let name = &method.name.node;
        if !seen.insert(name.clone()) {
            warnings.push(LintWarning::new(
                LintKind::DuplicateProtocolMethod,
                method.name.span,
                format!("duplicate protocol method '{}'", name),
            ));
        }
    }
}

fn lint_extend_protocol(extend: &ExtendProtocol, warnings: &mut Vec<LintWarning>) {
    let mut seen = HashSet::new();
    for method in &extend.implementations {
        let name = &method.name.node;
        if !seen.insert(name.clone()) {
            warnings.push(LintWarning::new(
                LintKind::DuplicateProtocolMethod,
                method.name.span,
                format!("duplicate protocol impl method '{}'", name),
            ));
        }
    }
}

fn lint_extend_protocol_default(extend: &ExtendProtocolDefault, warnings: &mut Vec<LintWarning>) {
    let mut seen = HashSet::new();
    for method in &extend.implementations {
        let name = &method.name.node;
        if !seen.insert(name.clone()) {
            warnings.push(LintWarning::new(
                LintKind::DuplicateProtocolMethod,
                method.name.span,
                format!("duplicate protocol default method '{}'", name),
            ));
        }
    }
}

fn lint_when_target(when_target: &WhenTarget, warnings: &mut Vec<LintWarning>) {
    for item in &when_target.items {
        lint_item(item, warnings);
    }
}

fn lint_trailing_whitespace(source: &str, warnings: &mut Vec<LintWarning>) {
    let mut offset = 0;
    for line in source.split_inclusive('\n') {
        let line_end = offset + line.len();
        let line_trimmed = line.trim_end_matches([' ', '\t']);
        if line_trimmed.len() < line.len() {
            let start = offset + line_trimmed.len();
            let end = line_end;
            warnings.push(LintWarning::new(
                LintKind::TrailingWhitespace,
                Span::new(start, end),
                "trailing whitespace",
            ));
        }
        offset = line_end;
    }
}

fn lint_tabs(source: &str, warnings: &mut Vec<LintWarning>) {
    for (idx, ch) in source.char_indices() {
        if ch == '\t' {
            warnings.push(LintWarning::new(
                LintKind::TabIndentation,
                Span::new(idx, idx + 1),
                "tab character used for indentation",
            ));
        }
    }
}

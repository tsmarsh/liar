//! Closure types
//!
//! Core types for closure analysis and conversion.

use crate::resolve::BindingId;
use crate::span::Span;

/// Closure capture mode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CaptureMode {
    /// Capture by value (move)
    Move,
    /// Capture by immutable borrow (closure has lifetime bound)
    Borrow,
    /// Capture by clone (for Copy types or explicit clone)
    Clone,
}

/// Closure "color" for thread safety (ADR-010)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ClosureColor {
    /// No captures, can go anywhere
    #[default]
    Pure,
    /// Captures borrows, cannot escape scope
    Local,
    /// Captures only Send+Sync values, thread-safe
    Sync,
    /// Captures non-Send values, single-threaded only
    NonSync,
}

impl ClosureColor {
    /// Combine two colors (most restrictive wins)
    pub fn combine(self, other: ClosureColor) -> ClosureColor {
        match (self, other) {
            (ClosureColor::Pure, x) | (x, ClosureColor::Pure) => x,
            (ClosureColor::Local, _) | (_, ClosureColor::Local) => ClosureColor::Local,
            (ClosureColor::NonSync, _) | (_, ClosureColor::NonSync) => ClosureColor::NonSync,
            (ClosureColor::Sync, ClosureColor::Sync) => ClosureColor::Sync,
        }
    }

    /// Check if this color is thread-safe (can be used in plet, async, spawn)
    pub fn is_thread_safe(&self) -> bool {
        matches!(self, ClosureColor::Pure | ClosureColor::Sync)
    }

    /// Get a human-readable description of why this color isn't thread-safe
    pub fn thread_safety_reason(&self) -> &'static str {
        match self {
            ClosureColor::Pure => "is thread-safe (no captures)",
            ClosureColor::Sync => "is thread-safe (captures only Send+Sync values)",
            ClosureColor::Local => "captures borrowed references and cannot be used across threads",
            ClosureColor::NonSync => "captures non-Send values and cannot be used across threads",
        }
    }
}

/// Information about a single capture
#[derive(Debug, Clone)]
pub struct Capture {
    /// Name of the captured variable
    pub name: String,
    /// Binding ID (if resolved)
    pub binding: Option<BindingId>,
    /// How the variable is captured
    pub mode: CaptureMode,
    /// Where the capture occurs
    pub span: Span,
}

/// Information about a closure's captures
#[derive(Debug, Clone, Default)]
pub struct CaptureInfo {
    /// All captured variables
    pub captures: Vec<Capture>,
    /// The closure's color
    pub color: ClosureColor,
}

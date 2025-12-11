//! Closure analysis and conversion
//!
//! This module handles closure analysis, thread safety checking, and closure conversion.
//!
//! The closure pipeline:
//! 1. **Analysis** (`analyze`) - Determine what variables each closure captures
//! 2. **Thread Safety** (`analyze_with_thread_safety`) - Verify closures used in parallel
//!    contexts (plet, async, pmap) are thread-safe
//! 3. **Conversion** (`convert`) - Transform lambdas to lifted functions with explicit
//!    environment structs
//!
//! # Example
//!
//! ```ignore
//! use liar::closures;
//!
//! // Analyze captures
//! let capture_info = closures::analyze(&program)?;
//!
//! // Check thread safety (optional, for parallel code)
//! closures::analyze_with_thread_safety(&program)?;
//!
//! // Convert lambdas to lifted functions
//! let converted = closures::convert(program, capture_info)?;
//! ```

mod analysis;
mod conversion;
mod escape;
mod safety;
mod types;

// Re-export public types
pub use types::{Capture, CaptureInfo, CaptureMode, ClosureColor};

// Re-export the main analysis function
pub use analysis::{analyze, ClosureAnalyzer};

// Re-export escape analysis
pub use escape::{EscapeAnalyzer, EscapeInfo, EscapeStatus};

// Re-export safety checker
pub use safety::ThreadSafetyChecker;

// Re-export conversion
pub use conversion::{convert, ClosureConverter};

#[cfg(test)]
pub use conversion::reset_lambda_counter;

use std::collections::HashMap;

use crate::ast::Program;
use crate::error::CompileError;
use crate::span::Span;

/// Full closure analysis with thread safety checking
pub fn analyze_with_thread_safety(
    program: &Program,
) -> std::result::Result<HashMap<Span, CaptureInfo>, Vec<CompileError>> {
    let analyzer = ClosureAnalyzer::new();
    let info = analyzer.analyze(program).map_err(|e| vec![e])?;

    // Check thread safety
    let checker = ThreadSafetyChecker::new(&info);
    checker.check(program)?;

    Ok(info)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Item;
    use crate::parser::Parser;

    fn analyze_source(source: &str) -> HashMap<Span, CaptureInfo> {
        let mut parser = Parser::new(source).expect("lexer failed");
        let program = parser.parse_program().expect("parser failed");
        ClosureAnalyzer::new().analyze(&program).unwrap()
    }

    #[test]
    fn test_no_captures() {
        let info = analyze_source(
            r#"
            (defun foo ()
              (let ((f (fn (x) x)))
                f))
            "#,
        );
        // Should have one closure with no captures
        assert_eq!(info.len(), 1);
        let capture_info = info.values().next().unwrap();
        assert!(capture_info.captures.is_empty());
        assert_eq!(capture_info.color, ClosureColor::Pure);
    }

    #[test]
    fn test_single_capture() {
        let info = analyze_source(
            r#"
            (defun foo ()
              (let ((x 1))
                (let ((f (fn (y) (+ x y))))
                  f)))
            "#,
        );
        assert_eq!(info.len(), 1);
        let capture_info = info.values().next().unwrap();
        assert_eq!(capture_info.captures.len(), 1);
        assert_eq!(capture_info.captures[0].name, "x");
        assert_eq!(capture_info.captures[0].mode, CaptureMode::Move);
    }

    #[test]
    fn test_multiple_captures() {
        let info = analyze_source(
            r#"
            (defun foo ()
              (let ((x 1) (y 2))
                (let ((f (fn (z) (+ x (+ y z)))))
                  f)))
            "#,
        );
        assert_eq!(info.len(), 1);
        let capture_info = info.values().next().unwrap();
        assert_eq!(capture_info.captures.len(), 2);
        let names: std::collections::HashSet<_> =
            capture_info.captures.iter().map(|c| &c.name).collect();
        assert!(names.contains(&"x".to_string()));
        assert!(names.contains(&"y".to_string()));
    }

    #[test]
    fn test_borrow_capture() {
        let info = analyze_source(
            r#"
            (defun foo ()
              (let ((x 1))
                (let ((f (fn (y) (+ (deref (ref x)) y))))
                  f)))
            "#,
        );
        assert_eq!(info.len(), 1);
        let capture_info = info.values().next().unwrap();
        assert_eq!(capture_info.captures.len(), 1);
        assert_eq!(capture_info.captures[0].mode, CaptureMode::Borrow);
        assert_eq!(capture_info.color, ClosureColor::Local);
    }

    #[test]
    fn test_nested_closure() {
        let info = analyze_source(
            r#"
            (defun foo ()
              (let ((x 1))
                (let ((f (fn (y) (fn (z) (+ x (+ y z))))))
                  f)))
            "#,
        );
        // Should have two closures
        assert_eq!(info.len(), 2);
    }

    #[test]
    fn test_closure_color_pure() {
        let info = analyze_source(
            r#"
            (defun foo ()
              (let ((f (fn (x) (+ x 1))))
                f))
            "#,
        );
        let capture_info = info.values().next().unwrap();
        assert_eq!(capture_info.color, ClosureColor::Pure);
    }

    #[test]
    fn test_closure_color_local() {
        let info = analyze_source(
            r#"
            (defun foo ()
              (let ((x 1))
                (let ((f (fn (y) (+ (deref (ref x)) y))))
                  f)))
            "#,
        );
        let capture_info = info.values().next().unwrap();
        assert_eq!(capture_info.color, ClosureColor::Local);
    }

    #[test]
    fn test_does_not_capture_params() {
        let info = analyze_source(
            r#"
            (defun foo (x)
              (let ((f (fn (y) (+ x y))))
                f))
            "#,
        );
        // x is a parameter of foo, so it should be captured by the inner closure
        let capture_info = info.values().next().unwrap();
        assert_eq!(capture_info.captures.len(), 1);
        assert_eq!(capture_info.captures[0].name, "x");
    }

    #[test]
    fn test_builtin_not_captured() {
        let info = analyze_source(
            r#"
            (defun foo ()
              (let ((f (fn (x y) (+ x y))))
                f))
            "#,
        );
        // + is a builtin, should not be captured
        let capture_info = info.values().next().unwrap();
        assert!(capture_info.captures.is_empty());
    }

    fn check_thread_safety(source: &str) -> std::result::Result<(), Vec<CompileError>> {
        let mut parser = Parser::new(source).expect("lexer failed");
        let program = parser.parse_program().expect("parser failed");
        analyze_with_thread_safety(&program).map(|_| ())
    }

    #[test]
    fn test_plet_pure_closure_ok() {
        // Pure closures (no captures) are OK in plet
        let result = check_thread_safety(
            r#"
            (defun foo ()
              (plet ((f (fn (x) (+ x 1))))
                (f 42)))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_plet_sync_closure_ok() {
        // Sync closures (move captures) are OK in plet
        let result = check_thread_safety(
            r#"
            (defun foo ()
              (let ((x 1))
                (plet ((f (fn (y) (+ x y))))
                  (f 42))))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_plet_local_closure_error() {
        // Local closures (borrow captures) are NOT OK in plet
        let result = check_thread_safety(
            r#"
            (defun foo ()
              (let ((x 1))
                (plet ((f (fn (y) (+ (deref (ref x)) y))))
                  (f 42))))
            "#,
        );
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("plet"));
        assert!(errors[0].message.contains("borrowed"));
    }

    #[test]
    fn test_plet_non_atom_mutation_error() {
        // set! on non-atom binding in plet is an error
        let result = check_thread_safety(
            r#"
            (defun foo ()
              (plet ((counter 0))
                (set! counter (+ counter 1))))
            "#,
        );
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("cannot mutate non-atom binding"));
        assert!(errors[0].message.contains("counter"));
    }

    #[test]
    fn test_plet_atom_mutation_ok() {
        // set! via atom operations (swap!, reset!) is OK in plet
        let result = check_thread_safety(
            r#"
            (defun foo ()
              (plet ((counter (atom 0)))
                (reset! counter 1)))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_plet_with_atom_swap_ok() {
        // Using swap! with atoms in plet is OK
        let result = check_thread_safety(
            r#"
            (defun foo ()
              (plet ((counter (atom 0)))
                (swap! counter inc)))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_nested_plet_respects_scopes() {
        // Atom bindings from outer plet should be visible in inner code
        let result = check_thread_safety(
            r#"
            (defun foo ()
              (plet ((counter (atom 0)))
                (do
                  (reset! counter 1)
                  @counter)))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_pmap_pure_closure_ok() {
        // Pure closures (no captures) are OK with pmap
        let result = check_thread_safety(
            r#"
            (defun foo (data)
              (pmap (fn (x) (* x x)) data))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_pmap_sync_closure_ok() {
        // Sync closures (move captures) are OK with pmap
        let result = check_thread_safety(
            r#"
            (defun foo (data)
              (let ((factor 2))
                (pmap (fn (x) (* x factor)) data)))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_pmap_local_closure_error() {
        // Local closures (borrow captures) are NOT OK with pmap
        let result = check_thread_safety(
            r#"
            (defun foo (data)
              (let ((factor 2))
                (pmap (fn (x) (* (deref (ref factor)) x)) data)))
            "#,
        );
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("pmap"));
        assert!(errors[0].message.contains("thread-safe"));
    }

    #[test]
    fn test_pfilter_requires_sync() {
        // pfilter also requires Sync closures
        let result = check_thread_safety(
            r#"
            (defun foo (data)
              (let ((x 1))
                (pfilter (fn (y) (= (deref (ref x)) y)) data)))
            "#,
        );
        assert!(result.is_err());
        assert!(result.unwrap_err()[0].message.contains("pfilter"));
    }

    // ==========================================================================
    // Closure Conversion Tests
    // ==========================================================================

    fn convert_source(source: &str) -> Program {
        reset_lambda_counter();
        let mut parser = Parser::new(source).expect("lexer failed");
        let program = parser.parse_program().expect("parser failed");
        let capture_info = ClosureAnalyzer::new().analyze(&program).unwrap();
        let escape_info = EscapeAnalyzer::new().analyze(&program);
        ClosureConverter::new(capture_info, escape_info)
            .convert(program)
            .expect("conversion failed")
    }

    fn find_defun<'a>(program: &'a Program, name: &str) -> Option<&'a crate::ast::Defun> {
        for item in &program.items {
            if let Item::Defun(defun) = &item.node {
                if defun.name.node == name {
                    return Some(defun);
                }
            }
        }
        None
    }

    fn count_items_of_type(program: &Program, matcher: fn(&Item) -> bool) -> usize {
        program.items.iter().filter(|i| matcher(&i.node)).count()
    }

    #[test]
    fn test_convert_pure_lambda() {
        // A pure lambda (no captures) should become a lifted function with ClosureLit
        let program = convert_source(
            r#"
            (defun foo ()
              (let ((f (fn (x) x)))
                f))
            "#,
        );

        // Should have original defun + generated lambda function
        let defun_count = count_items_of_type(&program, |i| matches!(i, Item::Defun(_)));
        assert_eq!(defun_count, 2, "Expected 2 functions (foo + lifted lambda)");

        // A lifted lambda should exist (name starts with __lambda_)
        let has_lifted = program.items.iter().any(|item| {
            if let Item::Defun(defun) = &item.node {
                defun.name.node.starts_with("__lambda_")
            } else {
                false
            }
        });
        assert!(has_lifted, "Should have generated a lifted lambda function");
    }

    #[test]
    fn test_convert_lambda_with_capture() {
        // A lambda with captures should generate an env struct and ClosureLit with HeapEnvAlloc
        let program = convert_source(
            r#"
            (defun constantly (v)
              (fn (x) v))
            "#,
        );

        // Should have: original defun + generated lambda function
        let defun_count = count_items_of_type(&program, |i| matches!(i, Item::Defun(_)));
        assert_eq!(
            defun_count, 2,
            "Expected 2 functions (constantly + lifted lambda)"
        );

        // Should have generated env struct
        let struct_count = count_items_of_type(&program, |i| matches!(i, Item::Defstruct(_)));
        assert!(struct_count >= 1, "Expected at least 1 struct (__env_N)");

        // Find the lifted lambda (starts with __lambda_)
        let lambda = program
            .items
            .iter()
            .find_map(|item| {
                if let Item::Defun(defun) = &item.node {
                    if defun.name.node.starts_with("__lambda_") {
                        return Some(defun);
                    }
                }
                None
            })
            .expect("Should have a lifted lambda");

        assert!(
            !lambda.params.is_empty(),
            "Lambda should have at least one parameter"
        );
        assert_eq!(
            lambda.params[0].name.node, "__env",
            "First param should be __env"
        );
    }

    #[test]
    fn test_convert_preserves_function_body() {
        // The original function's body should still work (modulo the lambda transformation)
        let program = convert_source(
            r#"
            (defun add (a b) (+ a b))
            "#,
        );

        // Should have exactly one function
        let defun_count = count_items_of_type(&program, |i| matches!(i, Item::Defun(_)));
        assert_eq!(defun_count, 1, "Expected 1 function");

        // The function should be preserved (with __env added as first param)
        let add = find_defun(&program, "add").expect("Should have add function");
        assert_eq!(add.params.len(), 3); // __env, a, b
    }

    #[test]
    fn test_convert_nested_lambdas() {
        // Nested lambdas should each become their own lifted function
        let program = convert_source(
            r#"
            (defun comp (f g)
              (fn (x) (f (g x))))
            "#,
        );

        // Should have: comp + lifted lambda
        let defun_count = count_items_of_type(&program, |i| matches!(i, Item::Defun(_)));
        assert_eq!(defun_count, 2, "Expected 2 functions");

        // Find the lifted lambda
        let lambda = program
            .items
            .iter()
            .find_map(|item| {
                if let Item::Defun(defun) = &item.node {
                    if defun.name.node.starts_with("__lambda_") {
                        return Some(defun);
                    }
                }
                None
            })
            .expect("Should have a lifted lambda");

        // Should have __env param (captures f, g) plus x
        assert!(!lambda.params.is_empty(), "Lambda should have params");
    }
}

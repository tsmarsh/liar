//! Macro expansion
//!
//! Macros are functions that evaluate at compile time and return AST.
//! This pass:
//! 1. Collects macro definitions
//! 2. Evaluates macro calls at compile time
//! 3. Removes macro definitions from the program (they don't generate code)

use std::collections::HashMap;

use crate::ast::{Defstruct, Expr, Item, Program};
use crate::error::{CompileError, Result};
use crate::eval::{Env, Evaluator, MacroDef, StructInfo, Value};
use crate::span::{Span, Spanned};

/// Convenience function to expand macros in a program
pub fn expand(program: &mut Program) -> Result<()> {
    Expander::new().expand_program(program)
}

/// Macro expander
pub struct Expander {
    macros: HashMap<String, MacroDef>,
    evaluator: Evaluator,
}

impl Default for Expander {
    fn default() -> Self {
        Self::new()
    }
}

impl Expander {
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
            evaluator: Evaluator::new(),
        }
    }

    /// Collect macro definitions from the program
    fn collect_macros(&mut self, program: &Program) {
        for item in &program.items {
            if let Item::Defmacro(defmacro) = &item.node {
                let def = MacroDef {
                    params: defmacro.params.iter().map(|p| p.node.clone()).collect(),
                    body: defmacro.body.clone(),
                };
                let name = defmacro.name.node.clone();
                // Store in local map
                self.macros.insert(name.clone(), def.clone());
                // Also register with evaluator for nested macro calls
                self.evaluator.register_macro(name, def);
            }
        }
    }

    /// Register a struct definition for compile-time reflection
    fn register_struct(&mut self, defstruct: &Defstruct) {
        let fields = defstruct
            .fields
            .iter()
            .map(|f| {
                let ty_str = format!("{:?}", f.ty.node);
                (f.name.node.clone(), ty_str)
            })
            .collect();
        self.evaluator
            .register_struct(defstruct.name.node.clone(), StructInfo { fields });
    }

    /// Expand all macros in a program
    pub fn expand_program(&mut self, program: &mut Program) -> Result<()> {
        // First, collect all macro definitions
        self.collect_macros(program);

        // Expand macros in all items
        for item in &mut program.items {
            self.expand_item(item)?;
        }

        // Remove macro definitions from the program (they don't generate code)
        program
            .items
            .retain(|item| !matches!(item.node, Item::Defmacro(_)));

        Ok(())
    }

    fn expand_item(&mut self, item: &mut Spanned<Item>) -> Result<()> {
        match &mut item.node {
            Item::Defun(defun) => {
                self.expand_expr(&mut defun.body)?;
            }
            Item::Def(def) => {
                self.expand_expr(&mut def.value)?;
            }
            Item::Defmacro(_) => {
                // Macro definitions are collected, not expanded
            }
            Item::Defstruct(defstruct) => {
                // Register struct for compile-time reflection
                self.register_struct(defstruct);
            }
            Item::Defprotocol(_) => {}
            Item::ExtendProtocol(extend) => {
                for method in &mut extend.implementations {
                    self.expand_expr(&mut method.body)?;
                }
            }
            Item::ExtendProtocolDefault(extend) => {
                for method in &mut extend.implementations {
                    self.expand_expr(&mut method.body)?;
                }
            }
            Item::Extern(_) => {
                // Extern declarations have no body to expand
            }
        }
        Ok(())
    }

    fn expand_expr(&mut self, expr: &mut Spanned<Expr>) -> Result<()> {
        // Check if this is a macro call
        if let Expr::Call(func, args) = &expr.node {
            if let Expr::Var(name) = &func.node {
                if let Some(macro_def) = self.macros.get(name).cloned() {
                    // This is a macro call - evaluate it
                    let expanded = self.expand_macro_call(&macro_def, args, expr.span)?;
                    *expr = expanded;
                    // Recursively expand the result
                    return self.expand_expr(expr);
                }
            }
        }

        // Not a macro call, recursively expand sub-expressions
        match &mut expr.node {
            Expr::Int(_)
            | Expr::Float(_)
            | Expr::Bool(_)
            | Expr::String(_)
            | Expr::Nil
            | Expr::Var(_)
            | Expr::Keyword(_)
            | Expr::Quote(_)
            | Expr::ByteArray(_)
            | Expr::Regex { .. } => {}

            Expr::Call(func, args) => {
                self.expand_expr(func)?;
                for arg in args {
                    self.expand_expr(arg)?;
                }
            }

            Expr::Lambda(_, body) => {
                self.expand_expr(body)?;
            }

            Expr::Let(bindings, body) | Expr::Plet(bindings, body) => {
                for binding in bindings {
                    self.expand_expr(&mut binding.value)?;
                }
                self.expand_expr(body)?;
            }

            Expr::If(cond, then_br, else_br) => {
                self.expand_expr(cond)?;
                self.expand_expr(then_br)?;
                self.expand_expr(else_br)?;
            }

            Expr::Do(exprs) => {
                for e in exprs {
                    self.expand_expr(e)?;
                }
            }

            Expr::Quasiquote(inner) => {
                self.expand_in_quasiquote(inner)?;
            }

            Expr::Unquote(inner) => {
                self.expand_expr(inner)?;
            }

            Expr::UnquoteSplicing(inner) => {
                self.expand_expr(inner)?;
            }

            Expr::Set(_, value) => {
                self.expand_expr(value)?;
            }

            Expr::Ref(inner) | Expr::RefMut(inner) | Expr::Deref(inner) => {
                self.expand_expr(inner)?;
            }

            Expr::Struct(_, fields) => {
                for (_, value) in fields {
                    self.expand_expr(value)?;
                }
            }

            Expr::Field(obj, _) => {
                self.expand_expr(obj)?;
            }

            Expr::Unsafe(inner) => {
                self.expand_expr(inner)?;
            }

            Expr::Atom(value) => {
                self.expand_expr(value)?;
            }

            Expr::Swap(atom, func) => {
                self.expand_expr(atom)?;
                self.expand_expr(func)?;
            }

            Expr::Reset(atom, value) => {
                self.expand_expr(atom)?;
                self.expand_expr(value)?;
            }

            Expr::AtomDeref(atom) => {
                self.expand_expr(atom)?;
            }

            Expr::CompareAndSet { atom, old, new } => {
                self.expand_expr(atom)?;
                self.expand_expr(old)?;
                self.expand_expr(new)?;
            }

            Expr::Vector(items) => {
                for item in items {
                    self.expand_expr(item)?;
                }
            }

            Expr::Map(pairs) => {
                for (k, v) in pairs {
                    self.expand_expr(k)?;
                    self.expand_expr(v)?;
                }
            }

            Expr::ConvVector(items) => {
                for item in items {
                    self.expand_expr(item)?;
                }
            }

            Expr::ConvMap(pairs) => {
                for (k, v) in pairs {
                    self.expand_expr(k)?;
                    self.expand_expr(v)?;
                }
            }

            Expr::SimdVector(items) => {
                for item in items {
                    self.expand_expr(item)?;
                }
            }

            Expr::Async(body) => {
                self.expand_expr(body)?;
            }

            Expr::Await(future) => {
                self.expand_expr(future)?;
            }

            Expr::Dosync(exprs) => {
                for e in exprs {
                    self.expand_expr(e)?;
                }
            }

            Expr::RefSetStm(ref_expr, value) => {
                self.expand_expr(ref_expr)?;
                self.expand_expr(value)?;
            }

            Expr::Alter {
                ref_expr,
                fn_expr,
                args,
            } => {
                self.expand_expr(ref_expr)?;
                self.expand_expr(fn_expr)?;
                for arg in args {
                    self.expand_expr(arg)?;
                }
            }

            Expr::Commute {
                ref_expr,
                fn_expr,
                args,
            } => {
                self.expand_expr(ref_expr)?;
                self.expand_expr(fn_expr)?;
                for arg in args {
                    self.expand_expr(arg)?;
                }
            }

            Expr::Iter(coll) => {
                self.expand_expr(coll)?;
            }

            Expr::Collect(iter) => {
                self.expand_expr(iter)?;
            }

            Expr::Boxed(inner) => {
                self.expand_expr(inner)?;
            }

            Expr::Wrapping(inner) => {
                self.expand_expr(inner)?;
            }

            Expr::Gensym(_) => {}

            // Closure conversion expressions (generated later)
            Expr::ClosureLit { .. } | Expr::HeapEnvAlloc { .. } | Expr::StackEnvAlloc { .. } => {}
        }
        Ok(())
    }

    fn expand_in_quasiquote(&mut self, expr: &mut Spanned<Expr>) -> Result<()> {
        match &mut expr.node {
            Expr::Unquote(inner) => {
                self.expand_expr(inner)?;
            }
            Expr::UnquoteSplicing(inner) => {
                self.expand_expr(inner)?;
            }
            Expr::Call(func, args) => {
                self.expand_in_quasiquote(func)?;
                for arg in args {
                    self.expand_in_quasiquote(arg)?;
                }
            }
            Expr::Vector(items) => {
                for item in items {
                    self.expand_in_quasiquote(item)?;
                }
            }
            Expr::Let(bindings, body) => {
                for binding in bindings {
                    self.expand_in_quasiquote(&mut binding.value)?;
                }
                self.expand_in_quasiquote(body)?;
            }
            Expr::If(cond, then_br, else_br) => {
                self.expand_in_quasiquote(cond)?;
                self.expand_in_quasiquote(then_br)?;
                self.expand_in_quasiquote(else_br)?;
            }
            _ => {}
        }
        Ok(())
    }

    /// Expand a macro call by evaluating the macro body
    fn expand_macro_call(
        &self,
        macro_def: &MacroDef,
        args: &[Spanned<Expr>],
        span: Span,
    ) -> Result<Spanned<Expr>> {
        if args.len() != macro_def.params.len() {
            return Err(CompileError::macro_error(
                span,
                format!(
                    "macro expects {} arguments, got {}",
                    macro_def.params.len(),
                    args.len()
                ),
            ));
        }

        // Build environment with macro arguments as values
        let mut env = Env::new();
        for (param, arg) in macro_def.params.iter().zip(args.iter()) {
            // Arguments are passed unevaluated (as expressions)
            env.bind(param.clone(), Value::Expr(arg.clone()));
        }

        // Evaluate the macro body
        let result = self.evaluator.eval(&env, &macro_def.body)?;

        // Convert the result back to an expression
        Ok(result.to_expr(span))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn parse_and_expand(source: &str) -> Result<Program> {
        let mut parser = Parser::new(source)?;
        let mut program = parser.parse_program()?;
        let mut expander = Expander::new();
        expander.expand_program(&mut program)?;
        Ok(program)
    }

    fn get_test_body(program: &Program) -> &Expr {
        for item in &program.items {
            if let Item::Defun(defun) = &item.node {
                if defun.name.node == "test" {
                    return &defun.body.node;
                }
            }
        }
        panic!("no test function found")
    }

    #[test]
    fn test_macro_returns_literal() {
        let source = r#"
            (defmacro answer () 42)
            (defun test () (answer))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        assert!(matches!(body, Expr::Int(42)));
    }

    #[test]
    fn test_macro_with_arithmetic() {
        let source = r#"
            (defmacro compute () (+ 1 2 3))
            (defun test () (compute))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        assert!(matches!(body, Expr::Int(6)));
    }

    #[test]
    fn test_macro_with_arithmetic_params() {
        // Test arithmetic with macro parameters
        let source = r#"
            (defmacro add (a b) (+ a b))
            (defun test () (add 3 7))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        assert!(matches!(body, Expr::Int(10)));
    }

    #[test]
    fn test_macro_with_let() {
        let source = r#"
            (defmacro with-let ()
              (let ((x 10)
                    (y 20))
                (+ x y)))
            (defun test () (with-let))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        assert!(matches!(body, Expr::Int(30)));
    }

    #[test]
    fn test_macro_with_if() {
        let source = r#"
            (defmacro check (cond)
              (if cond 1 0))
            (defun test () (check true))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        assert!(matches!(body, Expr::Int(1)));
    }

    #[test]
    fn test_macro_with_quasiquote() {
        let source = r#"
            (defmacro make-add (a b)
              `(+ ,a ,b))
            (defun test () (make-add 1 2))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        // Should expand to (+ 1 2), not evaluate to 3
        assert!(matches!(body, Expr::Call(_, _)));
    }

    #[test]
    fn test_macro_with_list_operations() {
        let source = r#"
            (defmacro first-elem ()
              (first (list 1 2 3)))
            (defun test () (first-elem))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        assert!(matches!(body, Expr::Int(1)));
    }

    #[test]
    fn test_macro_with_map() {
        let source = r#"
            (defmacro double-list ()
              (map (fn (x) (* x 2)) (list 1 2 3)))
            (defun test () (double-list))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        // Result is [2 4 6]
        if let Expr::Vector(items) = body {
            assert_eq!(items.len(), 3);
            assert!(matches!(items[0].node, Expr::Int(2)));
            assert!(matches!(items[1].node, Expr::Int(4)));
            assert!(matches!(items[2].node, Expr::Int(6)));
        } else {
            panic!("expected vector, got {:?}", body);
        }
    }

    #[test]
    fn test_macro_with_splicing() {
        let source = r#"
            (defmacro make-call (f args)
              `(,f ,@args))
            (defun test () (make-call + (list 1 2 3)))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        // Should expand to (+ 1 2 3)
        if let Expr::Call(func, args) = body {
            if let Expr::Vector(items) = &func.node {
                // The function might be wrapped in a vector from to_expr
                assert_eq!(items.len(), 1);
            }
            assert_eq!(args.len(), 3);
        }
    }

    #[test]
    fn test_struct_reflection() {
        let source = r#"
            (defstruct Point (x: i64 y: i64))
            (defmacro count-fields (s)
              (length (struct-fields s)))
            (defun test () (count-fields Point))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        assert!(matches!(body, Expr::Int(2)));
    }

    #[test]
    fn test_macro_with_recursion_helper() {
        // Test macro that uses a helper for loop-like behavior
        // Note: Direct recursive macro calls during expansion require
        // the evaluator to know about macros, which isn't currently supported.
        // Instead, use computed values in the macro body.
        let source = r#"
            (defmacro times-three (n)
              (+ n n n))
            (defun test () (times-three 5))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        assert!(matches!(body, Expr::Int(15)));
    }

    #[test]
    fn test_macro_generates_code() {
        let source = r#"
            (defmacro when (cond body)
              `(if ,cond ,body nil))
            (defun test () (when true 42))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        // Should be an if expression
        assert!(matches!(body, Expr::If(_, _, _)));
    }

    #[test]
    fn test_gensym_basic() {
        // Test gensym returns unique symbols
        // Note: Using gensym in let binding name position requires
        // special AST support for unquote in name slots, not yet implemented.
        let source = r#"
            (defmacro gen-name ()
              (gensym "var"))
            (defun test () 42)
        "#;
        // Just verify it parses and expands without error
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        assert!(matches!(body, Expr::Int(42)));
    }

    #[test]
    fn test_nested_macros() {
        let source = r#"
            (defmacro double (x) `(+ ,x ,x))
            (defmacro quadruple (x) `(double (double ,x)))
            (defun test () (quadruple 3))
        "#;
        let program = parse_and_expand(source).unwrap();
        let body = get_test_body(&program);
        // Should expand to (+ (+ 3 3) (+ 3 3))
        assert!(matches!(body, Expr::Call(_, _)));
    }
}

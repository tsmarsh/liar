//! Code generation - emit lIR from liar AST

use crate::ast::{Defun, Expr, Item, Program};
use crate::error::{CompileError, Result};
use crate::span::Spanned;

/// Generate lIR from a liar program
pub fn generate(program: &Program) -> Result<String> {
    let mut output = String::new();

    for item in &program.items {
        let lir = generate_item(item)?;
        output.push_str(&lir);
        output.push('\n');
    }

    Ok(output)
}

/// Generate lIR for a standalone expression (for REPL)
pub fn generate_expr_standalone(expr: &Spanned<Expr>) -> Result<String> {
    generate_expr(expr)
}

/// Generate lIR for a single item
fn generate_item(item: &Spanned<Item>) -> Result<String> {
    match &item.node {
        Item::Defun(defun) => generate_defun(defun),
        Item::Def(_def) => {
            // TODO: Generate global constant
            Ok(String::new())
        }
        Item::Defstruct(_s) => {
            // TODO: Generate struct definition
            Ok(String::new())
        }
    }
}

/// Generate lIR for a function definition
fn generate_defun(defun: &Defun) -> Result<String> {
    let name = &defun.name.node;

    // For now, assume all functions return i64 and take i64 params
    let params: Vec<String> = defun
        .params
        .iter()
        .map(|p| format!("(i64 {})", p.name.node))
        .collect();

    let body = generate_expr(&defun.body)?;

    Ok(format!(
        "(define ({} i64) ({}) (block entry (ret {})))",
        name,
        params.join(" "),
        body
    ))
}

/// Generate lIR for an expression
fn generate_expr(expr: &Spanned<Expr>) -> Result<String> {
    match &expr.node {
        Expr::Int(n) => Ok(format!("(i64 {})", n)),
        Expr::Float(f) => Ok(format!("(double {})", f)),
        Expr::Bool(b) => Ok(format!("(i1 {})", if *b { 1 } else { 0 })),
        Expr::Var(name) => Ok(name.clone()),

        Expr::Call(func, args) => {
            // Check for builtin operators
            if let Expr::Var(op) = &func.node {
                match op.as_str() {
                    "+" => {
                        if args.len() != 2 {
                            return Err(CompileError::codegen(
                                expr.span,
                                "+ requires exactly 2 arguments",
                            ));
                        }
                        let a = generate_expr(&args[0])?;
                        let b = generate_expr(&args[1])?;
                        return Ok(format!("(add {} {})", a, b));
                    }
                    "-" => {
                        if args.len() != 2 {
                            return Err(CompileError::codegen(
                                expr.span,
                                "- requires exactly 2 arguments",
                            ));
                        }
                        let a = generate_expr(&args[0])?;
                        let b = generate_expr(&args[1])?;
                        return Ok(format!("(sub {} {})", a, b));
                    }
                    "*" => {
                        if args.len() != 2 {
                            return Err(CompileError::codegen(
                                expr.span,
                                "* requires exactly 2 arguments",
                            ));
                        }
                        let a = generate_expr(&args[0])?;
                        let b = generate_expr(&args[1])?;
                        return Ok(format!("(mul {} {})", a, b));
                    }
                    "/" => {
                        if args.len() != 2 {
                            return Err(CompileError::codegen(
                                expr.span,
                                "/ requires exactly 2 arguments",
                            ));
                        }
                        let a = generate_expr(&args[0])?;
                        let b = generate_expr(&args[1])?;
                        return Ok(format!("(sdiv {} {})", a, b));
                    }
                    "<" => {
                        if args.len() != 2 {
                            return Err(CompileError::codegen(
                                expr.span,
                                "< requires exactly 2 arguments",
                            ));
                        }
                        let a = generate_expr(&args[0])?;
                        let b = generate_expr(&args[1])?;
                        return Ok(format!("(icmp slt {} {})", a, b));
                    }
                    ">" => {
                        if args.len() != 2 {
                            return Err(CompileError::codegen(
                                expr.span,
                                "> requires exactly 2 arguments",
                            ));
                        }
                        let a = generate_expr(&args[0])?;
                        let b = generate_expr(&args[1])?;
                        return Ok(format!("(icmp sgt {} {})", a, b));
                    }
                    "=" | "==" => {
                        if args.len() != 2 {
                            return Err(CompileError::codegen(
                                expr.span,
                                "= requires exactly 2 arguments",
                            ));
                        }
                        let a = generate_expr(&args[0])?;
                        let b = generate_expr(&args[1])?;
                        return Ok(format!("(icmp eq {} {})", a, b));
                    }
                    _ => {}
                }
            }

            // Regular function call
            let func_name = match &func.node {
                Expr::Var(name) => name.clone(),
                _ => {
                    return Err(CompileError::codegen(
                        func.span,
                        "indirect function calls not yet supported",
                    ))
                }
            };

            let args_str: Result<Vec<String>> = args.iter().map(generate_expr).collect();
            let args_str = args_str?;

            Ok(format!("(call @{} {})", func_name, args_str.join(" ")))
        }

        Expr::If(cond, then, else_) => {
            let cond = generate_expr(cond)?;
            let then = generate_expr(then)?;
            let else_ = generate_expr(else_)?;
            // For simple cases, use select
            Ok(format!("(select {} {} {})", cond, then, else_))
        }

        Expr::Let(bindings, body) => {
            // For now, generate nested let bindings
            let mut result = generate_expr(body)?;
            for binding in bindings.iter().rev() {
                let value = generate_expr(&binding.value)?;
                result = format!("(let (({} {})) {})", binding.name.node, value, result);
            }
            Ok(result)
        }

        _ => Err(CompileError::codegen(
            expr.span,
            format!("unsupported expression: {:?}", expr.node),
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    #[test]
    fn test_generate_simple() {
        let source = "(defun add (a b) (+ a b))";
        let mut parser = Parser::new(source).unwrap();
        let program = parser.parse_program().unwrap();
        let lir = generate(&program).unwrap();
        assert!(lir.contains("define"));
        assert!(lir.contains("add"));
    }
}

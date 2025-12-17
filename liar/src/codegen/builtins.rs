//! Builtin operations - arithmetic, comparison, boolean, ownership
//!
//! Handles codegen for built-in operators like +, -, *, /, =, <, etc.

use crate::ast::Expr;
use crate::error::{CompileError, Result};
use crate::span::Spanned;
use lir_core::ast as lir;

use super::context::CodegenContext;
use super::expr::generate_expr;

/// Parse a scalar type from an expression (should be a symbol like i8, i64, float, double)
fn parse_scalar_type(expr: &Spanned<Expr>, type_expr: &Spanned<Expr>) -> Result<lir::ScalarType> {
    if let Expr::Var(name) = &type_expr.node {
        match name.name.as_str() {
            "i1" => Ok(lir::ScalarType::I1),
            "i8" => Ok(lir::ScalarType::I8),
            "i16" => Ok(lir::ScalarType::I16),
            "i32" => Ok(lir::ScalarType::I32),
            "i64" => Ok(lir::ScalarType::I64),
            "float" => Ok(lir::ScalarType::Float),
            "double" => Ok(lir::ScalarType::Double),
            _ => Err(CompileError::codegen(
                type_expr.span,
                format!(
                    "unknown type '{}', expected i1/i8/i16/i32/i64/float/double",
                    name
                ),
            )),
        }
    } else {
        Err(CompileError::codegen(
            expr.span,
            "type conversion requires a type name as first argument (e.g., i64, double)",
        ))
    }
}

/// Generate code for builtin operations
/// Returns Some(expr) if the operation is a builtin, None otherwise
pub fn generate_builtin(
    ctx: &mut CodegenContext,
    expr: &Spanned<Expr>,
    op: &str,
    args: &[Spanned<Expr>],
) -> Result<Option<lir::Expr>> {
    fn check_binary(expr: &Spanned<Expr>, op_name: &str, args: &[Spanned<Expr>]) -> Result<()> {
        if args.len() != 2 {
            return Err(CompileError::codegen(
                expr.span,
                format!("{} requires exactly 2 arguments", op_name),
            ));
        }
        Ok(())
    }

    fn check_unary(expr: &Spanned<Expr>, op_name: &str, args: &[Spanned<Expr>]) -> Result<()> {
        if args.len() != 1 {
            return Err(CompileError::codegen(
                expr.span,
                format!("{} requires exactly 1 argument", op_name),
            ));
        }
        Ok(())
    }

    // Arguments to builtins are NOT in tail position - their result is used by the builtin
    let was_tail = ctx.set_tail_position(false);

    let result = match op {
        // Arithmetic
        "+" => {
            check_binary(expr, "+", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::Add(Box::new(a), Box::new(b)))
        }
        "-" => {
            check_binary(expr, "-", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::Sub(Box::new(a), Box::new(b)))
        }
        "*" => {
            check_binary(expr, "*", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::Mul(Box::new(a), Box::new(b)))
        }
        "/" => {
            check_binary(expr, "/", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::SDiv(Box::new(a), Box::new(b)))
        }
        "rem" => {
            check_binary(expr, "rem", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::SRem(Box::new(a), Box::new(b)))
        }

        // Float arithmetic
        "+." | "fadd" => {
            check_binary(expr, "+.", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::FAdd(Box::new(a), Box::new(b)))
        }
        "-." | "fsub" => {
            check_binary(expr, "-.", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::FSub(Box::new(a), Box::new(b)))
        }
        "*." | "fmul" => {
            check_binary(expr, "*.", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::FMul(Box::new(a), Box::new(b)))
        }
        "/." | "fdiv" => {
            check_binary(expr, "/.", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::FDiv(Box::new(a), Box::new(b)))
        }
        "%." | "frem" => {
            check_binary(expr, "%.", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::FRem(Box::new(a), Box::new(b)))
        }

        // Comparison
        "=" | "==" => {
            check_binary(expr, "=", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::ICmp {
                pred: lir::ICmpPred::Eq,
                lhs: Box::new(a),
                rhs: Box::new(b),
            })
        }
        "!=" => {
            check_binary(expr, "!=", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::ICmp {
                pred: lir::ICmpPred::Ne,
                lhs: Box::new(a),
                rhs: Box::new(b),
            })
        }
        "<" => {
            check_binary(expr, "<", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::ICmp {
                pred: lir::ICmpPred::Slt,
                lhs: Box::new(a),
                rhs: Box::new(b),
            })
        }
        ">" => {
            check_binary(expr, ">", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::ICmp {
                pred: lir::ICmpPred::Sgt,
                lhs: Box::new(a),
                rhs: Box::new(b),
            })
        }
        "<=" => {
            check_binary(expr, "<=", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::ICmp {
                pred: lir::ICmpPred::Sle,
                lhs: Box::new(a),
                rhs: Box::new(b),
            })
        }
        ">=" => {
            check_binary(expr, ">=", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::ICmp {
                pred: lir::ICmpPred::Sge,
                lhs: Box::new(a),
                rhs: Box::new(b),
            })
        }

        // Float comparison (ordered - false if either is NaN)
        "=." | "f=" => {
            check_binary(expr, "=.", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::FCmp {
                pred: lir::FCmpPred::Oeq,
                lhs: Box::new(a),
                rhs: Box::new(b),
            })
        }
        "!=." | "f!=" => {
            check_binary(expr, "!=.", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::FCmp {
                pred: lir::FCmpPred::One,
                lhs: Box::new(a),
                rhs: Box::new(b),
            })
        }
        "<." | "f<" => {
            check_binary(expr, "<.", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::FCmp {
                pred: lir::FCmpPred::Olt,
                lhs: Box::new(a),
                rhs: Box::new(b),
            })
        }
        ">." | "f>" => {
            check_binary(expr, ">.", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::FCmp {
                pred: lir::FCmpPred::Ogt,
                lhs: Box::new(a),
                rhs: Box::new(b),
            })
        }
        "<=." | "f<=" => {
            check_binary(expr, "<=.", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::FCmp {
                pred: lir::FCmpPred::Ole,
                lhs: Box::new(a),
                rhs: Box::new(b),
            })
        }
        ">=." | "f>=" => {
            check_binary(expr, ">=.", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::FCmp {
                pred: lir::FCmpPred::Oge,
                lhs: Box::new(a),
                rhs: Box::new(b),
            })
        }

        // Boolean
        "not" => {
            check_unary(expr, "not", args)?;
            let a = generate_expr(ctx, &args[0])?;
            // Ensure operand is i1 - convert non-boolean to bool via != 0
            let a_type = super::types::infer_return_type(ctx, &a);
            let a_bool = if matches!(a_type, lir::ReturnType::Scalar(lir::ScalarType::I1)) {
                a
            } else {
                // Convert to bool: a != 0
                lir::Expr::ICmp {
                    pred: lir::ICmpPred::Ne,
                    lhs: Box::new(a),
                    rhs: Box::new(lir::Expr::IntLit {
                        ty: lir::ScalarType::I64,
                        value: 0,
                    }),
                }
            };
            Some(lir::Expr::Xor(
                Box::new(lir::Expr::IntLit {
                    ty: lir::ScalarType::I1,
                    value: 1,
                }),
                Box::new(a_bool),
            ))
        }
        "and" => {
            check_binary(expr, "and", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::And(Box::new(a), Box::new(b)))
        }
        "or" => {
            check_binary(expr, "or", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::Or(Box::new(a), Box::new(b)))
        }

        // Integer bitwise operations (distinct from boolean and/or)
        "bit-and" => {
            check_binary(expr, "bit-and", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::And(Box::new(a), Box::new(b)))
        }
        "bit-or" => {
            check_binary(expr, "bit-or", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::Or(Box::new(a), Box::new(b)))
        }
        "bit-xor" => {
            check_binary(expr, "bit-xor", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::Xor(Box::new(a), Box::new(b)))
        }
        "bit-not" => {
            check_unary(expr, "bit-not", args)?;
            let a = generate_expr(ctx, &args[0])?;
            // XOR with -1 (all bits set) inverts all bits
            Some(lir::Expr::Xor(
                Box::new(lir::Expr::IntLit {
                    ty: lir::ScalarType::I64,
                    value: -1,
                }),
                Box::new(a),
            ))
        }
        "bit-shift-left" | "shl" => {
            check_binary(expr, "bit-shift-left", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::Shl(Box::new(a), Box::new(b)))
        }
        "bit-shift-right" | "shr" => {
            check_binary(expr, "bit-shift-right", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::LShr(Box::new(a), Box::new(b)))
        }
        "arithmetic-shift-right" | "ashr" => {
            check_binary(expr, "arithmetic-shift-right", args)?;
            let a = generate_expr(ctx, &args[0])?;
            let b = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::AShr(Box::new(a), Box::new(b)))
        }
        "popcount" => {
            check_unary(expr, "popcount", args)?;
            let a = generate_expr(ctx, &args[0])?;
            Some(lir::Expr::Ctpop(Box::new(a)))
        }

        // Ownership operations
        "alloc" => Some(lir::Expr::AllocOwn {
            elem_type: lir::ScalarType::I64,
        }),
        "drop" => {
            check_unary(expr, "drop", args)?;
            let a = generate_expr(ctx, &args[0])?;
            Some(lir::Expr::Drop { value: Box::new(a) })
        }
        "move" => {
            check_unary(expr, "move", args)?;
            let a = generate_expr(ctx, &args[0])?;
            Some(lir::Expr::Move { value: Box::new(a) })
        }

        // Reference counting
        "rc-new" => {
            check_unary(expr, "rc-new", args)?;
            let value = generate_expr(ctx, &args[0])?;
            let rc_var = ctx.fresh_var("rc");
            Some(lir::Expr::Let {
                bindings: vec![(
                    rc_var.clone(),
                    Box::new(lir::Expr::RcAlloc {
                        elem_type: lir::ScalarType::I64,
                    }),
                )],
                body: vec![
                    lir::Expr::Store {
                        value: Box::new(value),
                        ptr: Box::new(lir::Expr::RcPtr {
                            value: Box::new(lir::Expr::LocalRef(rc_var.clone())),
                        }),
                    },
                    lir::Expr::LocalRef(rc_var),
                ],
            })
        }
        "rc-clone" => {
            check_unary(expr, "rc-clone", args)?;
            let a = generate_expr(ctx, &args[0])?;
            Some(lir::Expr::RcClone { value: Box::new(a) })
        }
        "rc-drop" => {
            check_unary(expr, "rc-drop", args)?;
            let a = generate_expr(ctx, &args[0])?;
            Some(lir::Expr::RcDrop { value: Box::new(a) })
        }

        // Share - heap allocate struct and return owned pointer
        // (share (StructName field1 field2 ...)) -> (heap-struct StructName type_id field1 field2 ...)
        // The type_id is stored as the first field for runtime protocol dispatch
        "share" => {
            check_unary(expr, "share", args)?;
            let inner = &args[0];

            // Check if wrapping a struct constructor
            if let crate::ast::Expr::Call(func, struct_args) = &inner.node {
                if let crate::ast::Expr::Var(struct_name) = &func.node {
                    if ctx.lookup_struct(&struct_name.name).is_some() {
                        // Get the type ID for this struct
                        let type_id = ctx.get_struct_type_id(&struct_name.name).unwrap_or(0);

                        // Generate field values, with type_id as the first field
                        let mut field_exprs = vec![lir::Expr::IntLit {
                            ty: lir::ScalarType::I64,
                            value: type_id as i128,
                        }];
                        for arg in struct_args {
                            field_exprs.push(generate_expr(ctx, arg)?);
                        }

                        ctx.set_needs_malloc();
                        return Ok(Some(lir::Expr::HeapStruct {
                            struct_name: struct_name.name.clone(),
                            fields: field_exprs,
                        }));
                    }
                }
            }

            // Not a struct constructor - error for now
            // (Could support primitives later if needed)
            return Err(CompileError::codegen(
                expr.span,
                "share requires a struct constructor argument, e.g., (share (Point 1 2))",
            ));
        }
        "clone" => {
            check_unary(expr, "clone", args)?;
            let a = generate_expr(ctx, &args[0])?;
            Some(lir::Expr::RcClone { value: Box::new(a) })
        }

        // Array operations
        "array" | "make-array" => {
            check_unary(expr, "array", args)?;
            // Extract size from literal if possible
            let size = match &args[0].node {
                Expr::Int(n) => *n as u32,
                _ => 0, // Placeholder for dynamic size
            };
            Some(lir::Expr::ArrayAlloc {
                elem_type: lir::ScalarType::I64,
                size,
            })
        }
        "array-get" | "aget" => {
            if args.len() != 2 {
                return Err(CompileError::codegen(
                    expr.span,
                    "array-get requires 2 arguments (array, index)",
                ));
            }
            let arr = generate_expr(ctx, &args[0])?;
            let idx = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::ArrayGet {
                elem_type: lir::ScalarType::I64,
                size: 0, // Placeholder
                array: Box::new(arr),
                index: Box::new(idx),
            })
        }
        "array-set" | "aset" => {
            if args.len() != 3 {
                return Err(CompileError::codegen(
                    expr.span,
                    "array-set requires 3 arguments (array, index, value)",
                ));
            }
            let arr = generate_expr(ctx, &args[0])?;
            let idx = generate_expr(ctx, &args[1])?;
            let val = generate_expr(ctx, &args[2])?;
            Some(lir::Expr::ArraySet {
                elem_type: lir::ScalarType::I64,
                size: 0,
                array: Box::new(arr),
                index: Box::new(idx),
                value: Box::new(val),
            })
        }
        "array-len" | "alen" => Some(lir::Expr::ArrayLen { size: 0 }),

        // Heap array allocation
        "heap-array" => {
            check_unary(expr, "heap-array", args)?;
            // Check if we have a literal size
            if let Expr::Int(n) = &args[0].node {
                Some(lir::Expr::HeapArray {
                    elem_type: lir::ScalarType::I64,
                    size: *n as u32,
                })
            } else {
                // Dynamic size - use HeapArrayDyn for proper lIR tracking
                let size_expr = generate_expr(ctx, &args[0])?;
                Some(lir::Expr::HeapArrayDyn {
                    elem_type: lir::ScalarType::I64,
                    size: Box::new(size_expr),
                })
            }
        }

        // Array copy (for persistent data structures)
        "array-copy" => {
            if args.len() != 3 {
                return Err(CompileError::codegen(
                    expr.span,
                    "array-copy requires 3 arguments (size, dest, src)",
                ));
            }
            let size = match &args[0].node {
                Expr::Int(n) => *n as u32,
                _ => 0,
            };
            let dest = generate_expr(ctx, &args[1])?;
            let src = generate_expr(ctx, &args[2])?;
            Some(lir::Expr::ArrayCopy {
                elem_type: lir::ScalarType::I64,
                size,
                dest: Box::new(dest),
                src: Box::new(src),
            })
        }

        // Note: array-of and array-set-copy should be implemented in liar
        // using heap-array and array-copy primitives. Example:
        //
        // (defun array-of-3 (a b c)
        //   (let ((arr (heap-array 3)))
        //     (aset arr 0 a)
        //     (aset arr 1 b)
        //     (aset arr 2 c)
        //     arr))
        //
        // (defun array-set-copy (size src idx val)
        //   (let ((new (heap-array size)))
        //     (array-copy size new src)
        //     (aset new idx val)
        //     new))

        // Pointer array operations - for heterogeneous/tagged value storage
        "heap-array-ptr" => {
            check_unary(expr, "heap-array-ptr", args)?;
            if let Expr::Int(n) = &args[0].node {
                Some(lir::Expr::PtrArrayAlloc { size: *n as u32 })
            } else {
                // Dynamic size - use HeapArrayDyn (ptr is 8 bytes, same as i64)
                let size_expr = generate_expr(ctx, &args[0])?;
                Some(lir::Expr::HeapArrayDyn {
                    elem_type: lir::ScalarType::I64,
                    size: Box::new(size_expr),
                })
            }
        }
        "aget-ptr" => {
            if args.len() != 2 {
                return Err(CompileError::codegen(
                    expr.span,
                    "aget-ptr requires 2 arguments (array, index)",
                ));
            }
            let arr = generate_expr(ctx, &args[0])?;
            let idx = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::PtrArrayGet {
                size: 0,
                array: Box::new(arr),
                index: Box::new(idx),
            })
        }
        "aset-ptr" => {
            if args.len() != 3 {
                return Err(CompileError::codegen(
                    expr.span,
                    "aset-ptr requires 3 arguments (array, index, value)",
                ));
            }
            let arr = generate_expr(ctx, &args[0])?;
            let idx = generate_expr(ctx, &args[1])?;
            let val = generate_expr(ctx, &args[2])?;
            Some(lir::Expr::PtrArraySet {
                size: 0,
                array: Box::new(arr),
                index: Box::new(idx),
                value: Box::new(val),
            })
        }

        // Nil check - compares pointer to null
        "nil?" => {
            check_unary(expr, "nil?", args)?;
            let a = generate_expr(ctx, &args[0])?;
            Some(lir::Expr::ICmp {
                pred: lir::ICmpPred::Eq,
                lhs: Box::new(a),
                rhs: Box::new(lir::Expr::NullPtr),
            })
        }

        // I/O operations
        "print" => {
            check_unary(expr, "print", args)?;
            ctx.set_needs_printf();
            let a = generate_expr(ctx, &args[0])?;
            // Use %s for strings (ptr type), %ld for integers
            let format = match &args[0].node {
                Expr::String(_) => "%s",
                _ => "%ld",
            };
            Some(lir::Expr::Call {
                name: "printf".to_string(),
                args: vec![lir::Expr::StringLit(format.to_string()), a],
            })
        }
        "println" => {
            check_unary(expr, "println", args)?;
            ctx.set_needs_printf();
            let a = generate_expr(ctx, &args[0])?;
            // Use %s\n for strings (ptr type), %ld\n for integers
            let format = match &args[0].node {
                Expr::String(_) => "%s\n",
                _ => "%ld\n",
            };
            Some(lir::Expr::Call {
                name: "printf".to_string(),
                args: vec![lir::Expr::StringLit(format.to_string()), a],
            })
        }

        // Integer width conversions
        "trunc" => {
            check_binary(expr, "trunc", args)?;
            let ty = parse_scalar_type(expr, &args[0])?;
            let value = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::Trunc {
                ty,
                value: Box::new(value),
            })
        }
        "zext" => {
            check_binary(expr, "zext", args)?;
            let ty = parse_scalar_type(expr, &args[0])?;
            let value = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::ZExt {
                ty,
                value: Box::new(value),
            })
        }
        "sext" => {
            check_binary(expr, "sext", args)?;
            let ty = parse_scalar_type(expr, &args[0])?;
            let value = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::SExt {
                ty,
                value: Box::new(value),
            })
        }

        // Float precision conversions
        "fptrunc" => {
            check_binary(expr, "fptrunc", args)?;
            let ty = parse_scalar_type(expr, &args[0])?;
            let value = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::FPTrunc {
                ty,
                value: Box::new(value),
            })
        }
        "fpext" => {
            check_binary(expr, "fpext", args)?;
            let ty = parse_scalar_type(expr, &args[0])?;
            let value = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::FPExt {
                ty,
                value: Box::new(value),
            })
        }

        // Float <-> int conversions
        "fptosi" => {
            check_binary(expr, "fptosi", args)?;
            let ty = parse_scalar_type(expr, &args[0])?;
            let value = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::FPToSI {
                ty,
                value: Box::new(value),
            })
        }
        "fptoui" => {
            check_binary(expr, "fptoui", args)?;
            let ty = parse_scalar_type(expr, &args[0])?;
            let value = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::FPToUI {
                ty,
                value: Box::new(value),
            })
        }
        "sitofp" => {
            check_binary(expr, "sitofp", args)?;
            let ty = parse_scalar_type(expr, &args[0])?;
            let value = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::SIToFP {
                ty,
                value: Box::new(value),
            })
        }
        "uitofp" => {
            check_binary(expr, "uitofp", args)?;
            let ty = parse_scalar_type(expr, &args[0])?;
            let value = generate_expr(ctx, &args[1])?;
            Some(lir::Expr::UIToFP {
                ty,
                value: Box::new(value),
            })
        }

        // Byte operations (for string manipulation)
        "store-byte" => {
            // (store-byte ptr value) - store a byte at ptr
            check_binary(expr, "store-byte", args)?;
            let ptr = generate_expr(ctx, &args[0])?;
            let value = generate_expr(ctx, &args[1])?;
            // Truncate i64 value to i8 before storing
            Some(lir::Expr::Store {
                value: Box::new(lir::Expr::Trunc {
                    ty: lir::ScalarType::I8,
                    value: Box::new(value),
                }),
                ptr: Box::new(ptr),
            })
        }
        "load-byte" => {
            // (load-byte ptr) - load a byte from ptr, return as i64
            check_unary(expr, "load-byte", args)?;
            let ptr = generate_expr(ctx, &args[0])?;
            // Load i8 and zero-extend to i64
            Some(lir::Expr::ZExt {
                ty: lir::ScalarType::I64,
                value: Box::new(lir::Expr::Load {
                    ty: lir::ParamType::Scalar(lir::ScalarType::I8),
                    ptr: Box::new(ptr),
                }),
            })
        }
        "ptr+" => {
            // (ptr+ ptr offset) - add byte offset to pointer
            check_binary(expr, "ptr+", args)?;
            let ptr = generate_expr(ctx, &args[0])?;
            let offset = generate_expr(ctx, &args[1])?;
            // GEP on i8 type for byte-level arithmetic
            Some(lir::Expr::GetElementPtr {
                ty: lir::GepType::Scalar(lir::ScalarType::I8),
                ptr: Box::new(ptr),
                indices: vec![offset],
                inbounds: true,
            })
        }

        _ => None,
    };

    // Restore tail position
    ctx.set_tail_position(was_tail);

    Ok(result)
}

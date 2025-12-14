//! Collection and data type codegen
//!
//! Handles vectors, maps, keywords, SIMD vectors, byte arrays,
//! regex, async, STM, and iterator operations.

use crate::ast::Expr;
use crate::error::{CompileError, Result};
use crate::span::Spanned;
use lir_core::ast as lir;

use super::context::CodegenContext;
use super::expr::generate_expr;

/// Generate code for vector literal
/// Emits calls to vector1, vector2, etc. from stdlib
pub fn generate_vector(ctx: &mut CodegenContext, elements: &[Spanned<Expr>]) -> Result<lir::Expr> {
    let n = elements.len();

    // All stdlib functions expect (ptr null) as first arg for __env
    let null_env = lir::Expr::NullPtr;

    if n == 0 {
        // Empty vector: call (vector)
        return Ok(lir::Expr::Call {
            name: "vector".to_string(),
            args: vec![null_env],
        });
    }

    if n <= 8 {
        // Use fixed-arity constructor: vector1, vector2, etc.
        let mut args = vec![null_env];
        for elem in elements {
            args.push(generate_expr(ctx, elem)?);
        }
        return Ok(lir::Expr::Call {
            name: format!("vector{}", n),
            args,
        });
    }

    // For larger vectors, build incrementally with vec-conj
    // Start with vector8 for the first 8 elements
    let mut result = {
        let mut args = vec![lir::Expr::NullPtr];
        for elem in &elements[..8] {
            args.push(generate_expr(ctx, elem)?);
        }
        lir::Expr::Call {
            name: "vector8".to_string(),
            args,
        }
    };

    // Add remaining elements with vec-conj
    for elem in &elements[8..] {
        let elem_code = generate_expr(ctx, elem)?;
        result = lir::Expr::Call {
            name: "vec-conj".to_string(),
            args: vec![lir::Expr::NullPtr, result, elem_code],
        };
    }

    Ok(result)
}

/// Generate code for map literal
/// Emits calls to hashmap1, hashmap2, etc. from stdlib
pub fn generate_map(
    ctx: &mut CodegenContext,
    pairs: &[(Spanned<Expr>, Spanned<Expr>)],
) -> Result<lir::Expr> {
    let n = pairs.len();

    // All stdlib functions expect (ptr null) as first arg for __env
    let null_env = lir::Expr::NullPtr;

    if n == 0 {
        // Empty map: call (hash-map)
        return Ok(lir::Expr::Call {
            name: "hash-map".to_string(),
            args: vec![null_env],
        });
    }

    if n <= 8 {
        // Use fixed-arity constructor: hashmap1, hashmap2, etc.
        let mut args = vec![null_env];
        for (k, v) in pairs {
            args.push(generate_expr(ctx, k)?);
            args.push(generate_expr(ctx, v)?);
        }
        return Ok(lir::Expr::Call {
            name: format!("hashmap{}", n),
            args,
        });
    }

    // For larger maps, build incrementally with hm-assoc
    // Start with hashmap8 for the first 8 pairs
    let mut result = {
        let mut args = vec![lir::Expr::NullPtr];
        for (k, v) in &pairs[..8] {
            args.push(generate_expr(ctx, k)?);
            args.push(generate_expr(ctx, v)?);
        }
        lir::Expr::Call {
            name: "hashmap8".to_string(),
            args,
        }
    };

    // Add remaining pairs with hm-assoc
    for (k, v) in &pairs[8..] {
        let key_code = generate_expr(ctx, k)?;
        let val_code = generate_expr(ctx, v)?;
        result = lir::Expr::Call {
            name: "hm-assoc".to_string(),
            args: vec![lir::Expr::NullPtr, result, key_code, val_code],
        };
    }

    Ok(result)
}

/// Generate code for keyword
pub fn generate_keyword(name: &str) -> lir::Expr {
    lir::Expr::StringLit(format!(":{}", name))
}

/// Generate code for conventional vector (with comma separator)
pub fn generate_conv_vector(
    ctx: &mut CodegenContext,
    elements: &[Spanned<Expr>],
) -> Result<lir::Expr> {
    let elems: Result<Vec<lir::Expr>> = elements.iter().map(|e| generate_expr(ctx, e)).collect();
    Ok(lir::Expr::StructLit(elems?))
}

/// Generate code for conventional map (with comma separator)
pub fn generate_conv_map(
    ctx: &mut CodegenContext,
    pairs: &[(Spanned<Expr>, Spanned<Expr>)],
) -> Result<lir::Expr> {
    let mut lir_pairs = Vec::new();
    for (k, v) in pairs {
        lir_pairs.push(generate_expr(ctx, k)?);
        lir_pairs.push(generate_expr(ctx, v)?);
    }
    Ok(lir::Expr::StructLit(lir_pairs))
}

/// Generate code for SIMD vector literal
pub fn generate_simd_vector(
    ctx: &mut CodegenContext,
    elements: &[Spanned<Expr>],
) -> Result<lir::Expr> {
    let elems: Result<Vec<lir::Expr>> = elements.iter().map(|e| generate_expr(ctx, e)).collect();
    let elems = elems?;
    let n = elements.len() as u32;

    // Infer element type
    let elem_type = if let Some(first) = elements.first() {
        match &first.node {
            Expr::Float(_) => lir::ScalarType::Double,
            _ => lir::ScalarType::I64,
        }
    } else {
        lir::ScalarType::I64
    };

    Ok(lir::Expr::VectorLit {
        ty: lir::VectorType {
            count: n,
            element: elem_type,
        },
        elements: elems,
    })
}

/// Generate code for byte array literal
pub fn generate_byte_array(bytes: &[u8]) -> lir::Expr {
    let elements: Vec<lir::Expr> = bytes
        .iter()
        .map(|b| lir::Expr::IntLit {
            ty: lir::ScalarType::I8,
            value: *b as i128,
        })
        .collect();
    lir::Expr::StructLit(elements)
}

/// Generate code for regex literal
pub fn generate_regex(pattern: &str, flags: &str) -> lir::Expr {
    if flags.is_empty() {
        lir::Expr::StringLit(format!("regex:{}", pattern))
    } else {
        lir::Expr::StringLit(format!("regex:{}:{}", pattern, flags))
    }
}

/// Generate code for async block
pub fn generate_async(ctx: &mut CodegenContext, body: &Spanned<Expr>) -> Result<lir::Expr> {
    // Placeholder - real async needs runtime support
    generate_expr(ctx, body)
}

/// Generate code for await expression
pub fn generate_await(ctx: &mut CodegenContext, future: &Spanned<Expr>) -> Result<lir::Expr> {
    generate_expr(ctx, future)
}

/// Generate code for dosync block (STM)
pub fn generate_dosync(ctx: &mut CodegenContext, exprs: &[Spanned<Expr>]) -> Result<lir::Expr> {
    if exprs.is_empty() {
        return Ok(lir::Expr::NullPtr);
    }
    // Just evaluate the body for now
    if exprs.len() == 1 {
        return generate_expr(ctx, &exprs[0]);
    }
    let mut result = generate_expr(ctx, exprs.last().unwrap())?;
    for (i, e) in exprs.iter().rev().skip(1).enumerate() {
        let value = generate_expr(ctx, e)?;
        result = lir::Expr::Let {
            bindings: vec![(format!("_stm{}", i), Box::new(value))],
            body: vec![result],
        };
    }
    Ok(result)
}

/// Generate code for ref-set (STM ref mutation)
pub fn generate_ref_set_stm(
    ctx: &mut CodegenContext,
    ref_expr: &Spanned<Expr>,
    value: &Spanned<Expr>,
) -> Result<lir::Expr> {
    let ref_code = generate_expr(ctx, ref_expr)?;
    let val_code = generate_expr(ctx, value)?;
    Ok(lir::Expr::Store {
        value: Box::new(val_code),
        ptr: Box::new(ref_code),
    })
}

/// Generate code for alter (STM ref update with function)
pub fn generate_alter(
    ctx: &mut CodegenContext,
    ref_expr: &Spanned<Expr>,
    fn_expr: &Spanned<Expr>,
    args: &[Spanned<Expr>],
) -> Result<lir::Expr> {
    let ref_code = generate_expr(ctx, ref_expr)?;
    let fn_name = match &fn_expr.node {
        Expr::Var(name) => name.clone(),
        _ => {
            return Err(CompileError::codegen(
                fn_expr.span,
                "alter function must be a variable",
            ))
        }
    };
    let mut call_args = vec![lir::Expr::Load {
        ty: lir::ParamType::Scalar(lir::ScalarType::I64),
        ptr: Box::new(ref_code.clone()),
    }];
    for arg in args {
        call_args.push(generate_expr(ctx, arg)?);
    }
    let new_val = lir::Expr::Call {
        name: fn_name,
        args: call_args,
    };
    Ok(lir::Expr::Store {
        value: Box::new(new_val),
        ptr: Box::new(ref_code),
    })
}

/// Generate code for commute (STM commutative update)
pub fn generate_commute(
    ctx: &mut CodegenContext,
    ref_expr: &Spanned<Expr>,
    fn_expr: &Spanned<Expr>,
    args: &[Spanned<Expr>],
) -> Result<lir::Expr> {
    // Same as alter for now
    let ref_code = generate_expr(ctx, ref_expr)?;
    let fn_name = match &fn_expr.node {
        Expr::Var(name) => name.clone(),
        _ => {
            return Err(CompileError::codegen(
                fn_expr.span,
                "commute function must be a variable",
            ))
        }
    };
    let mut call_args = vec![lir::Expr::Load {
        ty: lir::ParamType::Scalar(lir::ScalarType::I64),
        ptr: Box::new(ref_code.clone()),
    }];
    for arg in args {
        call_args.push(generate_expr(ctx, arg)?);
    }
    let new_val = lir::Expr::Call {
        name: fn_name,
        args: call_args,
    };
    Ok(lir::Expr::Store {
        value: Box::new(new_val),
        ptr: Box::new(ref_code),
    })
}

/// Generate code for iter (create iterator)
pub fn generate_iter(ctx: &mut CodegenContext, coll: &Spanned<Expr>) -> Result<lir::Expr> {
    generate_expr(ctx, coll)
}

/// Generate code for collect (consume iterator)
pub fn generate_collect(ctx: &mut CodegenContext, iter: &Spanned<Expr>) -> Result<lir::Expr> {
    generate_expr(ctx, iter)
}

/// Generate code for boxed arithmetic (overflow checking)
pub fn generate_boxed(ctx: &mut CodegenContext, inner: &Spanned<Expr>) -> Result<lir::Expr> {
    // For now, just generate the inner expression
    // Real implementation would check for overflow and promote to bigint
    generate_expr(ctx, inner)
}

/// Generate code for wrapping arithmetic
pub fn generate_wrapping(ctx: &mut CodegenContext, inner: &Spanned<Expr>) -> Result<lir::Expr> {
    // Wrapping arithmetic is the default LLVM behavior
    generate_expr(ctx, inner)
}

/// Generate code for quote
pub fn generate_quote(sym: &str) -> lir::Expr {
    lir::Expr::StringLit(format!("symbol:{}", sym))
}

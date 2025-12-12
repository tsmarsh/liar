//! Struct handling - construction and field access
//!
//! Handles defstruct definitions, struct constructors, and field access via GEP.

use crate::ast::{Defstruct, Expr};
use crate::error::{CompileError, Result};
use crate::span::Spanned;
use lir_core::ast as lir;

use super::context::{CodegenContext, StructInfo};
use super::expr::generate_expr;
use super::types::liar_type_to_lir_param;

/// Generate lIR for a struct definition
/// All structs have __type_id (i64) as field 0 for runtime protocol dispatch
pub fn generate_defstruct(defstruct: &Defstruct) -> Result<lir::StructDef> {
    let name = defstruct.name.node.clone();
    // Prepend type_id field
    let mut fields: Vec<lir::ParamType> = vec![lir::ParamType::Scalar(lir::ScalarType::I64)];
    fields.extend(
        defstruct
            .fields
            .iter()
            .map(|f| liar_type_to_lir_param(&f.ty.node)),
    );
    Ok(lir::StructDef { name, fields })
}

/// Check if an expression is a struct constructor call and return the struct name if so
/// Also handles wrapped constructors like (share (Point 1 2))
pub fn is_struct_constructor_call(ctx: &CodegenContext, expr: &Expr) -> Option<String> {
    match expr {
        // Direct struct constructor: (Point 1 2)
        Expr::Call(func, _args) => {
            if let Expr::Var(name) = &func.node {
                // Check if it's a struct constructor
                if ctx.lookup_struct(name).is_some() {
                    return Some(name.clone());
                }
                // Check if it's share/clone wrapping a struct constructor
                if name == "share" || name == "clone" || name == "rc-new" {
                    if let Some(inner) = _args.first() {
                        return is_struct_constructor_call(ctx, &inner.node);
                    }
                }
            }
            None
        }
        _ => None,
    }
}

/// Generate code for struct constructor: (Point 10 20)
/// Allocates space on stack and stores each field value
/// Field 0 is always __type_id (i64), user fields start at index 1
pub fn generate_struct_constructor(
    ctx: &mut CodegenContext,
    _expr: &Spanned<Expr>,
    struct_name: &str,
    struct_info: &StructInfo,
    args: &[Spanned<Expr>],
) -> Result<lir::Expr> {
    // Generate field values from user args
    let field_values: Result<Vec<lir::Expr>> = args.iter().map(|a| generate_expr(ctx, a)).collect();
    let field_values = field_values?;

    // Create a struct pointer variable name
    let ptr_name = ctx.fresh_var("struct");
    let num_fields = struct_info.fields.len(); // includes __type_id

    // Get type ID for this struct
    let type_id = ctx.get_struct_type_id(struct_name).unwrap_or(0);

    // Build the struct construction:
    // (let ((ptr (alloca i64 (i32 num_fields))))
    //   (store type_id (getelementptr %struct.Point ptr (i32 0) (i32 0)))
    //   (store field0 (getelementptr %struct.Point ptr (i32 0) (i32 1)))
    //   (store field1 (getelementptr %struct.Point ptr (i32 0) (i32 2)))
    //   ptr)

    let mut body_exprs = Vec::new();

    // Store type_id at field 0
    let type_id_gep = lir::Expr::GetElementPtr {
        ty: lir::GepType::Struct(struct_name.to_string()),
        ptr: Box::new(lir::Expr::LocalRef(ptr_name.clone())),
        indices: vec![
            lir::Expr::IntLit {
                ty: lir::ScalarType::I32,
                value: 0,
            },
            lir::Expr::IntLit {
                ty: lir::ScalarType::I32,
                value: 0,
            },
        ],
        inbounds: true,
    };
    body_exprs.push(lir::Expr::Store {
        value: Box::new(lir::Expr::IntLit {
            ty: lir::ScalarType::I64,
            value: type_id as i128,
        }),
        ptr: Box::new(type_id_gep),
    });

    // Store user field values at indices 1..N
    // Skip __type_id (first field in struct_info.fields)
    let user_fields = struct_info.fields.iter().skip(1);
    for (i, ((_field_name, field_ty), value)) in
        user_fields.zip(field_values.into_iter()).enumerate()
    {
        // GEP to get field pointer (index i+1 because field 0 is type_id)
        let gep = lir::Expr::GetElementPtr {
            ty: lir::GepType::Struct(struct_name.to_string()),
            ptr: Box::new(lir::Expr::LocalRef(ptr_name.clone())),
            indices: vec![
                lir::Expr::IntLit {
                    ty: lir::ScalarType::I32,
                    value: 0,
                },
                lir::Expr::IntLit {
                    ty: lir::ScalarType::I32,
                    value: (i + 1) as i128,
                },
            ],
            inbounds: true,
        };

        // Wrap value in typed literal if needed (only for scalar types)
        let typed_value = match (&value, field_ty) {
            (lir::Expr::IntLit { value: v, .. }, lir::ParamType::Scalar(scalar_ty)) => {
                lir::Expr::IntLit {
                    ty: scalar_ty.clone(),
                    value: *v,
                }
            }
            _ => value,
        };

        // Store value at field pointer
        let store = lir::Expr::Store {
            value: Box::new(typed_value),
            ptr: Box::new(gep),
        };
        body_exprs.push(store);
    }

    // Return the struct pointer
    body_exprs.push(lir::Expr::LocalRef(ptr_name.clone()));

    // Wrap in let with alloca for the struct
    Ok(lir::Expr::Let {
        bindings: vec![(
            ptr_name,
            Box::new(lir::Expr::Alloca {
                ty: lir::ParamType::Scalar(lir::ScalarType::I64),
                count: Some(Box::new(lir::Expr::IntLit {
                    ty: lir::ScalarType::I32,
                    value: num_fields as i128,
                })),
            }),
        )],
        body: body_exprs,
    })
}

/// Generate struct fields into a pre-allocated pointer (for heap allocation via share)
/// Returns a sequence of store expressions, not a let binding
#[allow(dead_code)]
pub fn generate_struct_into_ptr(
    ctx: &mut CodegenContext,
    struct_name: &str,
    struct_info: &StructInfo,
    args: &[Spanned<Expr>],
    dest_ptr: lir::Expr,
) -> Result<Vec<lir::Expr>> {
    // Generate field values
    let field_values: Result<Vec<lir::Expr>> = args.iter().map(|a| generate_expr(ctx, a)).collect();
    let field_values = field_values?;

    // Generate field stores into the destination pointer
    let mut stores = Vec::new();
    for (i, ((_field_name, field_ty), value)) in struct_info
        .fields
        .iter()
        .zip(field_values.into_iter())
        .enumerate()
    {
        // GEP to get field pointer
        let gep = lir::Expr::GetElementPtr {
            ty: lir::GepType::Struct(struct_name.to_string()),
            ptr: Box::new(dest_ptr.clone()),
            indices: vec![
                lir::Expr::IntLit {
                    ty: lir::ScalarType::I32,
                    value: 0,
                },
                lir::Expr::IntLit {
                    ty: lir::ScalarType::I32,
                    value: i as i128,
                },
            ],
            inbounds: true,
        };

        // Wrap value in typed literal if needed (only for scalar types)
        let typed_value = match (&value, field_ty) {
            (lir::Expr::IntLit { value: v, .. }, lir::ParamType::Scalar(scalar_ty)) => {
                lir::Expr::IntLit {
                    ty: scalar_ty.clone(),
                    value: *v,
                }
            }
            _ => value,
        };

        // Store value at field pointer
        let store = lir::Expr::Store {
            value: Box::new(typed_value),
            ptr: Box::new(gep),
        };
        stores.push(store);
    }

    Ok(stores)
}

/// Generate code for field access: (. obj field)
pub fn generate_field_access(
    ctx: &mut CodegenContext,
    expr: &Spanned<Expr>,
    obj: &Spanned<Expr>,
    field: &Spanned<String>,
) -> Result<lir::Expr> {
    // Get the struct pointer expression
    let obj_expr = generate_expr(ctx, obj)?;

    // Determine the struct type
    // First try: lookup from variable type tracking
    // Second try: infer from which struct has this field (for ptr-typed variables)
    let struct_name = if let Expr::Var(var_name) = &obj.node {
        if let Some(name) = ctx.lookup_var_struct_type(var_name) {
            name.clone()
        } else {
            // Infer struct type from field name
            ctx.find_struct_with_field(&field.node).ok_or_else(|| {
                CompileError::codegen(
                    expr.span,
                    format!(
                        "cannot determine struct type for variable '{}' (no struct has field '{}')",
                        var_name, field.node
                    ),
                )
            })?
        }
    } else {
        // For non-variable expressions, try to infer from field
        ctx.find_struct_with_field(&field.node).ok_or_else(|| {
            CompileError::codegen(
                expr.span,
                format!(
                    "cannot determine struct type (no struct has field '{}')",
                    field.node
                ),
            )
        })?
    };

    // Look up the struct definition to find the field index
    let struct_info = ctx.lookup_struct(&struct_name).ok_or_else(|| {
        CompileError::codegen(expr.span, format!("unknown struct type '{}'", struct_name))
    })?;

    // Find the field index
    let field_name = &field.node;
    let field_idx = struct_info
        .fields
        .iter()
        .position(|(name, _)| name == field_name)
        .ok_or_else(|| {
            CompileError::codegen(
                field.span,
                format!("struct '{}' has no field '{}'", struct_name, field_name),
            )
        })?;

    // Get the field type
    let field_ty = struct_info.fields[field_idx].1.clone();

    // Generate GEP to get pointer to field
    let field_ptr_var = ctx.fresh_var("field_ptr");
    let gep = lir::Expr::GetElementPtr {
        ty: lir::GepType::Struct(struct_name.clone()),
        ptr: Box::new(obj_expr),
        indices: vec![
            lir::Expr::IntLit {
                ty: lir::ScalarType::I32,
                value: 0,
            },
            lir::Expr::IntLit {
                ty: lir::ScalarType::I32,
                value: field_idx as i128,
            },
        ],
        inbounds: true,
    };

    // Load the value from the field pointer
    Ok(lir::Expr::Let {
        bindings: vec![(field_ptr_var.clone(), Box::new(gep))],
        body: vec![lir::Expr::Load {
            ty: field_ty,
            ptr: Box::new(lir::Expr::LocalRef(field_ptr_var)),
        }],
    })
}

//! Code generation - emit lIR AST from liar AST
//!
//! Transforms the liar AST into lIR AST nodes, which can then be:
//! - Displayed as S-expression strings (for debugging/testing)
//! - Passed directly to LLVM codegen (type-safe, no parsing)

use crate::ast::{Defstruct, Defun, Expr, Item, Program};
use crate::error::{CompileError, Result};
use crate::span::Spanned;
use lir_core::ast as lir;
use lir_core::display::Module;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

// Fresh variable counter for generating unique temporaries
static VAR_COUNTER: AtomicUsize = AtomicUsize::new(0);

// Thread-local function signature table for type inference
thread_local! {
    static FUNC_RETURN_TYPES: std::cell::RefCell<HashMap<String, lir::ReturnType>> =
        std::cell::RefCell::new(HashMap::new());
}

/// Struct field information for codegen
#[derive(Clone, Debug)]
struct StructInfo {
    fields: Vec<(String, lir::ScalarType)>,
}

// Thread-local struct definition table
thread_local! {
    static STRUCT_DEFS: std::cell::RefCell<HashMap<String, StructInfo>> =
        std::cell::RefCell::new(HashMap::new());
}

// Thread-local variable type tracking (variable name -> struct type name)
thread_local! {
    static VAR_STRUCT_TYPES: std::cell::RefCell<HashMap<String, String>> =
        std::cell::RefCell::new(HashMap::new());
}

/// Generate a fresh variable name with the given prefix
fn fresh_var(prefix: &str) -> String {
    let n = VAR_COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("_{}_{}", prefix, n)
}

/// Reset the variable counter (for testing)
pub fn reset_var_counter() {
    VAR_COUNTER.store(0, Ordering::SeqCst);
}

/// Reset the function signature table (for testing)
fn reset_func_table() {
    FUNC_RETURN_TYPES.with(|table| table.borrow_mut().clear());
}

/// Reset the struct definition table
fn reset_struct_table() {
    STRUCT_DEFS.with(|table| table.borrow_mut().clear());
}

/// Register a struct definition
fn register_struct(name: &str, info: StructInfo) {
    STRUCT_DEFS.with(|table| {
        table.borrow_mut().insert(name.to_string(), info);
    });
}

/// Look up a struct definition
fn lookup_struct(name: &str) -> Option<StructInfo> {
    STRUCT_DEFS.with(|table| table.borrow().get(name).cloned())
}

/// Reset the variable struct type table
fn reset_var_struct_types() {
    VAR_STRUCT_TYPES.with(|table| table.borrow_mut().clear());
}

/// Register a variable's struct type
fn register_var_struct_type(var_name: &str, struct_name: &str) {
    VAR_STRUCT_TYPES.with(|table| {
        table
            .borrow_mut()
            .insert(var_name.to_string(), struct_name.to_string());
    });
}

/// Look up a variable's struct type
fn lookup_var_struct_type(var_name: &str) -> Option<String> {
    VAR_STRUCT_TYPES.with(|table| table.borrow().get(var_name).cloned())
}

/// Check if an expression is a struct constructor call and return the struct name if so
fn is_struct_constructor_call(expr: &Expr) -> Option<String> {
    if let Expr::Call(func, _args) = expr {
        if let Expr::Var(name) = &func.node {
            if lookup_struct(name).is_some() {
                return Some(name.clone());
            }
        }
    }
    None
}

/// Register a function's return type
fn register_func_return_type(name: &str, return_type: lir::ReturnType) {
    FUNC_RETURN_TYPES.with(|table| {
        table.borrow_mut().insert(name.to_string(), return_type);
    });
}

/// Look up a function's return type
fn lookup_func_return_type(name: &str) -> Option<lir::ReturnType> {
    FUNC_RETURN_TYPES.with(|table| table.borrow().get(name).cloned())
}

/// Infer the return type of a liar function definition
fn infer_function_return_type(defun: &Defun) -> lir::ReturnType {
    // If explicit return type is given, use it
    if let Some(ref ty) = defun.return_type {
        return liar_type_to_return(&ty.node);
    }
    // Otherwise infer from body
    infer_liar_expr_type(&defun.body.node)
}

/// Infer the return type of a liar expression (before lowering to lIR)
fn infer_liar_expr_type(expr: &Expr) -> lir::ReturnType {
    match expr {
        // Literals
        Expr::Int(_) => lir::ReturnType::Scalar(lir::ScalarType::I64),
        Expr::Float(_) => lir::ReturnType::Scalar(lir::ScalarType::Double),
        Expr::Bool(_) => lir::ReturnType::Scalar(lir::ScalarType::I1),
        Expr::String(_) => lir::ReturnType::Ptr,
        Expr::Nil => lir::ReturnType::Ptr,

        // Comparison operators return bool
        Expr::Call(func, _args) => {
            if let Expr::Var(op) = &func.node {
                match op.as_str() {
                    // Comparisons return i1
                    "=" | "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                        return lir::ReturnType::Scalar(lir::ScalarType::I1);
                    }
                    // Boolean ops return i1
                    "not" | "and" | "or" => {
                        return lir::ReturnType::Scalar(lir::ScalarType::I1);
                    }
                    // Look up user-defined function return types
                    name => {
                        if let Some(ret_type) = lookup_func_return_type(name) {
                            return ret_type;
                        }
                    }
                }
            }
            // Default for unknown calls
            lir::ReturnType::Scalar(lir::ScalarType::I64)
        }

        // If inherits from branches (check true branch)
        Expr::If(_cond, then_branch, _else_branch) => infer_liar_expr_type(&then_branch.node),

        // Let inherits from body
        Expr::Let(_bindings, body) => infer_liar_expr_type(&body.node),
        Expr::Plet(_bindings, body) => infer_liar_expr_type(&body.node),

        // Do block inherits from last expression
        Expr::Do(exprs) => {
            if let Some(last) = exprs.last() {
                infer_liar_expr_type(&last.node)
            } else {
                lir::ReturnType::Scalar(lir::ScalarType::I64)
            }
        }

        // Variable - we don't track variable types, default to i64
        Expr::Var(_) => lir::ReturnType::Scalar(lir::ScalarType::I64),

        // Default
        _ => lir::ReturnType::Scalar(lir::ScalarType::I64),
    }
}

/// Generate lIR module from a liar program
pub fn generate(program: &Program) -> Result<Module> {
    reset_var_counter();
    reset_func_table();
    reset_struct_table();
    reset_var_struct_types();

    // First pass: collect struct definitions
    for item in &program.items {
        if let Item::Defstruct(defstruct) = &item.node {
            let fields: Vec<(String, lir::ScalarType)> = defstruct
                .fields
                .iter()
                .map(|f| {
                    let ty = liar_type_to_scalar(&f.ty.node);
                    (f.name.node.clone(), ty)
                })
                .collect();
            register_struct(&defstruct.name.node, StructInfo { fields });
        }
    }

    // Second pass: collect function signatures for type inference
    for item in &program.items {
        if let Item::Defun(defun) = &item.node {
            let return_type = infer_function_return_type(defun);
            register_func_return_type(&defun.name.node, return_type);
        }
    }

    // Third pass: generate code
    let mut items = Vec::new();
    for item in &program.items {
        if let Some(lir_item) = generate_item(item)? {
            items.push(lir_item);
        }
    }

    Ok(Module { items })
}

/// Generate lIR string from a liar program (convenience wrapper)
pub fn generate_string(program: &Program) -> Result<String> {
    let module = generate(program)?;
    Ok(module.to_string())
}

/// Generate lIR for a standalone expression (for REPL)
pub fn generate_expr_standalone(expr: &Spanned<Expr>) -> Result<String> {
    let lir_expr = generate_expr(expr)?;
    Ok(lir_expr.to_string())
}

/// Generate lIR for a single item
fn generate_item(item: &Spanned<Item>) -> Result<Option<lir::Item>> {
    match &item.node {
        Item::Defun(defun) => Ok(Some(lir::Item::Function(generate_defun(defun)?))),
        Item::Def(_def) => {
            // Global constants need special handling - skip for now
            Ok(None)
        }
        Item::Defstruct(s) => Ok(Some(lir::Item::Struct(generate_defstruct(s)?))),
        Item::Defprotocol(_p) => {
            // Protocols are metadata - skip in lIR output for now
            Ok(None)
        }
        Item::ExtendProtocol(_e) => {
            // Protocol implementations are metadata - skip for now
            Ok(None)
        }
    }
}

/// Generate lIR for a struct definition
fn generate_defstruct(defstruct: &Defstruct) -> Result<lir::StructDef> {
    let name = defstruct.name.node.clone();
    let fields: Vec<lir::ParamType> = defstruct
        .fields
        .iter()
        .map(|f| liar_type_to_lir_param(&f.ty.node))
        .collect();
    Ok(lir::StructDef { name, fields })
}

/// Convert a liar type to lIR ParamType
fn liar_type_to_lir_param(ty: &crate::ast::Type) -> lir::ParamType {
    use crate::ast::Type;
    match ty {
        Type::Named(name) => match name.as_str() {
            "i8" => lir::ParamType::Scalar(lir::ScalarType::I8),
            "i16" => lir::ParamType::Scalar(lir::ScalarType::I16),
            "i32" => lir::ParamType::Scalar(lir::ScalarType::I32),
            "i64" | "int" => lir::ParamType::Scalar(lir::ScalarType::I64),
            "float" | "f32" => lir::ParamType::Scalar(lir::ScalarType::Float),
            "double" | "f64" => lir::ParamType::Scalar(lir::ScalarType::Double),
            "bool" => lir::ParamType::Scalar(lir::ScalarType::I1),
            "ptr" => lir::ParamType::Ptr,
            _ => lir::ParamType::Ptr, // User-defined types are pointers
        },
        Type::Ref(inner) => {
            let inner_scalar = liar_type_to_scalar(inner);
            lir::ParamType::Ref(Box::new(inner_scalar))
        }
        Type::RefMut(inner) => {
            let inner_scalar = liar_type_to_scalar(inner);
            lir::ParamType::RefMut(Box::new(inner_scalar))
        }
        Type::Unit => lir::ParamType::Scalar(lir::ScalarType::Void),
        Type::Fn(_, _) => lir::ParamType::Ptr,
        Type::Tuple(_) => lir::ParamType::Ptr,
    }
}

/// Convert a liar type to lIR ScalarType (for ownership types)
fn liar_type_to_scalar(ty: &crate::ast::Type) -> lir::ScalarType {
    use crate::ast::Type;
    match ty {
        Type::Named(name) => match name.as_str() {
            "i8" => lir::ScalarType::I8,
            "i16" => lir::ScalarType::I16,
            "i32" => lir::ScalarType::I32,
            "i64" | "int" => lir::ScalarType::I64,
            "float" | "f32" => lir::ScalarType::Float,
            "double" | "f64" => lir::ScalarType::Double,
            "bool" => lir::ScalarType::I1,
            _ => lir::ScalarType::I64, // Default
        },
        _ => lir::ScalarType::I64,
    }
}

/// Convert liar type to lIR ReturnType
fn liar_type_to_return(ty: &crate::ast::Type) -> lir::ReturnType {
    use crate::ast::Type;
    match ty {
        Type::Named(name) => match name.as_str() {
            "i8" => lir::ReturnType::Scalar(lir::ScalarType::I8),
            "i16" => lir::ReturnType::Scalar(lir::ScalarType::I16),
            "i32" => lir::ReturnType::Scalar(lir::ScalarType::I32),
            "i64" | "int" => lir::ReturnType::Scalar(lir::ScalarType::I64),
            "float" | "f32" => lir::ReturnType::Scalar(lir::ScalarType::Float),
            "double" | "f64" => lir::ReturnType::Scalar(lir::ScalarType::Double),
            "bool" => lir::ReturnType::Scalar(lir::ScalarType::I1),
            "ptr" => lir::ReturnType::Ptr,
            _ => lir::ReturnType::Ptr,
        },
        Type::Unit => lir::ReturnType::Scalar(lir::ScalarType::Void),
        _ => lir::ReturnType::Scalar(lir::ScalarType::I64),
    }
}

/// Generate lIR for a function definition
fn generate_defun(defun: &Defun) -> Result<lir::FunctionDef> {
    let name = defun.name.node.clone();

    // Generate parameters
    let params: Vec<lir::Param> = defun
        .params
        .iter()
        .map(|p| {
            let ty =
                p.ty.as_ref()
                    .map(|t| liar_type_to_lir_param(&t.node))
                    .unwrap_or(lir::ParamType::Scalar(lir::ScalarType::I64));
            lir::Param {
                ty,
                name: p.name.node.clone(),
            }
        })
        .collect();

    // Generate body expression
    let body_expr = generate_expr(&defun.body)?;

    // Determine return type - use explicit if provided, otherwise infer from body
    let return_type = defun
        .return_type
        .as_ref()
        .map(|t| liar_type_to_return(&t.node))
        .unwrap_or_else(|| infer_return_type(&body_expr));

    // Wrap in ret instruction
    let ret_instr = lir::Expr::Ret(Some(Box::new(body_expr)));

    // Create single entry block
    let entry_block = lir::BasicBlock {
        label: "entry".to_string(),
        instructions: vec![ret_instr],
    };

    Ok(lir::FunctionDef {
        name,
        return_type,
        params,
        blocks: vec![entry_block],
    })
}

/// Infer the return type of an expression
fn infer_return_type(expr: &lir::Expr) -> lir::ReturnType {
    match expr {
        // Literals
        lir::Expr::IntLit { ty, .. } => lir::ReturnType::Scalar(ty.clone()),
        lir::Expr::FloatLit { ty, .. } => lir::ReturnType::Scalar(ty.clone()),
        lir::Expr::NullPtr => lir::ReturnType::Ptr,
        lir::Expr::StringLit(_) => lir::ReturnType::Ptr,

        // Comparisons return i1
        lir::Expr::ICmp { .. } => lir::ReturnType::Scalar(lir::ScalarType::I1),
        lir::Expr::FCmp { .. } => lir::ReturnType::Scalar(lir::ScalarType::I1),

        // Boolean ops return i1
        lir::Expr::And(_, _) | lir::Expr::Or(_, _) | lir::Expr::Xor(_, _) => {
            // Check if it's i1 operations - for now assume i1
            lir::ReturnType::Scalar(lir::ScalarType::I1)
        }

        // Arithmetic inherits from operands (assume i64)
        lir::Expr::Add(a, _)
        | lir::Expr::Sub(a, _)
        | lir::Expr::Mul(a, _)
        | lir::Expr::SDiv(a, _)
        | lir::Expr::UDiv(a, _)
        | lir::Expr::SRem(a, _)
        | lir::Expr::URem(a, _) => infer_return_type(a),

        // Select inherits from its branches
        lir::Expr::Select { true_val, .. } => infer_return_type(true_val),

        // Let returns type of body
        lir::Expr::Let { body, .. } => {
            if let Some(last) = body.last() {
                infer_return_type(last)
            } else {
                lir::ReturnType::Scalar(lir::ScalarType::I64)
            }
        }

        // Call - look up from function table
        lir::Expr::Call { name, .. } => {
            lookup_func_return_type(name).unwrap_or(lir::ReturnType::Scalar(lir::ScalarType::I64))
        }

        // Local ref - we don't track types, default to i64
        lir::Expr::LocalRef(_) => lir::ReturnType::Scalar(lir::ScalarType::I64),

        // Default to i64
        _ => lir::ReturnType::Scalar(lir::ScalarType::I64),
    }
}

/// Generate lIR expression from liar expression
fn generate_expr(expr: &Spanned<Expr>) -> Result<lir::Expr> {
    match &expr.node {
        // Literals
        Expr::Int(n) => Ok(lir::Expr::IntLit {
            ty: lir::ScalarType::I64,
            value: *n as i128,
        }),
        Expr::Float(f) => Ok(lir::Expr::FloatLit {
            ty: lir::ScalarType::Double,
            value: lir::FloatValue::Number(*f),
        }),
        Expr::Bool(b) => Ok(lir::Expr::IntLit {
            ty: lir::ScalarType::I1,
            value: if *b { 1 } else { 0 },
        }),
        Expr::String(s) => Ok(lir::Expr::StringLit(s.clone())),
        Expr::Nil => Ok(lir::Expr::NullPtr),

        // Variables
        Expr::Var(name) => Ok(lir::Expr::LocalRef(name.clone())),

        // Function calls
        Expr::Call(func, args) => generate_call(expr, func, args),

        // Control flow
        Expr::If(cond, then, else_) => {
            let cond_expr = generate_expr(cond)?;
            let then_expr = generate_expr(then)?;
            let else_expr = generate_expr(else_)?;
            Ok(lir::Expr::Select {
                cond: Box::new(cond_expr),
                true_val: Box::new(then_expr),
                false_val: Box::new(else_expr),
            })
        }

        // Let bindings
        Expr::Let(bindings, body) => {
            // First pass: register struct types for bindings that are struct constructor calls
            for binding in bindings.iter() {
                if let Some(struct_name) = is_struct_constructor_call(&binding.value.node) {
                    register_var_struct_type(&binding.name.node, &struct_name);
                }
            }

            let body_expr = generate_expr(body)?;

            // Build let chain from inside out
            let mut result = body_expr;
            for binding in bindings.iter().rev() {
                let value = generate_expr(&binding.value)?;
                result = lir::Expr::Let {
                    bindings: vec![(binding.name.node.clone(), Box::new(value))],
                    body: vec![result],
                };
            }
            Ok(result)
        }

        // Parallel let (same as let for codegen)
        Expr::Plet(bindings, body) => {
            // First pass: register struct types for bindings that are struct constructor calls
            for binding in bindings.iter() {
                if let Some(struct_name) = is_struct_constructor_call(&binding.value.node) {
                    register_var_struct_type(&binding.name.node, &struct_name);
                }
            }

            let body_expr = generate_expr(body)?;
            let mut result = body_expr;
            for binding in bindings.iter().rev() {
                let value = generate_expr(&binding.value)?;
                result = lir::Expr::Let {
                    bindings: vec![(binding.name.node.clone(), Box::new(value))],
                    body: vec![result],
                };
            }
            Ok(result)
        }

        // Do block - sequence of expressions
        Expr::Do(exprs) => {
            if exprs.is_empty() {
                return Ok(lir::Expr::IntLit {
                    ty: lir::ScalarType::I64,
                    value: 0,
                });
            }
            if exprs.len() == 1 {
                return generate_expr(&exprs[0]);
            }

            // Generate as nested lets with dummy bindings
            let mut result = generate_expr(exprs.last().unwrap())?;
            for (i, e) in exprs.iter().rev().skip(1).enumerate() {
                let value = generate_expr(e)?;
                result = lir::Expr::Let {
                    bindings: vec![(format!("_discard{}", i), Box::new(value))],
                    body: vec![result],
                };
            }
            Ok(result)
        }

        // Lambdas
        Expr::Lambda(_params, _body) => Err(CompileError::codegen(
            expr.span,
            "lambdas require closure analysis (not yet implemented)",
        )),

        // Mutation
        Expr::Set(name, value) => {
            let value_expr = generate_expr(value)?;
            Ok(lir::Expr::Store {
                value: Box::new(value_expr),
                ptr: Box::new(lir::Expr::LocalRef(name.node.clone())),
            })
        }

        // References
        Expr::Ref(inner) => {
            let inner_expr = generate_expr(inner)?;
            Ok(lir::Expr::BorrowRef {
                value: Box::new(inner_expr),
            })
        }
        Expr::RefMut(inner) => {
            let inner_expr = generate_expr(inner)?;
            Ok(lir::Expr::BorrowRefMut {
                value: Box::new(inner_expr),
            })
        }
        Expr::Deref(inner) => {
            let inner_expr = generate_expr(inner)?;
            Ok(lir::Expr::Load {
                ty: lir::ParamType::Scalar(lir::ScalarType::I64),
                ptr: Box::new(inner_expr),
            })
        }

        // Structs
        Expr::Struct(name, fields) => {
            let field_values: Result<Vec<lir::Expr>> =
                fields.iter().map(|(_, v)| generate_expr(v)).collect();
            let _field_values = field_values?;
            // For now, return a placeholder - real struct handling needs GEP
            Ok(lir::Expr::StringLit(format!("struct:{}", name)))
        }
        Expr::Field(obj, field) => {
            // Get the struct pointer expression
            let obj_expr = generate_expr(obj)?;

            // Determine the struct type - the object should be a variable bound to a struct
            let struct_name = if let Expr::Var(var_name) = &obj.node {
                lookup_var_struct_type(var_name).ok_or_else(|| {
                    CompileError::codegen(
                        expr.span,
                        format!("cannot determine struct type for variable '{}'", var_name),
                    )
                })?
            } else {
                return Err(CompileError::codegen(
                    expr.span,
                    "field access requires a variable (complex expressions not yet supported)",
                ));
            };

            // Look up the struct definition to find the field index
            let struct_info = lookup_struct(&struct_name).ok_or_else(|| {
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
            let field_ptr_var = fresh_var("field_ptr");
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
                    ty: lir::ParamType::Scalar(field_ty),
                    ptr: Box::new(lir::Expr::LocalRef(field_ptr_var)),
                }],
            })
        }

        // Match
        Expr::Match(_scrutinee, _arms) => Err(CompileError::codegen(
            expr.span,
            "match requires control flow codegen (not yet implemented)",
        )),

        // Quote
        Expr::Quote(sym) => Ok(lir::Expr::StringLit(format!("symbol:{}", sym))),

        // Unsafe
        Expr::Unsafe(inner) => generate_expr(inner),

        // Atoms (ADR-011)
        Expr::Atom(value) => {
            let value_expr = generate_expr(value)?;
            let atom_var = fresh_var("atom");

            Ok(lir::Expr::Let {
                bindings: vec![(
                    atom_var.clone(),
                    Box::new(lir::Expr::RcAlloc {
                        elem_type: lir::ScalarType::I64,
                    }),
                )],
                body: vec![
                    lir::Expr::AtomicStore {
                        ordering: lir::MemoryOrdering::SeqCst,
                        value: Box::new(value_expr),
                        ptr: Box::new(lir::Expr::RcPtr {
                            value: Box::new(lir::Expr::LocalRef(atom_var.clone())),
                        }),
                    },
                    lir::Expr::LocalRef(atom_var),
                ],
            })
        }

        Expr::AtomDeref(atom) => {
            let atom_expr = generate_expr(atom)?;
            Ok(lir::Expr::AtomicLoad {
                ordering: lir::MemoryOrdering::SeqCst,
                ty: lir::ScalarType::I64,
                ptr: Box::new(lir::Expr::RcPtr {
                    value: Box::new(atom_expr),
                }),
            })
        }

        Expr::Reset(atom, value) => {
            let atom_expr = generate_expr(atom)?;
            let value_expr = generate_expr(value)?;
            let new_var = fresh_var("new");

            Ok(lir::Expr::Let {
                bindings: vec![(new_var.clone(), Box::new(value_expr))],
                body: vec![
                    lir::Expr::AtomicStore {
                        ordering: lir::MemoryOrdering::SeqCst,
                        value: Box::new(lir::Expr::LocalRef(new_var.clone())),
                        ptr: Box::new(lir::Expr::RcPtr {
                            value: Box::new(atom_expr),
                        }),
                    },
                    lir::Expr::LocalRef(new_var),
                ],
            })
        }

        Expr::Swap(atom, func) => {
            let atom_expr = generate_expr(atom)?;
            let func_name = match &func.node {
                Expr::Var(name) => name.clone(),
                _ => {
                    return Err(CompileError::codegen(
                        func.span,
                        "swap! function must be a variable",
                    ))
                }
            };

            let ptr_var = fresh_var("ptr");
            let old_var = fresh_var("old");
            let new_var = fresh_var("new");

            Ok(lir::Expr::Let {
                bindings: vec![(
                    ptr_var.clone(),
                    Box::new(lir::Expr::RcPtr {
                        value: Box::new(atom_expr),
                    }),
                )],
                body: vec![lir::Expr::Let {
                    bindings: vec![(
                        old_var.clone(),
                        Box::new(lir::Expr::AtomicLoad {
                            ordering: lir::MemoryOrdering::SeqCst,
                            ty: lir::ScalarType::I64,
                            ptr: Box::new(lir::Expr::LocalRef(ptr_var.clone())),
                        }),
                    )],
                    body: vec![lir::Expr::Let {
                        bindings: vec![(
                            new_var.clone(),
                            Box::new(lir::Expr::Call {
                                name: func_name,
                                args: vec![lir::Expr::LocalRef(old_var)],
                            }),
                        )],
                        body: vec![
                            lir::Expr::AtomicStore {
                                ordering: lir::MemoryOrdering::SeqCst,
                                value: Box::new(lir::Expr::LocalRef(new_var.clone())),
                                ptr: Box::new(lir::Expr::LocalRef(ptr_var)),
                            },
                            lir::Expr::LocalRef(new_var),
                        ],
                    }],
                }],
            })
        }

        Expr::CompareAndSet { atom, old, new } => {
            let atom_expr = generate_expr(atom)?;
            let old_expr = generate_expr(old)?;
            let new_expr = generate_expr(new)?;
            let result_var = fresh_var("result");

            Ok(lir::Expr::Let {
                bindings: vec![(
                    result_var.clone(),
                    Box::new(lir::Expr::CmpXchg {
                        ordering: lir::MemoryOrdering::SeqCst,
                        ptr: Box::new(lir::Expr::RcPtr {
                            value: Box::new(atom_expr),
                        }),
                        expected: Box::new(old_expr),
                        new_value: Box::new(new_expr),
                    }),
                )],
                body: vec![lir::Expr::ExtractValue {
                    aggregate: Box::new(lir::Expr::LocalRef(result_var)),
                    indices: vec![1],
                }],
            })
        }

        // Collections (stubs for now - these need runtime support)
        Expr::Vector(elements) => {
            let elems: Result<Vec<lir::Expr>> = elements.iter().map(generate_expr).collect();
            let elems = elems?;
            // Placeholder - real implementation needs persistent vector runtime
            Ok(lir::Expr::StructLit(elems))
        }

        Expr::Map(pairs) => {
            let mut lir_pairs = Vec::new();
            for (k, v) in pairs {
                lir_pairs.push(generate_expr(k)?);
                lir_pairs.push(generate_expr(v)?);
            }
            Ok(lir::Expr::StructLit(lir_pairs))
        }

        Expr::Keyword(name) => Ok(lir::Expr::StringLit(format!(":{}", name))),

        Expr::ConvVector(elements) => {
            let elems: Result<Vec<lir::Expr>> = elements.iter().map(generate_expr).collect();
            Ok(lir::Expr::StructLit(elems?))
        }

        Expr::ConvMap(pairs) => {
            let mut lir_pairs = Vec::new();
            for (k, v) in pairs {
                lir_pairs.push(generate_expr(k)?);
                lir_pairs.push(generate_expr(v)?);
            }
            Ok(lir::Expr::StructLit(lir_pairs))
        }

        // Async (stubs)
        Expr::Async(body) => {
            let body_expr = generate_expr(body)?;
            // Placeholder - real async needs runtime support
            Ok(body_expr)
        }
        Expr::Await(future) => generate_expr(future),

        // SIMD vectors
        Expr::SimdVector(elements) => {
            let elems: Result<Vec<lir::Expr>> = elements.iter().map(generate_expr).collect();
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

        // STM (stubs)
        Expr::Dosync(exprs) => {
            if exprs.is_empty() {
                return Ok(lir::Expr::NullPtr);
            }
            // Just evaluate the body for now
            if exprs.len() == 1 {
                return generate_expr(&exprs[0]);
            }
            let mut result = generate_expr(exprs.last().unwrap())?;
            for (i, e) in exprs.iter().rev().skip(1).enumerate() {
                let value = generate_expr(e)?;
                result = lir::Expr::Let {
                    bindings: vec![(format!("_stm{}", i), Box::new(value))],
                    body: vec![result],
                };
            }
            Ok(result)
        }

        Expr::RefSetStm(ref_expr, value) => {
            let ref_code = generate_expr(ref_expr)?;
            let val_code = generate_expr(value)?;
            Ok(lir::Expr::Store {
                value: Box::new(val_code),
                ptr: Box::new(ref_code),
            })
        }

        Expr::Alter {
            ref_expr,
            fn_expr,
            args,
        } => {
            let ref_code = generate_expr(ref_expr)?;
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
                call_args.push(generate_expr(arg)?);
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

        Expr::Commute {
            ref_expr,
            fn_expr,
            args,
        } => {
            // Same as alter for now
            let ref_code = generate_expr(ref_expr)?;
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
                call_args.push(generate_expr(arg)?);
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

        // Iterators (stubs)
        Expr::Iter(coll) => generate_expr(coll),
        Expr::Collect(iter) => generate_expr(iter),

        // Byte arrays
        Expr::ByteArray(bytes) => {
            let elements: Vec<lir::Expr> = bytes
                .iter()
                .map(|b| lir::Expr::IntLit {
                    ty: lir::ScalarType::I8,
                    value: *b as i128,
                })
                .collect();
            Ok(lir::Expr::StructLit(elements))
        }

        // Regex
        Expr::Regex { pattern, flags } => {
            // Store as string literal for now
            if flags.is_empty() {
                Ok(lir::Expr::StringLit(format!("regex:{}", pattern)))
            } else {
                Ok(lir::Expr::StringLit(format!("regex:{}:{}", pattern, flags)))
            }
        }

        // Overflow handling
        Expr::Boxed(inner) => {
            // For now, just generate the inner expression
            // Real implementation would check for overflow and promote to bigint
            generate_expr(inner)
        }
        Expr::Wrapping(inner) => {
            // Wrapping arithmetic is the default LLVM behavior
            generate_expr(inner)
        }
    }
}

/// Generate code for a function call (handles builtins and struct constructors)
fn generate_call(
    expr: &Spanned<Expr>,
    func: &Spanned<Expr>,
    args: &[Spanned<Expr>],
) -> Result<lir::Expr> {
    // Check for builtin operators
    if let Expr::Var(op) = &func.node {
        if let Some(result) = generate_builtin(expr, op, args)? {
            return Ok(result);
        }

        // Check for struct constructor
        if let Some(struct_info) = lookup_struct(op) {
            return generate_struct_constructor(expr, op, &struct_info, args);
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

    let call_args: Result<Vec<lir::Expr>> = args.iter().map(generate_expr).collect();

    Ok(lir::Expr::Call {
        name: func_name,
        args: call_args?,
    })
}

/// Generate code for struct constructor: (Point 10 20)
/// Allocates space on stack and stores each field value
fn generate_struct_constructor(
    _expr: &Spanned<Expr>,
    struct_name: &str,
    struct_info: &StructInfo,
    args: &[Spanned<Expr>],
) -> Result<lir::Expr> {
    // Generate field values
    let field_values: Result<Vec<lir::Expr>> = args.iter().map(generate_expr).collect();
    let field_values = field_values?;

    // Create a struct pointer variable name
    let ptr_name = fresh_var("struct");
    let num_fields = struct_info.fields.len();

    // Build the struct construction:
    // (let ((ptr (alloca i64 (i32 num_fields))))
    //   (store field0 (getelementptr %struct.Point ptr (i32 0) (i32 0)))
    //   (store field1 (getelementptr %struct.Point ptr (i32 0) (i32 1)))
    //   ptr)

    // Generate field stores
    let mut body_exprs = Vec::new();
    for (i, ((_field_name, field_ty), value)) in struct_info
        .fields
        .iter()
        .zip(field_values.into_iter())
        .enumerate()
    {
        // GEP to get field pointer
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
                    value: i as i128,
                },
            ],
            inbounds: true,
        };

        // Wrap value in typed literal if needed
        let typed_value = match value {
            lir::Expr::IntLit { value: v, .. } => lir::Expr::IntLit {
                ty: field_ty.clone(),
                value: v,
            },
            other => other,
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
    // Allocate enough space for all fields (use i64 as the element type, with count = num_fields)
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

/// Generate code for builtin operations
fn generate_builtin(
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

    let result = match op {
        // Arithmetic
        "+" => {
            check_binary(expr, "+", args)?;
            let a = generate_expr(&args[0])?;
            let b = generate_expr(&args[1])?;
            Some(lir::Expr::Add(Box::new(a), Box::new(b)))
        }
        "-" => {
            check_binary(expr, "-", args)?;
            let a = generate_expr(&args[0])?;
            let b = generate_expr(&args[1])?;
            Some(lir::Expr::Sub(Box::new(a), Box::new(b)))
        }
        "*" => {
            check_binary(expr, "*", args)?;
            let a = generate_expr(&args[0])?;
            let b = generate_expr(&args[1])?;
            Some(lir::Expr::Mul(Box::new(a), Box::new(b)))
        }
        "/" => {
            check_binary(expr, "/", args)?;
            let a = generate_expr(&args[0])?;
            let b = generate_expr(&args[1])?;
            Some(lir::Expr::SDiv(Box::new(a), Box::new(b)))
        }
        "rem" => {
            check_binary(expr, "rem", args)?;
            let a = generate_expr(&args[0])?;
            let b = generate_expr(&args[1])?;
            Some(lir::Expr::SRem(Box::new(a), Box::new(b)))
        }

        // Comparison
        "=" | "==" => {
            check_binary(expr, "=", args)?;
            let a = generate_expr(&args[0])?;
            let b = generate_expr(&args[1])?;
            Some(lir::Expr::ICmp {
                pred: lir::ICmpPred::Eq,
                lhs: Box::new(a),
                rhs: Box::new(b),
            })
        }
        "!=" => {
            check_binary(expr, "!=", args)?;
            let a = generate_expr(&args[0])?;
            let b = generate_expr(&args[1])?;
            Some(lir::Expr::ICmp {
                pred: lir::ICmpPred::Ne,
                lhs: Box::new(a),
                rhs: Box::new(b),
            })
        }
        "<" => {
            check_binary(expr, "<", args)?;
            let a = generate_expr(&args[0])?;
            let b = generate_expr(&args[1])?;
            Some(lir::Expr::ICmp {
                pred: lir::ICmpPred::Slt,
                lhs: Box::new(a),
                rhs: Box::new(b),
            })
        }
        ">" => {
            check_binary(expr, ">", args)?;
            let a = generate_expr(&args[0])?;
            let b = generate_expr(&args[1])?;
            Some(lir::Expr::ICmp {
                pred: lir::ICmpPred::Sgt,
                lhs: Box::new(a),
                rhs: Box::new(b),
            })
        }
        "<=" => {
            check_binary(expr, "<=", args)?;
            let a = generate_expr(&args[0])?;
            let b = generate_expr(&args[1])?;
            Some(lir::Expr::ICmp {
                pred: lir::ICmpPred::Sle,
                lhs: Box::new(a),
                rhs: Box::new(b),
            })
        }
        ">=" => {
            check_binary(expr, ">=", args)?;
            let a = generate_expr(&args[0])?;
            let b = generate_expr(&args[1])?;
            Some(lir::Expr::ICmp {
                pred: lir::ICmpPred::Sge,
                lhs: Box::new(a),
                rhs: Box::new(b),
            })
        }

        // Boolean
        "not" => {
            check_unary(expr, "not", args)?;
            let a = generate_expr(&args[0])?;
            Some(lir::Expr::Xor(
                Box::new(lir::Expr::IntLit {
                    ty: lir::ScalarType::I1,
                    value: 1,
                }),
                Box::new(a),
            ))
        }
        "and" => {
            check_binary(expr, "and", args)?;
            let a = generate_expr(&args[0])?;
            let b = generate_expr(&args[1])?;
            Some(lir::Expr::And(Box::new(a), Box::new(b)))
        }
        "or" => {
            check_binary(expr, "or", args)?;
            let a = generate_expr(&args[0])?;
            let b = generate_expr(&args[1])?;
            Some(lir::Expr::Or(Box::new(a), Box::new(b)))
        }

        // Ownership operations
        "alloc" => Some(lir::Expr::AllocOwn {
            elem_type: lir::ScalarType::I64,
        }),
        "drop" => {
            check_unary(expr, "drop", args)?;
            let a = generate_expr(&args[0])?;
            Some(lir::Expr::Drop { value: Box::new(a) })
        }
        "move" => {
            check_unary(expr, "move", args)?;
            let a = generate_expr(&args[0])?;
            Some(lir::Expr::Move { value: Box::new(a) })
        }

        // Reference counting
        "rc-new" => {
            check_unary(expr, "rc-new", args)?;
            let value = generate_expr(&args[0])?;
            let rc_var = fresh_var("rc");
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
            let a = generate_expr(&args[0])?;
            Some(lir::Expr::RcClone { value: Box::new(a) })
        }
        "rc-drop" => {
            check_unary(expr, "rc-drop", args)?;
            let a = generate_expr(&args[0])?;
            Some(lir::Expr::RcDrop { value: Box::new(a) })
        }

        // Share and clone
        "share" => {
            check_unary(expr, "share", args)?;
            let value = generate_expr(&args[0])?;
            let rc_var = fresh_var("rc");
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
        "clone" => {
            check_unary(expr, "clone", args)?;
            let a = generate_expr(&args[0])?;
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
            let arr = generate_expr(&args[0])?;
            let idx = generate_expr(&args[1])?;
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
            let arr = generate_expr(&args[0])?;
            let idx = generate_expr(&args[1])?;
            let val = generate_expr(&args[2])?;
            Some(lir::Expr::ArraySet {
                elem_type: lir::ScalarType::I64,
                size: 0,
                array: Box::new(arr),
                index: Box::new(idx),
                value: Box::new(val),
            })
        }
        "array-len" | "alen" => Some(lir::Expr::ArrayLen { size: 0 }),

        _ => None,
    };

    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn compile(source: &str) -> String {
        let mut parser = Parser::new(source).unwrap();
        let program = parser.parse_program().unwrap();
        generate_string(&program).unwrap()
    }

    #[test]
    fn test_generate_simple() {
        let lir = compile("(defun add (a b) (+ a b))");
        assert!(lir.contains("define"));
        assert!(lir.contains("add"));
    }

    #[test]
    fn test_comparison_ops() {
        let lir = compile("(defun cmp (a b) (< a b))");
        assert!(lir.contains("icmp slt"));

        let lir = compile("(defun cmp (a b) (<= a b))");
        assert!(lir.contains("icmp sle"));

        let lir = compile("(defun cmp (a b) (!= a b))");
        assert!(lir.contains("icmp ne"));
    }

    #[test]
    fn test_let_binding() {
        let lir = compile("(defun foo () (let ((x 1) (y 2)) (+ x y)))");
        assert!(lir.contains("let"));
        assert!(lir.contains("(i64 1)"));
        assert!(lir.contains("(i64 2)"));
    }

    #[test]
    fn test_if_expr() {
        let lir = compile("(defun max (a b) (if (> a b) a b))");
        assert!(lir.contains("select"));
        assert!(lir.contains("icmp sgt"));
    }

    #[test]
    fn test_do_block() {
        let lir = compile("(defun foo () (do 1 2 3))");
        assert!(lir.contains("(i64 3)"));
    }

    #[test]
    fn test_function_call() {
        let lir = compile(
            r#"
            (defun double (x) (* x 2))
            (defun quad (x) (double (double x)))
            "#,
        );
        assert!(lir.contains("call @double"));
    }

    #[test]
    fn test_borrow_ref() {
        let lir = compile("(defun get-ref (x) (ref x))");
        assert!(lir.contains("borrow ref"));
    }

    #[test]
    fn test_borrow_refmut() {
        let lir = compile("(defun get-mut (x) (ref-mut x))");
        assert!(lir.contains("borrow refmut"));
    }

    #[test]
    fn test_ownership_ops() {
        let lir = compile("(defun test-drop (x) (drop x))");
        assert!(lir.contains("(drop x)"));

        let lir = compile("(defun test-move (x) (move x))");
        assert!(lir.contains("(move x)"));
    }

    #[test]
    fn test_rc_new() {
        let lir = compile("(defun make-rc () (rc-new 42))");
        assert!(lir.contains("rc-alloc"));
        assert!(lir.contains("rc-ptr"));
    }

    #[test]
    fn test_share() {
        let lir = compile("(defun make-shared () (share 42))");
        assert!(lir.contains("rc-alloc"));
        assert!(lir.contains("rc-ptr"));
    }

    #[test]
    fn test_clone() {
        let lir = compile("(defun clone-it (x) (clone x))");
        assert!(lir.contains("rc-clone"));
    }

    #[test]
    fn test_atom_create() {
        reset_var_counter();
        let lir = compile("(defun make-counter () (atom 0))");
        assert!(lir.contains("rc-alloc"));
        assert!(lir.contains("atomic-store"));
    }

    #[test]
    fn test_atom_deref() {
        let lir = compile("(defun read-atom (a) @a)");
        assert!(lir.contains("atomic-load"));
        assert!(lir.contains("seq_cst"));
    }

    #[test]
    fn test_atom_reset() {
        let lir = compile("(defun set-atom (a v) (reset! a v))");
        assert!(lir.contains("atomic-store"));
    }

    #[test]
    fn test_atom_swap() {
        reset_var_counter();
        let lir = compile("(defun inc-atom (a) (swap! a inc))");
        assert!(lir.contains("atomic-load"));
        assert!(lir.contains("call @inc"));
        assert!(lir.contains("atomic-store"));
    }

    #[test]
    fn test_atom_compare_and_set() {
        let lir = compile("(defun cas-atom (a old new) (compare-and-set! a old new))");
        assert!(lir.contains("cmpxchg"));
        assert!(lir.contains("extractvalue"));
    }

    #[test]
    fn test_vector_literal() {
        let lir = compile("(defun make-vec () [1 2 3])");
        assert!(lir.contains("(i64 1)"));
        assert!(lir.contains("(i64 2)"));
        assert!(lir.contains("(i64 3)"));
    }

    #[test]
    fn test_map_literal() {
        let lir = compile("(defun make-map () {:a 1 :b 2})");
        assert!(lir.contains(":a"));
        assert!(lir.contains("(i64 1)"));
    }

    #[test]
    fn test_keyword_literal() {
        let lir = compile("(defun get-key () :foo)");
        assert!(lir.contains(":foo"));
    }

    #[test]
    fn test_async() {
        let lir = compile("(defun fetch-data () (async (compute)))");
        // Async is currently a stub - just check it compiles
        assert!(lir.contains("define"));
    }

    #[test]
    fn test_await() {
        let lir = compile("(defun wait-data (f) (await f))");
        assert!(lir.contains("define"));
    }

    #[test]
    fn test_conv_vector_literal() {
        let lir = compile("(defun make-conv-vec () <[1 2 3]>)");
        assert!(lir.contains("(i64 1)"));
        assert!(lir.contains("(i64 2)"));
        assert!(lir.contains("(i64 3)"));
    }

    #[test]
    fn test_conv_map_literal() {
        let lir = compile("(defun make-conv-map () <{:a 1 :b 2}>)");
        assert!(lir.contains(":a"));
        assert!(lir.contains("(i64 1)"));
    }

    #[test]
    fn test_dosync() {
        let lir = compile("(defun transfer () (dosync (alter a - 50)))");
        assert!(lir.contains("define"));
    }

    #[test]
    fn test_ref_set() {
        let lir = compile("(defun set-val (r) (ref-set r 10))");
        assert!(lir.contains("store"));
    }

    #[test]
    fn test_alter() {
        let lir = compile("(defun update (r) (alter r + 1))");
        // Alter should generate a load, call, and store
        assert!(lir.contains("define"));
    }

    #[test]
    fn test_commute() {
        let lir = compile("(defun inc (r) (commute r + 1))");
        assert!(lir.contains("define"));
    }

    #[test]
    fn test_simd_vector_int() {
        let lir = compile("(defun make-vec () <<1 2 3 4>>)");
        assert!(lir.contains("vector"));
        assert!(lir.contains("(i64 1)"));
        assert!(lir.contains("(i64 4)"));
    }

    #[test]
    fn test_simd_vector_float() {
        let lir = compile("(defun make-vec () <<1.0 2.0 3.0 4.0>>)");
        assert!(lir.contains("vector"));
        assert!(lir.contains("double"));
    }

    #[test]
    fn test_defprotocol() {
        // Protocols are currently skipped in output
        let lir = compile(
            r#"
            (defprotocol Seq
              (first [self])
              (rest [self]))
            "#,
        );
        // Should compile without error
        assert!(lir.is_empty() || !lir.contains("define"));
    }

    #[test]
    fn test_extend_protocol() {
        // Protocol extensions are currently skipped
        let lir = compile(
            r#"
            (defprotocol Counted
              (count [self]))
            (extend-protocol Counted PersistentVector
              (count [self] (len self)))
            "#,
        );
        assert!(lir.is_empty() || !lir.contains("extend"));
    }

    #[test]
    fn test_iter() {
        let lir = compile("(defun make-iter (v) (iter v))");
        assert!(lir.contains("define"));
    }

    #[test]
    fn test_collect() {
        let lir = compile("(defun materialize (it) (collect it))");
        assert!(lir.contains("define"));
    }

    #[test]
    fn test_iter_pipeline() {
        let lir = compile("(defun squares () (collect (iter [1 2 3])))");
        assert!(lir.contains("define"));
    }

    #[test]
    fn test_byte_array() {
        let lir = compile("(defun get-bytes () #[1 2 3])");
        assert!(lir.contains("(i8 1)"));
        assert!(lir.contains("(i8 2)"));
        assert!(lir.contains("(i8 3)"));
    }

    #[test]
    fn test_byte_array_empty() {
        let lir = compile("(defun get-empty () #[])");
        assert!(lir.contains("define"));
    }

    #[test]
    fn test_regex() {
        let lir = compile(r#"(defun get-pattern () #r"hello")"#);
        assert!(lir.contains("regex:hello"));
    }

    #[test]
    fn test_regex_with_flags() {
        let lir = compile(r#"(defun get-pattern () #r"pattern"im)"#);
        assert!(lir.contains("regex:pattern:im"));
    }

    #[test]
    fn test_boxed_arithmetic() {
        let lir = compile("(defun big-mult (x y) (boxed (* x y)))");
        assert!(lir.contains("mul"));
    }

    #[test]
    fn test_wrapping_arithmetic() {
        let lir = compile("(defun wrap-add (a b) (wrapping (+ a b)))");
        assert!(lir.contains("add"));
    }
}

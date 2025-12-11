//! Closure conversion pass
//!
//! Transforms lambdas into lifted functions with explicit environment structs.
//! This runs AFTER closure analysis and BEFORE codegen.
//!
//! The conversion:
//! 1. ALL functions (defun) get an env parameter as first argument (ptr, can be null)
//! 2. Each lambda becomes:
//!    - A lifted top-level function `__lambda_N` with signature `(env: ptr, params...)`
//!    - If it has captures: an environment struct `__env_N` with captured values
//!    - A ClosureLit { fn_name, env } that creates the closure struct
//! 3. Function references (when passed as values) become ClosureLit { fn_name, None }
//! 4. Direct function calls add null as the first (env) argument

use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ast::{Def, Defun, Expr, ExtendProtocol, Item, LetBinding, Param, Program};
use crate::error::Result;
use crate::span::{Span, Spanned};

use super::escape::{EscapeInfo, EscapeStatus};
use super::types::{Capture, CaptureInfo};

/// Counter for generating unique lambda names
static LAMBDA_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn fresh_lambda_name() -> String {
    let n = LAMBDA_COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("__lambda_{}", n)
}

fn fresh_env_struct_name() -> String {
    let n = LAMBDA_COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("__env_{}", n)
}

/// Find which variables in a set are used as callables (function position) in an expression
fn find_callable_vars(expr: &Expr, var_names: &HashSet<String>) -> HashSet<String> {
    let mut callable = HashSet::new();
    find_callable_vars_rec(expr, var_names, &mut callable);
    callable
}

fn find_callable_vars_rec(
    expr: &Expr,
    var_names: &HashSet<String>,
    callable: &mut HashSet<String>,
) {
    match expr {
        Expr::Call(func, args) => {
            // If the function is one of our variables, it's callable
            if let Expr::Var(name) = &func.node {
                if var_names.contains(name) {
                    callable.insert(name.clone());
                }
            }
            // Recurse into function expression and arguments
            find_callable_vars_rec(&func.node, var_names, callable);
            for arg in args {
                find_callable_vars_rec(&arg.node, var_names, callable);
            }
        }
        Expr::If(cond, then_, else_) => {
            find_callable_vars_rec(&cond.node, var_names, callable);
            find_callable_vars_rec(&then_.node, var_names, callable);
            find_callable_vars_rec(&else_.node, var_names, callable);
        }
        Expr::Let(bindings, body) | Expr::Plet(bindings, body) => {
            for binding in bindings {
                find_callable_vars_rec(&binding.value.node, var_names, callable);
            }
            find_callable_vars_rec(&body.node, var_names, callable);
        }
        Expr::Do(exprs) => {
            for e in exprs {
                find_callable_vars_rec(&e.node, var_names, callable);
            }
        }
        Expr::Lambda(_, body) => {
            find_callable_vars_rec(&body.node, var_names, callable);
        }
        Expr::Field(obj, _) => {
            find_callable_vars_rec(&obj.node, var_names, callable);
        }
        Expr::Struct(_, fields) => {
            for (_, value) in fields {
                find_callable_vars_rec(&value.node, var_names, callable);
            }
        }
        Expr::Ref(inner) | Expr::RefMut(inner) | Expr::Deref(inner) | Expr::Unsafe(inner) => {
            find_callable_vars_rec(&inner.node, var_names, callable);
        }
        Expr::Set(_, value) => {
            find_callable_vars_rec(&value.node, var_names, callable);
        }
        Expr::Atom(value) => {
            find_callable_vars_rec(&value.node, var_names, callable);
        }
        Expr::Swap(atom, func) | Expr::Reset(atom, func) => {
            find_callable_vars_rec(&atom.node, var_names, callable);
            find_callable_vars_rec(&func.node, var_names, callable);
        }
        Expr::AtomDeref(atom) => {
            find_callable_vars_rec(&atom.node, var_names, callable);
        }
        Expr::CompareAndSet { atom, old, new } => {
            find_callable_vars_rec(&atom.node, var_names, callable);
            find_callable_vars_rec(&old.node, var_names, callable);
            find_callable_vars_rec(&new.node, var_names, callable);
        }
        Expr::Vector(elements) | Expr::ConvVector(elements) | Expr::SimdVector(elements) => {
            for e in elements {
                find_callable_vars_rec(&e.node, var_names, callable);
            }
        }
        Expr::Map(pairs) | Expr::ConvMap(pairs) => {
            for (k, v) in pairs {
                find_callable_vars_rec(&k.node, var_names, callable);
                find_callable_vars_rec(&v.node, var_names, callable);
            }
        }
        Expr::Async(body) | Expr::Await(body) => {
            find_callable_vars_rec(&body.node, var_names, callable);
        }
        Expr::Dosync(exprs) => {
            for e in exprs {
                find_callable_vars_rec(&e.node, var_names, callable);
            }
        }
        Expr::RefSetStm(ref_expr, value) => {
            find_callable_vars_rec(&ref_expr.node, var_names, callable);
            find_callable_vars_rec(&value.node, var_names, callable);
        }
        Expr::Alter {
            ref_expr,
            fn_expr,
            args,
        }
        | Expr::Commute {
            ref_expr,
            fn_expr,
            args,
        } => {
            find_callable_vars_rec(&ref_expr.node, var_names, callable);
            find_callable_vars_rec(&fn_expr.node, var_names, callable);
            for arg in args {
                find_callable_vars_rec(&arg.node, var_names, callable);
            }
        }
        Expr::Iter(inner) | Expr::Collect(inner) => {
            find_callable_vars_rec(&inner.node, var_names, callable);
        }
        Expr::Boxed(inner) | Expr::Wrapping(inner) => {
            find_callable_vars_rec(&inner.node, var_names, callable);
        }
        Expr::Quasiquote(inner) | Expr::Unquote(inner) | Expr::UnquoteSplicing(inner) => {
            find_callable_vars_rec(&inner.node, var_names, callable);
        }
        Expr::ClosureLit { env, .. } => {
            if let Some(e) = env {
                find_callable_vars_rec(&e.node, var_names, callable);
            }
        }
        Expr::HeapEnvAlloc { fields, .. } | Expr::StackEnvAlloc { fields, .. } => {
            for (_, value) in fields {
                find_callable_vars_rec(&value.node, var_names, callable);
            }
        }
        // Leaf nodes - no recursion needed
        Expr::Int(_)
        | Expr::Float(_)
        | Expr::Bool(_)
        | Expr::String(_)
        | Expr::Nil
        | Expr::Var(_)
        | Expr::Quote(_)
        | Expr::Keyword(_)
        | Expr::ByteArray(_)
        | Expr::Regex { .. }
        | Expr::Gensym(_) => {}
    }
}

/// Reset the lambda counter (for testing)
#[cfg(test)]
pub fn reset_lambda_counter() {
    LAMBDA_COUNTER.store(0, Ordering::SeqCst);
}

/// Closure converter state
pub struct ClosureConverter {
    /// Capture info from analysis phase
    capture_info: HashMap<Span, CaptureInfo>,
    /// Escape info from escape analysis phase
    escape_info: EscapeInfo,
    /// Generated top-level functions (lifted lambdas)
    generated_functions: Vec<Spanned<Item>>,
    /// Generated struct definitions (environment structs)
    generated_structs: Vec<Spanned<Item>>,
    /// Set of known function names (for detecting function-as-value usage)
    known_functions: HashSet<String>,
}

impl ClosureConverter {
    pub fn new(capture_info: HashMap<Span, CaptureInfo>, escape_info: EscapeInfo) -> Self {
        Self {
            capture_info,
            escape_info,
            generated_functions: Vec::new(),
            generated_structs: Vec::new(),
            known_functions: HashSet::new(),
        }
    }

    /// Convert a program, transforming all lambdas to lifted functions
    pub fn convert(mut self, program: Program) -> Result<Program> {
        // First pass: collect all known function names
        for item in &program.items {
            if let Item::Defun(defun) = &item.node {
                self.known_functions.insert(defun.name.node.clone());
            }
        }

        // Second pass: convert all items
        let mut new_items = Vec::new();
        for item in program.items {
            let converted = self.convert_item(item)?;
            new_items.push(converted);
        }

        // Add generated structs first (they need to be defined before functions use them)
        let mut final_items = self.generated_structs;
        final_items.extend(self.generated_functions);
        final_items.extend(new_items);

        Ok(Program { items: final_items })
    }

    fn convert_item(&mut self, item: Spanned<Item>) -> Result<Spanned<Item>> {
        let span = item.span;
        let node = match item.node {
            Item::Defun(defun) => Item::Defun(self.convert_defun(defun)?),
            Item::Def(def) => Item::Def(self.convert_def(def)?),
            Item::ExtendProtocol(extend) => {
                Item::ExtendProtocol(self.convert_extend_protocol(extend)?)
            }
            // These don't need conversion
            Item::Defstruct(s) => Item::Defstruct(s),
            Item::Defprotocol(p) => Item::Defprotocol(p),
            Item::Defmacro(m) => Item::Defmacro(m),
            Item::Extern(e) => Item::Extern(e),
        };
        Ok(Spanned::new(node, span))
    }

    fn convert_defun(&mut self, defun: Defun) -> Result<Defun> {
        // First, find which parameters are used as callables in the body
        // This includes direct calls AND captures used as callables in lambdas
        let param_names: HashSet<String> =
            defun.params.iter().map(|p| p.name.node.clone()).collect();
        let callable_params = find_callable_vars(&defun.body.node, &param_names);

        // Add __env as first parameter (uniform calling convention)
        // All functions take an env parameter, even if they don't use it
        let env_param = Param {
            name: Spanned::new("__env".to_string(), defun.name.span),
            ty: Some(Spanned::new(
                crate::ast::Type::Named("ptr".to_string()),
                defun.name.span,
            )),
            mutable: false,
        };

        // Update parameter types for callable params that don't have explicit types
        let user_params: Vec<Param> = defun
            .params
            .into_iter()
            .map(|p| {
                if p.ty.is_none() && callable_params.contains(&p.name.node) {
                    // This parameter is used as a callable - give it Closure type
                    Param {
                        name: p.name.clone(),
                        ty: Some(Spanned::new(crate::ast::Type::Closure, p.name.span)),
                        mutable: p.mutable,
                    }
                } else {
                    p
                }
            })
            .collect();

        // Combine: __env first, then user params
        let mut new_params = vec![env_param];
        new_params.extend(user_params);

        // Convert the body, which may contain lambdas
        let new_body = self.convert_expr(defun.body)?;
        Ok(Defun {
            name: defun.name,
            params: new_params,
            return_type: defun.return_type,
            body: new_body,
        })
    }

    fn convert_def(&mut self, def: Def) -> Result<Def> {
        let new_value = self.convert_expr(def.value)?;
        Ok(Def {
            name: def.name,
            value: new_value,
        })
    }

    fn convert_extend_protocol(&mut self, extend: ExtendProtocol) -> Result<ExtendProtocol> {
        let mut new_implementations = Vec::new();
        for method in extend.implementations {
            let new_body = self.convert_expr(method.body)?;
            new_implementations.push(crate::ast::MethodImpl {
                name: method.name,
                params: method.params,
                body: new_body,
            });
        }
        Ok(ExtendProtocol {
            protocol: extend.protocol,
            type_name: extend.type_name,
            implementations: new_implementations,
        })
    }

    fn convert_expr(&mut self, expr: Spanned<Expr>) -> Result<Spanned<Expr>> {
        let span = expr.span;
        let node = match expr.node {
            // Lambda - the main conversion target
            Expr::Lambda(params, body) => {
                return self.convert_lambda(params, *body, span);
            }

            // Recursively convert sub-expressions
            // For calls: don't wrap the function position in ClosureLit
            Expr::Call(func, args) => {
                // Convert function - if it's a known function name, keep it as Var (direct call)
                // Otherwise convert recursively (might be a closure call)
                let new_func = match &func.node {
                    Expr::Var(name) if self.known_functions.contains(name) => {
                        // Direct call to known function - keep as Var
                        func
                    }
                    _ => {
                        // Closure or computed function - convert recursively
                        Box::new(self.convert_expr(*func)?)
                    }
                };
                let new_args: Result<Vec<_>> =
                    args.into_iter().map(|a| self.convert_expr(a)).collect();
                Expr::Call(new_func, new_args?)
            }

            Expr::Let(bindings, body) => {
                let new_bindings = self.convert_bindings(bindings)?;
                let new_body = Box::new(self.convert_expr(*body)?);
                Expr::Let(new_bindings, new_body)
            }

            Expr::Plet(bindings, body) => {
                let new_bindings = self.convert_bindings(bindings)?;
                let new_body = Box::new(self.convert_expr(*body)?);
                Expr::Plet(new_bindings, new_body)
            }

            Expr::If(cond, then_, else_) => {
                let new_cond = Box::new(self.convert_expr(*cond)?);
                let new_then = Box::new(self.convert_expr(*then_)?);
                let new_else = Box::new(self.convert_expr(*else_)?);
                Expr::If(new_cond, new_then, new_else)
            }

            Expr::Do(exprs) => {
                let new_exprs: Result<Vec<_>> =
                    exprs.into_iter().map(|e| self.convert_expr(e)).collect();
                Expr::Do(new_exprs?)
            }

            Expr::Set(name, value) => {
                let new_value = Box::new(self.convert_expr(*value)?);
                Expr::Set(name, new_value)
            }

            Expr::Ref(inner) => Expr::Ref(Box::new(self.convert_expr(*inner)?)),
            Expr::RefMut(inner) => Expr::RefMut(Box::new(self.convert_expr(*inner)?)),
            Expr::Deref(inner) => Expr::Deref(Box::new(self.convert_expr(*inner)?)),
            Expr::Unsafe(inner) => Expr::Unsafe(Box::new(self.convert_expr(*inner)?)),

            Expr::Struct(name, fields) => {
                let new_fields: Result<Vec<_>> = fields
                    .into_iter()
                    .map(|(n, v)| Ok((n, self.convert_expr(v)?)))
                    .collect();
                Expr::Struct(name, new_fields?)
            }

            Expr::Field(obj, field) => {
                let new_obj = Box::new(self.convert_expr(*obj)?);
                Expr::Field(new_obj, field)
            }

            // Atom expressions
            Expr::Atom(value) => Expr::Atom(Box::new(self.convert_expr(*value)?)),
            Expr::Swap(atom, func) => {
                let new_atom = Box::new(self.convert_expr(*atom)?);
                let new_func = Box::new(self.convert_expr(*func)?);
                Expr::Swap(new_atom, new_func)
            }
            Expr::Reset(atom, value) => {
                let new_atom = Box::new(self.convert_expr(*atom)?);
                let new_value = Box::new(self.convert_expr(*value)?);
                Expr::Reset(new_atom, new_value)
            }
            Expr::AtomDeref(atom) => Expr::AtomDeref(Box::new(self.convert_expr(*atom)?)),
            Expr::CompareAndSet { atom, old, new } => {
                let new_atom = Box::new(self.convert_expr(*atom)?);
                let new_old = Box::new(self.convert_expr(*old)?);
                let new_new = Box::new(self.convert_expr(*new)?);
                Expr::CompareAndSet {
                    atom: new_atom,
                    old: new_old,
                    new: new_new,
                }
            }

            // Collections
            Expr::Vector(elements) => {
                let new_elements: Result<Vec<_>> =
                    elements.into_iter().map(|e| self.convert_expr(e)).collect();
                Expr::Vector(new_elements?)
            }
            Expr::Map(pairs) => {
                let new_pairs: Result<Vec<_>> = pairs
                    .into_iter()
                    .map(|(k, v)| Ok((self.convert_expr(k)?, self.convert_expr(v)?)))
                    .collect();
                Expr::Map(new_pairs?)
            }
            Expr::ConvVector(elements) => {
                let new_elements: Result<Vec<_>> =
                    elements.into_iter().map(|e| self.convert_expr(e)).collect();
                Expr::ConvVector(new_elements?)
            }
            Expr::ConvMap(pairs) => {
                let new_pairs: Result<Vec<_>> = pairs
                    .into_iter()
                    .map(|(k, v)| Ok((self.convert_expr(k)?, self.convert_expr(v)?)))
                    .collect();
                Expr::ConvMap(new_pairs?)
            }

            // Async
            Expr::Async(body) => Expr::Async(Box::new(self.convert_expr(*body)?)),
            Expr::Await(future) => Expr::Await(Box::new(self.convert_expr(*future)?)),

            // SIMD
            Expr::SimdVector(elements) => {
                let new_elements: Result<Vec<_>> =
                    elements.into_iter().map(|e| self.convert_expr(e)).collect();
                Expr::SimdVector(new_elements?)
            }

            // STM
            Expr::Dosync(exprs) => {
                let new_exprs: Result<Vec<_>> =
                    exprs.into_iter().map(|e| self.convert_expr(e)).collect();
                Expr::Dosync(new_exprs?)
            }
            Expr::RefSetStm(ref_expr, value) => {
                let new_ref = Box::new(self.convert_expr(*ref_expr)?);
                let new_value = Box::new(self.convert_expr(*value)?);
                Expr::RefSetStm(new_ref, new_value)
            }
            Expr::Alter {
                ref_expr,
                fn_expr,
                args,
            } => {
                let new_ref = Box::new(self.convert_expr(*ref_expr)?);
                let new_fn = Box::new(self.convert_expr(*fn_expr)?);
                let new_args: Result<Vec<_>> =
                    args.into_iter().map(|a| self.convert_expr(a)).collect();
                Expr::Alter {
                    ref_expr: new_ref,
                    fn_expr: new_fn,
                    args: new_args?,
                }
            }
            Expr::Commute {
                ref_expr,
                fn_expr,
                args,
            } => {
                let new_ref = Box::new(self.convert_expr(*ref_expr)?);
                let new_fn = Box::new(self.convert_expr(*fn_expr)?);
                let new_args: Result<Vec<_>> =
                    args.into_iter().map(|a| self.convert_expr(a)).collect();
                Expr::Commute {
                    ref_expr: new_ref,
                    fn_expr: new_fn,
                    args: new_args?,
                }
            }

            // Iterators
            Expr::Iter(coll) => Expr::Iter(Box::new(self.convert_expr(*coll)?)),
            Expr::Collect(iter) => Expr::Collect(Box::new(self.convert_expr(*iter)?)),

            // Overflow handling
            Expr::Boxed(inner) => Expr::Boxed(Box::new(self.convert_expr(*inner)?)),
            Expr::Wrapping(inner) => Expr::Wrapping(Box::new(self.convert_expr(*inner)?)),

            // Macro syntax (should be expanded before this pass)
            Expr::Quasiquote(inner) => Expr::Quasiquote(Box::new(self.convert_expr(*inner)?)),
            Expr::Unquote(inner) => Expr::Unquote(Box::new(self.convert_expr(*inner)?)),
            Expr::UnquoteSplicing(inner) => {
                Expr::UnquoteSplicing(Box::new(self.convert_expr(*inner)?))
            }

            // Already converted (shouldn't happen, but handle gracefully)
            Expr::ClosureLit { fn_name, env } => {
                let new_env = match env {
                    Some(e) => Some(Box::new(self.convert_expr(*e)?)),
                    None => None,
                };
                Expr::ClosureLit {
                    fn_name,
                    env: new_env,
                }
            }
            Expr::HeapEnvAlloc {
                struct_name,
                fields,
            } => {
                let new_fields: Result<Vec<_>> = fields
                    .into_iter()
                    .map(|(n, v)| Ok((n, self.convert_expr(v)?)))
                    .collect();
                Expr::HeapEnvAlloc {
                    struct_name,
                    fields: new_fields?,
                }
            }
            Expr::StackEnvAlloc {
                struct_name,
                fields,
            } => {
                let new_fields: Result<Vec<_>> = fields
                    .into_iter()
                    .map(|(n, v)| Ok((n, self.convert_expr(v)?)))
                    .collect();
                Expr::StackEnvAlloc {
                    struct_name,
                    fields: new_fields?,
                }
            }

            // Function references used as values get wrapped in ClosureLit
            Expr::Var(ref name) if self.known_functions.contains(name) => {
                // Function used as a value - wrap in ClosureLit { fn_name, None }
                Expr::ClosureLit {
                    fn_name: name.clone(),
                    env: None,
                }
            }

            // Literals - no conversion needed
            Expr::Int(_)
            | Expr::Float(_)
            | Expr::Bool(_)
            | Expr::String(_)
            | Expr::Nil
            | Expr::Var(_)
            | Expr::Keyword(_)
            | Expr::Quote(_)
            | Expr::ByteArray(_)
            | Expr::Regex { .. }
            | Expr::Gensym(_) => expr.node,
        };
        Ok(Spanned::new(node, span))
    }

    fn convert_bindings(&mut self, bindings: Vec<LetBinding>) -> Result<Vec<LetBinding>> {
        bindings
            .into_iter()
            .map(|binding| {
                let new_value = self.convert_expr(binding.value)?;
                Ok(LetBinding {
                    name: binding.name,
                    ty: binding.ty,
                    value: new_value,
                })
            })
            .collect()
    }

    /// Convert a lambda to a ClosureLit
    fn convert_lambda(
        &mut self,
        params: Vec<Param>,
        body: Spanned<Expr>,
        span: Span,
    ) -> Result<Spanned<Expr>> {
        // Get capture info for this lambda
        let capture_info = self.capture_info.get(&span).cloned().unwrap_or_default();

        // Generate unique names
        let fn_name = fresh_lambda_name();

        // Convert the body (may contain nested lambdas)
        let converted_body = self.convert_expr(body)?;

        // Track the env struct name (if any) for use in RcAlloc later
        let mut env_struct_name_for_alloc: Option<String> = None;

        // Build the lifted function
        let lifted_fn = if capture_info.captures.is_empty() {
            // No captures - simple case
            // Function signature: (params...) -> result
            Defun {
                name: Spanned::new(fn_name.clone(), span),
                params: params.clone(),
                return_type: None,
                body: converted_body,
            }
        } else {
            // Has captures - need environment struct
            let env_struct_name = fresh_env_struct_name();
            env_struct_name_for_alloc = Some(env_struct_name.clone());

            // Find which captures are used as callables in the body
            let capture_names: HashSet<String> = capture_info
                .captures
                .iter()
                .map(|c| c.name.clone())
                .collect();
            let callable_captures = find_callable_vars(&converted_body.node, &capture_names);

            // Create the environment struct definition
            // Use Type::Closure for callable captures, i64 for others
            let env_fields: Vec<crate::ast::StructField> = capture_info
                .captures
                .iter()
                .map(|cap| {
                    let ty = if callable_captures.contains(&cap.name) {
                        // This capture is called as a function - it's a closure struct
                        crate::ast::Type::Closure
                    } else {
                        // Default to i64 for non-callable captures
                        crate::ast::Type::Named("i64".to_string())
                    };
                    crate::ast::StructField {
                        name: Spanned::new(cap.name.clone(), cap.span),
                        ty: Spanned::new(ty, cap.span),
                    }
                })
                .collect();

            let env_struct = crate::ast::Defstruct {
                name: Spanned::new(env_struct_name.clone(), span),
                fields: env_fields,
            };
            self.generated_structs
                .push(Spanned::new(Item::Defstruct(env_struct), span));

            // Create body that accesses captures from env parameter
            // We need to wrap the body to extract captured variables from the env
            let body_with_env_access = self.wrap_body_with_env_access(
                converted_body,
                &env_struct_name,
                &capture_info.captures,
                span,
            );

            // Add env parameter at the start with the actual struct type (not just ptr)
            // This allows codegen to know the struct type for field access
            let mut lifted_params = vec![Param {
                name: Spanned::new("__env".to_string(), span),
                ty: Some(Spanned::new(
                    crate::ast::Type::Named(env_struct_name.clone()),
                    span,
                )),
                mutable: false,
            }];
            lifted_params.extend(params.clone());

            Defun {
                name: Spanned::new(fn_name.clone(), span),
                params: lifted_params,
                return_type: None,
                body: body_with_env_access,
            }
        };

        // Add the lifted function to generated functions
        self.generated_functions
            .push(Spanned::new(Item::Defun(lifted_fn), span));

        // Create the ClosureLit expression
        let env_expr = if let Some(env_struct_name) = env_struct_name_for_alloc {
            // Create env allocation based on escape status
            let env_fields: Vec<(Spanned<String>, Spanned<Expr>)> = capture_info
                .captures
                .iter()
                .map(|cap| {
                    (
                        Spanned::new(cap.name.clone(), cap.span),
                        Spanned::new(Expr::Var(cap.name.clone()), cap.span),
                    )
                })
                .collect();

            // Check if this closure escapes - use stack alloc if local, heap alloc if escapes
            let escapes = self
                .escape_info
                .get(&span)
                .copied()
                .unwrap_or(EscapeStatus::Escapes); // Conservative default

            let alloc_expr = if escapes == EscapeStatus::Local {
                // Non-escaping closure - use stack allocation
                Expr::StackEnvAlloc {
                    struct_name: env_struct_name,
                    fields: env_fields,
                }
            } else {
                // Escaping closure - use heap allocation
                Expr::HeapEnvAlloc {
                    struct_name: env_struct_name,
                    fields: env_fields,
                }
            };

            Some(Box::new(Spanned::new(alloc_expr, span)))
        } else {
            None
        };

        Ok(Spanned::new(
            Expr::ClosureLit {
                fn_name,
                env: env_expr,
            },
            span,
        ))
    }

    /// Wrap a lambda body to extract captured variables from the env parameter
    fn wrap_body_with_env_access(
        &self,
        body: Spanned<Expr>,
        _env_struct_name: &str,
        captures: &[Capture],
        span: Span,
    ) -> Spanned<Expr> {
        if captures.is_empty() {
            return body;
        }

        // Create let bindings to extract each capture from the env
        // (let ((x (. __env x)) (y (. __env y))) body)
        let bindings: Vec<LetBinding> = captures
            .iter()
            .map(|cap| {
                let field_access = Expr::Field(
                    Box::new(Spanned::new(Expr::Var("__env".to_string()), span)),
                    Spanned::new(cap.name.clone(), cap.span),
                );
                LetBinding {
                    name: Spanned::new(cap.name.clone(), cap.span),
                    ty: None,
                    value: Spanned::new(field_access, cap.span),
                }
            })
            .collect();

        Spanned::new(Expr::Let(bindings, Box::new(body)), span)
    }
}

/// Convert all lambdas in a program to lifted functions with explicit environments
pub fn convert(
    program: Program,
    capture_info: HashMap<Span, CaptureInfo>,
    escape_info: EscapeInfo,
) -> Result<Program> {
    let converter = ClosureConverter::new(capture_info, escape_info);
    converter.convert(program)
}

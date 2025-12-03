//! Type inference (Hindley-Milner style)

use std::collections::HashMap;

use crate::ast::{Defstruct, Defun, Expr, Item, Program};
use crate::error::{CompileError, Errors, Result};
use crate::span::{Span, Spanned};
use crate::types::{Ty, TyVar, TypeDef, TypeEnv};

/// Type inferencer with unification
pub struct Inferencer {
    /// Substitutions from type variables to types
    substitutions: HashMap<TyVar, Ty>,
    /// Next type variable ID
    next_var: u32,
    /// Collected errors
    errors: Errors,
}

impl Inferencer {
    pub fn new() -> Self {
        Self {
            substitutions: HashMap::new(),
            next_var: 0,
            errors: Errors::new(),
        }
    }

    /// Create a fresh type variable
    fn fresh_var(&mut self) -> Ty {
        let var = TyVar(self.next_var);
        self.next_var += 1;
        Ty::Var(var)
    }

    /// Apply substitutions to a type
    fn apply(&self, ty: &Ty) -> Ty {
        match ty {
            Ty::Var(v) => {
                if let Some(t) = self.substitutions.get(v) {
                    self.apply(t)
                } else {
                    ty.clone()
                }
            }
            Ty::Ref(inner) => Ty::Ref(Box::new(self.apply(inner))),
            Ty::RefMut(inner) => Ty::RefMut(Box::new(self.apply(inner))),
            Ty::Fn(params, ret) => Ty::Fn(
                params.iter().map(|p| self.apply(p)).collect(),
                Box::new(self.apply(ret)),
            ),
            Ty::Tuple(elems) => Ty::Tuple(elems.iter().map(|e| self.apply(e)).collect()),
            _ => ty.clone(),
        }
    }

    /// Check if a type variable occurs in a type (occurs check)
    fn occurs_in(&self, var: TyVar, ty: &Ty) -> bool {
        match ty {
            Ty::Var(v) => {
                if *v == var {
                    return true;
                }
                if let Some(t) = self.substitutions.get(v) {
                    self.occurs_in(var, t)
                } else {
                    false
                }
            }
            Ty::Ref(inner) | Ty::RefMut(inner) => self.occurs_in(var, inner),
            Ty::Fn(params, ret) => {
                params.iter().any(|p| self.occurs_in(var, p)) || self.occurs_in(var, ret)
            }
            Ty::Tuple(elems) => elems.iter().any(|e| self.occurs_in(var, e)),
            _ => false,
        }
    }

    /// Unify two types
    fn unify(&mut self, t1: &Ty, t2: &Ty, span: Span) -> std::result::Result<(), ()> {
        let t1 = self.apply(t1);
        let t2 = self.apply(t2);

        match (&t1, &t2) {
            // Same type
            _ if t1 == t2 => Ok(()),

            // Type variable on left
            (Ty::Var(v), _) => {
                if self.occurs_in(*v, &t2) {
                    self.errors.push(CompileError::type_error(
                        span,
                        format!("infinite type: {} = {}", t1, t2),
                    ));
                    Err(())
                } else {
                    self.substitutions.insert(*v, t2);
                    Ok(())
                }
            }

            // Type variable on right
            (_, Ty::Var(v)) => {
                if self.occurs_in(*v, &t1) {
                    self.errors.push(CompileError::type_error(
                        span,
                        format!("infinite type: {} = {}", t2, t1),
                    ));
                    Err(())
                } else {
                    self.substitutions.insert(*v, t1);
                    Ok(())
                }
            }

            // References
            (Ty::Ref(a), Ty::Ref(b)) | (Ty::RefMut(a), Ty::RefMut(b)) => self.unify(a, b, span),

            // Functions
            (Ty::Fn(p1, r1), Ty::Fn(p2, r2)) => {
                if p1.len() != p2.len() {
                    self.errors.push(CompileError::type_error(
                        span,
                        format!(
                            "function arity mismatch: expected {} arguments, got {}",
                            p1.len(),
                            p2.len()
                        ),
                    ));
                    return Err(());
                }
                for (a, b) in p1.iter().zip(p2.iter()) {
                    self.unify(a, b, span)?;
                }
                self.unify(r1, r2, span)
            }

            // Tuples
            (Ty::Tuple(e1), Ty::Tuple(e2)) => {
                if e1.len() != e2.len() {
                    self.errors.push(CompileError::type_error(
                        span,
                        format!(
                            "tuple length mismatch: expected {}, got {}",
                            e1.len(),
                            e2.len()
                        ),
                    ));
                    return Err(());
                }
                for (a, b) in e1.iter().zip(e2.iter()) {
                    self.unify(a, b, span)?;
                }
                Ok(())
            }

            // Named types
            (Ty::Named(n1), Ty::Named(n2)) if n1 == n2 => Ok(()),

            // Error type unifies with anything
            (Ty::Error, _) | (_, Ty::Error) => Ok(()),

            // Never type unifies with anything (diverges)
            (Ty::Never, _) | (_, Ty::Never) => Ok(()),

            // Type mismatch
            _ => {
                self.errors.push(CompileError::type_error(
                    span,
                    format!("type mismatch: expected {}, found {}", t1, t2),
                ));
                Err(())
            }
        }
    }

    /// Infer type of an expression
    fn infer_expr(&mut self, expr: &Spanned<Expr>, env: &TypeEnv) -> Ty {
        match &expr.node {
            Expr::Int(_) => Ty::I64,
            Expr::Float(_) => Ty::Double,
            Expr::Bool(_) => Ty::Bool,
            Expr::String(_) => Ty::String,
            Expr::Nil => Ty::Ptr,

            Expr::Var(name) => {
                if let Some(ty) = env.get(name) {
                    ty.clone()
                } else {
                    // Undefined variable - should have been caught by resolve
                    Ty::Error
                }
            }

            Expr::Call(func, args) => {
                // Infer function type
                let func_ty = self.infer_expr(func, env);

                // Handle builtin operators
                if let Expr::Var(op) = &func.node {
                    if let Some(ty) = self.infer_builtin(op, args, env, expr.span) {
                        return ty;
                    }
                }

                // Check it's a function
                let ret_ty = self.fresh_var();
                let param_tys: Vec<Ty> = args.iter().map(|_| self.fresh_var()).collect();
                let expected_fn = Ty::Fn(param_tys.clone(), Box::new(ret_ty.clone()));

                let _ = self.unify(&func_ty, &expected_fn, expr.span);

                // Unify argument types
                for (arg, param_ty) in args.iter().zip(param_tys.iter()) {
                    let arg_ty = self.infer_expr(arg, env);
                    let _ = self.unify(&arg_ty, param_ty, arg.span);
                }

                self.apply(&ret_ty)
            }

            Expr::Lambda(params, body) => {
                let mut child_env = env.child();
                let param_tys: Vec<Ty> = params
                    .iter()
                    .map(|p| {
                        let ty = if let Some(ty_ann) = &p.ty {
                            Self::ast_type_to_ty(&ty_ann.node)
                        } else {
                            self.fresh_var()
                        };
                        child_env.insert(p.name.node.clone(), ty.clone());
                        ty
                    })
                    .collect();

                let ret_ty = self.infer_expr(body, &child_env);
                Ty::Fn(param_tys, Box::new(ret_ty))
            }

            Expr::Let(bindings, body) => {
                let mut child_env = env.child();
                for binding in bindings {
                    let value_ty = self.infer_expr(&binding.value, &child_env);
                    child_env.insert(binding.name.node.clone(), value_ty);
                }
                self.infer_expr(body, &child_env)
            }

            Expr::Plet(bindings, body) => {
                // Same as let for type inference
                let mut child_env = env.child();
                for binding in bindings {
                    let value_ty = self.infer_expr(&binding.value, &child_env);
                    child_env.insert(binding.name.node.clone(), value_ty);
                }
                self.infer_expr(body, &child_env)
            }

            Expr::If(cond, then_, else_) => {
                let cond_ty = self.infer_expr(cond, env);
                let _ = self.unify(&cond_ty, &Ty::Bool, cond.span);

                let then_ty = self.infer_expr(then_, env);
                let else_ty = self.infer_expr(else_, env);
                let _ = self.unify(&then_ty, &else_ty, expr.span);

                self.apply(&then_ty)
            }

            Expr::Do(exprs) => {
                if exprs.is_empty() {
                    return Ty::Unit;
                }
                let mut last_ty = Ty::Unit;
                for e in exprs {
                    last_ty = self.infer_expr(e, env);
                }
                last_ty
            }

            Expr::Set(name, value) => {
                if let Some(expected) = env.get(&name.node) {
                    let value_ty = self.infer_expr(value, env);
                    let _ = self.unify(&value_ty, expected, value.span);
                }
                Ty::Unit
            }

            Expr::Ref(inner) => {
                let inner_ty = self.infer_expr(inner, env);
                Ty::Ref(Box::new(inner_ty))
            }

            Expr::RefMut(inner) => {
                let inner_ty = self.infer_expr(inner, env);
                Ty::RefMut(Box::new(inner_ty))
            }

            Expr::Deref(inner) => {
                let inner_ty = self.infer_expr(inner, env);
                match inner_ty {
                    Ty::Ref(t) | Ty::RefMut(t) => *t,
                    Ty::Ptr => self.fresh_var(),
                    _ => {
                        self.errors.push(CompileError::type_error(
                            expr.span,
                            format!("cannot dereference type {}", inner_ty),
                        ));
                        Ty::Error
                    }
                }
            }

            Expr::Struct(name, fields) => {
                // Infer all field types
                for (_, value) in fields {
                    let _ = self.infer_expr(value, env);
                }
                Ty::Named(name.clone())
            }

            Expr::Field(obj, _field) => {
                let _ = self.infer_expr(obj, env);
                // TODO: Look up field type from struct definition
                self.fresh_var()
            }

            Expr::Match(scrutinee, arms) => {
                let scrutinee_ty = self.infer_expr(scrutinee, env);
                let result_ty = self.fresh_var();

                for arm in arms {
                    let mut arm_env = env.child();
                    self.bind_pattern(&arm.pattern, &scrutinee_ty, &mut arm_env);
                    let body_ty = self.infer_expr(&arm.body, &arm_env);
                    let _ = self.unify(&body_ty, &result_ty, arm.body.span);
                }

                self.apply(&result_ty)
            }

            Expr::Quote(_) => Ty::Named("Symbol".to_string()),

            Expr::Unsafe(inner) => self.infer_expr(inner, env),

            // Atom expressions (ADR-011)
            Expr::Atom(value) => {
                // (atom value) creates Atom<T> where T is the type of value
                let inner_ty = self.infer_expr(value, env);
                Ty::Named(format!("Atom<{}>", inner_ty))
            }

            Expr::AtomDeref(atom) => {
                // @atom returns the inner type of the atom
                let atom_ty = self.infer_expr(atom, env);
                // For now, atoms contain i64
                // TODO: Extract inner type from Atom<T>
                let _ = atom_ty; // Use to avoid warning
                Ty::I64
            }

            Expr::Reset(atom, value) => {
                // (reset! atom value) returns the new value
                let _ = self.infer_expr(atom, env);
                self.infer_expr(value, env)
            }

            Expr::Swap(atom, func) => {
                // (swap! atom fn) returns the new value
                let _ = self.infer_expr(atom, env);
                let func_ty = self.infer_expr(func, env);
                // fn should be T -> T, return T
                match func_ty {
                    Ty::Fn(_, ret) => *ret,
                    _ => Ty::I64, // Default to i64 for atoms
                }
            }

            Expr::CompareAndSet { atom, old, new } => {
                // (compare-and-set! atom old new) returns bool (success/failure)
                let _ = self.infer_expr(atom, env);
                let old_ty = self.infer_expr(old, env);
                let new_ty = self.infer_expr(new, env);
                let _ = self.unify(&old_ty, &new_ty, expr.span);
                Ty::Bool
            }

            // Persistent collections (ADR-018)
            Expr::Vector(elements) => {
                let elem_ty = self.fresh_var();
                for elem in elements {
                    let ty = self.infer_expr(elem, env);
                    let _ = self.unify(&ty, &elem_ty, elem.span);
                }
                Ty::Named(format!("Vector<{}>", self.apply(&elem_ty)))
            }

            Expr::Map(pairs) => {
                let key_ty = self.fresh_var();
                let val_ty = self.fresh_var();
                for (k, v) in pairs {
                    let kt = self.infer_expr(k, env);
                    let vt = self.infer_expr(v, env);
                    let _ = self.unify(&kt, &key_ty, k.span);
                    let _ = self.unify(&vt, &val_ty, v.span);
                }
                Ty::Named(format!(
                    "Map<{}, {}>",
                    self.apply(&key_ty),
                    self.apply(&val_ty)
                ))
            }

            Expr::Keyword(_) => Ty::Named("Keyword".to_string()),

            // Conventional mutable collections (ADR-018)
            Expr::ConvVector(elements) => {
                let elem_ty = self.fresh_var();
                for elem in elements {
                    let ty = self.infer_expr(elem, env);
                    let _ = self.unify(&ty, &elem_ty, elem.span);
                }
                Ty::Named(format!("ConvVector<{}>", self.apply(&elem_ty)))
            }

            Expr::ConvMap(pairs) => {
                let key_ty = self.fresh_var();
                let val_ty = self.fresh_var();
                for (k, v) in pairs {
                    let kt = self.infer_expr(k, env);
                    let vt = self.infer_expr(v, env);
                    let _ = self.unify(&kt, &key_ty, k.span);
                    let _ = self.unify(&vt, &val_ty, v.span);
                }
                Ty::Named(format!(
                    "ConvMap<{}, {}>",
                    self.apply(&key_ty),
                    self.apply(&val_ty)
                ))
            }

            // Async/await (ADR-014)
            Expr::Async(body) => {
                // async returns Future<T> where T is the body's return type
                let body_ty = self.infer_expr(body, env);
                Ty::Named(format!("Future<{}>", body_ty))
            }

            Expr::Await(future) => {
                // await takes Future<T> and returns T
                let future_ty = self.infer_expr(future, env);
                // For now, return a fresh type variable
                // TODO: Extract T from Future<T>
                let _ = future_ty;
                self.fresh_var()
            }

            // SIMD vectors (ADR-016)
            Expr::SimdVector(elements) => {
                let elem_ty = self.fresh_var();
                for elem in elements {
                    let ty = self.infer_expr(elem, env);
                    let _ = self.unify(&ty, &elem_ty, elem.span);
                }
                let n = elements.len();
                Ty::Named(format!("<{} x {}>", n, self.apply(&elem_ty)))
            }

            // STM (ADR-012)
            Expr::Dosync(exprs) => {
                // Dosync returns the value of the last expression
                if exprs.is_empty() {
                    Ty::Unit
                } else {
                    let mut last_ty = Ty::Unit;
                    for expr in exprs {
                        last_ty = self.infer_expr(expr, env);
                    }
                    last_ty
                }
            }

            Expr::RefSetStm(ref_expr, value) => {
                // ref-set returns the new value
                let _ = self.infer_expr(ref_expr, env);
                self.infer_expr(value, env)
            }

            Expr::Alter {
                ref_expr,
                fn_expr,
                args,
            } => {
                // alter returns the new value (result of applying fn)
                let _ = self.infer_expr(ref_expr, env);
                let func_ty = self.infer_expr(fn_expr, env);
                for arg in args {
                    let _ = self.infer_expr(arg, env);
                }
                // Return type is the return type of the function
                match func_ty {
                    Ty::Fn(_, ret) => *ret,
                    _ => self.fresh_var(),
                }
            }

            Expr::Commute {
                ref_expr,
                fn_expr,
                args,
            } => {
                // commute returns the new value (result of applying fn)
                let _ = self.infer_expr(ref_expr, env);
                let func_ty = self.infer_expr(fn_expr, env);
                for arg in args {
                    let _ = self.infer_expr(arg, env);
                }
                // Return type is the return type of the function
                match func_ty {
                    Ty::Fn(_, ret) => *ret,
                    _ => self.fresh_var(),
                }
            }

            // Iterators
            Expr::Iter(coll) => {
                // iter returns an iterator type
                let _coll_ty = self.infer_expr(coll, env);
                // For now, return a fresh variable (Iter<T>)
                self.fresh_var()
            }
            Expr::Collect(iter) => {
                // collect returns a vector
                let _iter_ty = self.infer_expr(iter, env);
                // Returns Vec<T>
                self.fresh_var()
            }

            // Byte arrays and regex
            Expr::ByteArray(_) => {
                // Byte arrays have type ByteArray
                Ty::Named("ByteArray".to_string())
            }
            Expr::Regex { .. } => {
                // Regex literals have a Regex type
                Ty::Named("Regex".to_string())
            }

            // Overflow handling - inner expression type flows through
            Expr::Boxed(inner) => {
                // Boxed may promote to BigInt, but starts with inner type
                self.infer_expr(inner, env)
            }
            Expr::Wrapping(inner) => {
                // Wrapping preserves inner type
                self.infer_expr(inner, env)
            }
        }
    }

    /// Infer type for builtin operators
    fn infer_builtin(
        &mut self,
        op: &str,
        args: &[Spanned<Expr>],
        env: &TypeEnv,
        span: Span,
    ) -> Option<Ty> {
        match op {
            // Arithmetic - requires matching numeric types
            "+" | "-" | "*" | "/" | "rem" => {
                if args.len() != 2 {
                    return None;
                }
                let a = self.infer_expr(&args[0], env);
                let b = self.infer_expr(&args[1], env);
                let _ = self.unify(&a, &b, span);
                Some(self.apply(&a))
            }

            // Comparison - requires matching types, returns bool
            "=" | "==" | "!=" | "<" | ">" | "<=" | ">=" => {
                if args.len() != 2 {
                    return None;
                }
                let a = self.infer_expr(&args[0], env);
                let b = self.infer_expr(&args[1], env);
                let _ = self.unify(&a, &b, span);
                Some(Ty::Bool)
            }

            // Boolean
            "not" => {
                if args.len() != 1 {
                    return None;
                }
                let a = self.infer_expr(&args[0], env);
                let _ = self.unify(&a, &Ty::Bool, span);
                Some(Ty::Bool)
            }
            "and" | "or" => {
                if args.len() != 2 {
                    return None;
                }
                let a = self.infer_expr(&args[0], env);
                let b = self.infer_expr(&args[1], env);
                let _ = self.unify(&a, &Ty::Bool, span);
                let _ = self.unify(&b, &Ty::Bool, span);
                Some(Ty::Bool)
            }

            _ => None,
        }
    }

    /// Bind pattern variables to types
    fn bind_pattern(
        &mut self,
        pattern: &Spanned<crate::ast::Pattern>,
        expected: &Ty,
        env: &mut TypeEnv,
    ) {
        use crate::ast::Pattern;
        match &pattern.node {
            Pattern::Wildcard => {}
            Pattern::Var(name) => {
                env.insert(name.clone(), expected.clone());
            }
            Pattern::Literal(_) => {
                // Literals should match the scrutinee type
            }
            Pattern::Struct(_name, fields) => {
                // TODO: Check struct type and bind field patterns
                for (_, pat) in fields {
                    let field_ty = self.fresh_var();
                    self.bind_pattern(&Spanned::new(pat.clone(), pattern.span), &field_ty, env);
                }
            }
            Pattern::Tuple(pats) => {
                for pat in pats {
                    let elem_ty = self.fresh_var();
                    self.bind_pattern(&Spanned::new(pat.clone(), pattern.span), &elem_ty, env);
                }
            }
        }
    }

    /// Convert AST type annotation to internal Ty
    fn ast_type_to_ty(ty: &crate::ast::Type) -> Ty {
        use crate::ast::Type;
        match ty {
            Type::Named(name) => match name.as_str() {
                "i8" => Ty::I8,
                "i16" => Ty::I16,
                "i32" => Ty::I32,
                "i64" | "int" => Ty::I64,
                "float" | "f32" => Ty::Float,
                "double" | "f64" => Ty::Double,
                "bool" => Ty::Bool,
                "char" => Ty::Char,
                "string" => Ty::String,
                "ptr" => Ty::Ptr,
                _ => Ty::Named(name.clone()),
            },
            Type::Ref(inner) => Ty::Ref(Box::new(Self::ast_type_to_ty(inner))),
            Type::RefMut(inner) => Ty::RefMut(Box::new(Self::ast_type_to_ty(inner))),
            Type::Unit => Ty::Unit,
            Type::Fn(params, ret) => Ty::Fn(
                params.iter().map(Self::ast_type_to_ty).collect(),
                Box::new(Self::ast_type_to_ty(ret)),
            ),
            Type::Tuple(elems) => Ty::Tuple(elems.iter().map(Self::ast_type_to_ty).collect()),
        }
    }

    /// Infer types for a function definition
    fn infer_defun(&mut self, defun: &Defun, env: &mut TypeEnv) -> Ty {
        let mut child_env = env.child();

        // Add parameters to environment
        let param_tys: Vec<Ty> = defun
            .params
            .iter()
            .map(|p| {
                let ty = if let Some(ty_ann) = &p.ty {
                    Self::ast_type_to_ty(&ty_ann.node)
                } else {
                    self.fresh_var()
                };
                child_env.insert(p.name.node.clone(), ty.clone());
                ty
            })
            .collect();

        // Infer body type
        let body_ty = self.infer_expr(&defun.body, &child_env);

        // Check return type annotation if present
        let ret_ty = if let Some(ret_ann) = &defun.return_type {
            let expected = Self::ast_type_to_ty(&ret_ann.node);
            let _ = self.unify(&body_ty, &expected, defun.body.span);
            expected
        } else {
            body_ty
        };

        let fn_ty = Ty::Fn(param_tys, Box::new(ret_ty));

        // Add function to environment
        env.insert(defun.name.node.clone(), fn_ty.clone());

        fn_ty
    }

    /// Infer types for a struct definition
    fn infer_defstruct(&mut self, defstruct: &Defstruct, env: &mut TypeEnv) {
        let fields: Vec<(String, Ty)> = defstruct
            .fields
            .iter()
            .map(|f| (f.name.node.clone(), Self::ast_type_to_ty(&f.ty.node)))
            .collect();

        env.types.insert(
            defstruct.name.node.clone(),
            TypeDef {
                name: defstruct.name.node.clone(),
                fields,
            },
        );

        // Add struct type to environment
        env.insert(
            defstruct.name.node.clone(),
            Ty::Named(defstruct.name.node.clone()),
        );
    }
}

impl Default for Inferencer {
    fn default() -> Self {
        Self::new()
    }
}

/// Infer types for a program
pub fn infer(program: &Program, env: &mut TypeEnv) -> Result<()> {
    let mut inferencer = Inferencer::new();

    // First pass: collect all type definitions
    for item in &program.items {
        if let Item::Defstruct(defstruct) = &item.node {
            inferencer.infer_defstruct(defstruct, env);
        }
    }

    // Second pass: collect function signatures (for forward references)
    for item in &program.items {
        if let Item::Defun(defun) = &item.node {
            // Create placeholder type for function
            let param_tys: Vec<Ty> = defun
                .params
                .iter()
                .map(|p| {
                    if let Some(ty_ann) = &p.ty {
                        Inferencer::ast_type_to_ty(&ty_ann.node)
                    } else {
                        inferencer.fresh_var()
                    }
                })
                .collect();

            let ret_ty = if let Some(ret_ann) = &defun.return_type {
                Inferencer::ast_type_to_ty(&ret_ann.node)
            } else {
                inferencer.fresh_var()
            };

            env.insert(defun.name.node.clone(), Ty::Fn(param_tys, Box::new(ret_ty)));
        }
    }

    // Third pass: infer types in all function bodies
    for item in &program.items {
        if let Item::Defun(defun) = &item.node {
            inferencer.infer_defun(defun, env);
        }
    }

    inferencer
        .errors
        .into_result(())
        .map_err(|errors| errors.into_iter().next().expect("at least one error"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn infer_source(source: &str) -> std::result::Result<TypeEnv, CompileError> {
        let mut parser = Parser::new(source).expect("lexer failed");
        let program = parser.parse_program().expect("parser failed");
        let mut env = TypeEnv::new();
        infer(&program, &mut env)?;
        Ok(env)
    }

    #[test]
    fn test_infer_int_literal() {
        let env = infer_source("(defun foo () 42)").unwrap();
        let fn_ty = env.get("foo").unwrap();
        assert!(matches!(fn_ty, Ty::Fn(_, ret) if **ret == Ty::I64));
    }

    #[test]
    fn test_infer_float_literal() {
        let env = infer_source("(defun foo () 3.14)").unwrap();
        let fn_ty = env.get("foo").unwrap();
        assert!(matches!(fn_ty, Ty::Fn(_, ret) if **ret == Ty::Double));
    }

    #[test]
    fn test_infer_bool_literal() {
        let env = infer_source("(defun foo () true)").unwrap();
        let fn_ty = env.get("foo").unwrap();
        assert!(matches!(fn_ty, Ty::Fn(_, ret) if **ret == Ty::Bool));
    }

    #[test]
    fn test_infer_arithmetic() {
        let env = infer_source("(defun add (a b) (+ a b))").unwrap();
        let fn_ty = env.get("add").unwrap();
        // Parameters should be unified to the same type
        if let Ty::Fn(params, _) = fn_ty {
            assert_eq!(params.len(), 2);
        } else {
            panic!("expected function type");
        }
    }

    #[test]
    fn test_infer_comparison() {
        let env = infer_source("(defun less (a b) (< a b))").unwrap();
        let fn_ty = env.get("less").unwrap();
        assert!(matches!(fn_ty, Ty::Fn(_, ret) if **ret == Ty::Bool));
    }

    #[test]
    fn test_infer_if() {
        let env = infer_source("(defun max (a b) (if (> a b) a b))").unwrap();
        let fn_ty = env.get("max").unwrap();
        // Return type should match the branch types
        if let Ty::Fn(params, _ret) = fn_ty {
            assert_eq!(params.len(), 2);
        } else {
            panic!("expected function type");
        }
    }

    #[test]
    fn test_infer_let() {
        let env = infer_source("(defun foo () (let ((x 1)) x))").unwrap();
        let fn_ty = env.get("foo").unwrap();
        assert!(matches!(fn_ty, Ty::Fn(_, ret) if **ret == Ty::I64));
    }
}

//! Code generation - emit lIR from liar AST

use crate::ast::{Def, Defstruct, Defun, Expr, Item, Program};
use crate::error::{CompileError, Result};
use crate::span::Spanned;

/// Generate lIR from a liar program
pub fn generate(program: &Program) -> Result<String> {
    let mut output = String::new();

    for item in &program.items {
        let lir = generate_item(item)?;
        if !lir.is_empty() {
            output.push_str(&lir);
            output.push('\n');
        }
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
        Item::Def(def) => generate_def(def),
        Item::Defstruct(s) => generate_defstruct(s),
    }
}

/// Generate lIR for a constant definition
fn generate_def(def: &Def) -> Result<String> {
    let name = &def.name.node;
    let value = generate_expr(&def.value)?;
    // Global constants become let bindings at top level
    // For now, we'll skip these as they need special handling
    Ok(format!("; def {} = {}", name, value))
}

/// Generate lIR for a struct definition
fn generate_defstruct(defstruct: &Defstruct) -> Result<String> {
    let name = &defstruct.name.node;
    let fields: Vec<String> = defstruct
        .fields
        .iter()
        .map(|f| {
            // Convert liar type to lIR type
            liar_type_to_lir(&f.ty.node)
        })
        .collect();
    Ok(format!("(defstruct {} ({}))", name, fields.join(" ")))
}

/// Convert a liar type to lIR type string
fn liar_type_to_lir(ty: &crate::ast::Type) -> String {
    use crate::ast::Type;
    match ty {
        Type::Named(name) => match name.as_str() {
            "i8" | "i16" | "i32" | "i64" => name.clone(),
            "int" => "i64".to_string(),
            "float" | "f32" => "float".to_string(),
            "double" | "f64" => "double".to_string(),
            "bool" => "i1".to_string(),
            "ptr" => "ptr".to_string(),
            _ => format!("%struct.{}", name), // Assume user-defined struct
        },
        // Use lIR ownership types for references
        Type::Ref(inner) => format!("ref {}", liar_type_to_lir(inner)),
        Type::RefMut(inner) => format!("refmut {}", liar_type_to_lir(inner)),
        Type::Unit => "void".to_string(),
        Type::Fn(_, _) => "ptr".to_string(), // Function pointers
        Type::Tuple(_) => "ptr".to_string(), // Tuples as pointers for now
    }
}

/// Generate lIR for a function definition
fn generate_defun(defun: &Defun) -> Result<String> {
    let name = &defun.name.node;

    // Determine return type
    let ret_type = defun
        .return_type
        .as_ref()
        .map(|t| liar_type_to_lir(&t.node))
        .unwrap_or_else(|| "i64".to_string());

    // Generate parameters
    let params: Vec<String> = defun
        .params
        .iter()
        .map(|p| {
            let ty =
                p.ty.as_ref()
                    .map(|t| liar_type_to_lir(&t.node))
                    .unwrap_or_else(|| "i64".to_string());
            format!("({} {})", ty, p.name.node)
        })
        .collect();

    let body = generate_expr(&defun.body)?;

    Ok(format!(
        "(define ({} {}) ({}) (block entry (ret {})))",
        name,
        ret_type,
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
        Expr::String(s) => {
            // Strings become global constants - for now just escape them
            Ok(format!("(string {:?})", s))
        }
        Expr::Nil => Ok("(ptr null)".to_string()),
        Expr::Var(name) => Ok(name.clone()),

        Expr::Call(func, args) => generate_call(expr, func, args),

        Expr::If(cond, then, else_) => {
            let cond = generate_expr(cond)?;
            let then = generate_expr(then)?;
            let else_ = generate_expr(else_)?;
            // For simple cases, use select
            Ok(format!("(select {} {} {})", cond, then, else_))
        }

        Expr::Let(bindings, body) => {
            // Generate let bindings with ownership tracking
            // We need to: 1) set up bindings, 2) evaluate body, 3) drop owned values

            // First pass: collect binding info
            let mut binding_info: Vec<(String, String, bool)> = Vec::new();
            for binding in bindings.iter() {
                let value = generate_expr(&binding.value)?;
                let name = binding.name.node.clone();
                // Check if this binding creates an owned value
                let is_owned = value.contains("(alloc own") || value.contains("(rc-alloc");
                binding_info.push((name, value, is_owned));
            }

            // Generate body
            let body_code = generate_expr(body)?;

            // Build the let chain with drops at the end
            // Structure: (let ((a val_a)) (let ((b val_b)) ... (let ((_r body)) (drop a) (drop b) _r)))
            let owned_names: Vec<&str> = binding_info
                .iter()
                .filter(|(_, _, is_owned)| *is_owned)
                .map(|(name, _, _)| name.as_str())
                .collect();

            // Inner result with drops
            let mut inner = if owned_names.is_empty() {
                body_code
            } else {
                // Capture result, drop owned values, return result
                let mut drop_seq = "_let_result".to_string();
                for name in owned_names.iter().rev() {
                    drop_seq = format!("(let ((__drop_tmp (drop {}))) {})", name, drop_seq);
                }
                format!("(let ((_let_result {})) {})", body_code, drop_seq)
            };

            // Wrap with let bindings (outermost first)
            for (name, value, _) in binding_info.iter().rev() {
                inner = format!("(let (({} {})) {})", name, value, inner);
            }

            Ok(inner)
        }

        Expr::Plet(bindings, body) => {
            // Plet is the same as let for codegen (threading handled elsewhere)
            let mut result = generate_expr(body)?;
            for binding in bindings.iter().rev() {
                let value = generate_expr(&binding.value)?;
                result = format!("(let (({} {})) {})", binding.name.node, value, result);
            }
            Ok(result)
        }

        Expr::Do(exprs) => {
            // Do block - evaluate expressions in sequence, return last
            if exprs.is_empty() {
                return Ok("(i64 0)".to_string()); // Unit value
            }
            if exprs.len() == 1 {
                return generate_expr(&exprs[0]);
            }
            // Generate as nested lets with dummy bindings for discarded values
            let mut result = generate_expr(exprs.last().unwrap())?;
            for (i, e) in exprs.iter().rev().skip(1).enumerate() {
                let value = generate_expr(e)?;
                result = format!("(let ((_discard{} {})) {})", i, value, result);
            }
            Ok(result)
        }

        Expr::Lambda(_params, _body) => {
            // Lambdas need closure conversion - placeholder for now
            Err(CompileError::codegen(
                expr.span,
                "lambdas require closure analysis (not yet implemented)",
            ))
        }

        Expr::Set(name, value) => {
            let value = generate_expr(value)?;
            // Set generates a store instruction
            Ok(format!("(store {} {})", value, name.node))
        }

        Expr::Ref(inner) => {
            // Create a shared borrow
            let inner_val = generate_expr(inner)?;
            Ok(format!("(borrow ref {})", inner_val))
        }

        Expr::RefMut(inner) => {
            // Create a mutable borrow
            let inner_val = generate_expr(inner)?;
            Ok(format!("(borrow refmut {})", inner_val))
        }

        Expr::Deref(inner) => {
            let inner_val = generate_expr(inner)?;
            // Dereference - generate a load
            Ok(format!("(load i64 {})", inner_val))
        }

        Expr::Struct(name, fields) => {
            // Struct construction
            // This needs alloca + stores
            let field_values: Result<Vec<String>> =
                fields.iter().map(|(_, v)| generate_expr(v)).collect();
            let field_values = field_values?;

            // Generate inline struct literal
            let fields_str = field_values.join(" ");
            Ok(format!("(struct %struct.{} {})", name, fields_str))
        }

        Expr::Field(obj, field) => {
            let obj = generate_expr(obj)?;
            // Field access - needs GEP
            Ok(format!("(field {} {})", obj, field.node))
        }

        Expr::Match(_scrutinee, _arms) => {
            // Match needs branch codegen
            Err(CompileError::codegen(
                expr.span,
                "match requires control flow codegen (not yet implemented)",
            ))
        }

        Expr::Quote(sym) => {
            // Quoted symbol becomes a symbol literal
            Ok(format!("(symbol {})", sym))
        }

        Expr::Unsafe(inner) => {
            // Unsafe just compiles the inner expression
            generate_expr(inner)
        }

        // Atom expressions (ADR-011)
        Expr::Atom(value) => {
            // (atom value) - create atomic cell
            // Allocates RC-managed atomic cell, initializes with value
            let value = generate_expr(value)?;
            // Use rc-alloc for reference counting, atomic-store for initial value
            Ok(format!(
                "(let ((_atom (rc-alloc i64))) (atomic-store seq_cst {} (rc-ptr _atom)) _atom)",
                value
            ))
        }

        Expr::AtomDeref(atom) => {
            // @atom - atomic read
            let atom = generate_expr(atom)?;
            Ok(format!("(atomic-load seq_cst i64 (rc-ptr {}))", atom))
        }

        Expr::Reset(atom, value) => {
            // (reset! atom value) - atomic set
            let atom = generate_expr(atom)?;
            let value = generate_expr(value)?;
            // Store and return new value
            Ok(format!(
                "(let ((_new {})) (atomic-store seq_cst _new (rc-ptr {})) _new)",
                value, atom
            ))
        }

        Expr::Swap(atom, func) => {
            // (swap! atom fn) - atomic update
            // Generates a CAS loop: load current, apply fn, try CAS, retry on failure
            let atom = generate_expr(atom)?;
            let func = generate_expr(func)?;
            // For now, generate a simplified version that assumes single-threaded
            // Full implementation needs CAS loop with retry
            Ok(format!(
                "(let ((_ptr (rc-ptr {}))) \
                   (let ((_old (atomic-load seq_cst i64 _ptr))) \
                     (let ((_new (call @{} _old))) \
                       (atomic-store seq_cst _new _ptr) \
                       _new)))",
                atom, func
            ))
        }

        Expr::CompareAndSet { atom, old, new } => {
            // (compare-and-set! atom old new) - CAS operation
            // Returns true if successful, false otherwise
            let atom = generate_expr(atom)?;
            let old = generate_expr(old)?;
            let new = generate_expr(new)?;
            Ok(format!(
                "(let ((_result (cmpxchg seq_cst (rc-ptr {}) {} {}))) (extractvalue _result 1))",
                atom, old, new
            ))
        }

        // Persistent collections (ADR-018)
        Expr::Vector(elements) => {
            // Generate a persistent vector
            // For now, create a simple representation
            let elems: Result<Vec<String>> = elements.iter().map(generate_expr).collect();
            let elems = elems?;
            Ok(format!("(vec {})", elems.join(" ")))
        }

        Expr::Map(pairs) => {
            // Generate a persistent map
            let mut pair_strs = Vec::new();
            for (k, v) in pairs {
                let key = generate_expr(k)?;
                let val = generate_expr(v)?;
                pair_strs.push(format!("{} {}", key, val));
            }
            Ok(format!("(map {})", pair_strs.join(" ")))
        }

        Expr::Keyword(name) => {
            // Keywords are interned strings
            Ok(format!("(keyword {})", name))
        }

        // Conventional mutable collections (ADR-018)
        Expr::ConvVector(elements) => {
            // Generate a conventional (mutable) vector
            let elems: Result<Vec<String>> = elements.iter().map(generate_expr).collect();
            let elems = elems?;
            Ok(format!("(conv-vec {})", elems.join(" ")))
        }

        Expr::ConvMap(pairs) => {
            // Generate a conventional (mutable) map
            let mut pair_strs = Vec::new();
            for (k, v) in pairs {
                let key = generate_expr(k)?;
                let val = generate_expr(v)?;
                pair_strs.push(format!("{} {}", key, val));
            }
            Ok(format!("(conv-map {})", pair_strs.join(" ")))
        }

        // Async/await (ADR-014)
        Expr::Async(body) => {
            // Async creates a future
            let body = generate_expr(body)?;
            Ok(format!("(future {})", body))
        }

        Expr::Await(future) => {
            // Await blocks until future completes
            let future = generate_expr(future)?;
            Ok(format!("(await {})", future))
        }

        // SIMD vectors (ADR-016)
        Expr::SimdVector(elements) => {
            // Generate SIMD vector literal
            // Infer type from elements
            let elems: Result<Vec<String>> = elements.iter().map(generate_expr).collect();
            let elems = elems?;
            let n = elements.len();
            // Infer element type from first element (simplified)
            let elem_type = if let Some(first) = elements.first() {
                match &first.node {
                    Expr::Float(_) => "double",
                    _ => "i64",
                }
            } else {
                "i64"
            };
            Ok(format!(
                "(vector <{} x {}> {})",
                n,
                elem_type,
                elems.join(" ")
            ))
        }

        // STM (ADR-012)
        Expr::Dosync(exprs) => {
            // Transaction block
            // Start transaction, execute body, commit or retry
            if exprs.is_empty() {
                return Ok("(stm-nil)".to_string());
            }
            let body: Result<Vec<String>> = exprs.iter().map(generate_expr).collect();
            let body = body?;
            if body.len() == 1 {
                Ok(format!("(stm-dosync {})", body[0]))
            } else {
                // Chain expressions
                let mut result = body.last().unwrap().clone();
                for (i, e) in body.iter().rev().skip(1).enumerate() {
                    result = format!("(let ((_stm{} {})) {})", i, e, result);
                }
                Ok(format!("(stm-dosync {})", result))
            }
        }

        Expr::RefSetStm(ref_expr, value) => {
            // Set ref value in transaction
            let ref_code = generate_expr(ref_expr)?;
            let val_code = generate_expr(value)?;
            Ok(format!("(stm-ref-set {} {})", ref_code, val_code))
        }

        Expr::Alter {
            ref_expr,
            fn_expr,
            args,
        } => {
            // Apply function to ref value in transaction
            let ref_code = generate_expr(ref_expr)?;
            let fn_code = generate_expr(fn_expr)?;
            let args_code: Result<Vec<String>> = args.iter().map(generate_expr).collect();
            let args_code = args_code?;
            if args_code.is_empty() {
                Ok(format!("(stm-alter {} {})", ref_code, fn_code))
            } else {
                Ok(format!(
                    "(stm-alter {} {} {})",
                    ref_code,
                    fn_code,
                    args_code.join(" ")
                ))
            }
        }

        Expr::Commute {
            ref_expr,
            fn_expr,
            args,
        } => {
            // Commutative update in transaction (can reorder)
            let ref_code = generate_expr(ref_expr)?;
            let fn_code = generate_expr(fn_expr)?;
            let args_code: Result<Vec<String>> = args.iter().map(generate_expr).collect();
            let args_code = args_code?;
            if args_code.is_empty() {
                Ok(format!("(stm-commute {} {})", ref_code, fn_code))
            } else {
                Ok(format!(
                    "(stm-commute {} {} {})",
                    ref_code,
                    fn_code,
                    args_code.join(" ")
                ))
            }
        }
    }
}

/// Generate code for a function call (handles builtins)
fn generate_call(
    expr: &Spanned<Expr>,
    func: &Spanned<Expr>,
    args: &[Spanned<Expr>],
) -> Result<String> {
    // Check for builtin operators
    if let Expr::Var(op) = &func.node {
        if let Some(result) = generate_builtin(expr, op, args)? {
            return Ok(result);
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

    if args_str.is_empty() {
        Ok(format!("(call @{})", func_name))
    } else {
        Ok(format!("(call @{} {})", func_name, args_str.join(" ")))
    }
}

/// Generate code for builtin operations
fn generate_builtin(
    expr: &Spanned<Expr>,
    op: &str,
    args: &[Spanned<Expr>],
) -> Result<Option<String>> {
    fn binary_op(
        expr: &Spanned<Expr>,
        op_name: &str,
        lir_op: &str,
        args: &[Spanned<Expr>],
    ) -> Result<String> {
        if args.len() != 2 {
            return Err(CompileError::codegen(
                expr.span,
                format!("{} requires exactly 2 arguments", op_name),
            ));
        }
        let a = generate_expr(&args[0])?;
        let b = generate_expr(&args[1])?;
        Ok(format!("({} {} {})", lir_op, a, b))
    }

    fn unary_op(
        expr: &Spanned<Expr>,
        op_name: &str,
        lir_op: &str,
        args: &[Spanned<Expr>],
    ) -> Result<String> {
        if args.len() != 1 {
            return Err(CompileError::codegen(
                expr.span,
                format!("{} requires exactly 1 argument", op_name),
            ));
        }
        let a = generate_expr(&args[0])?;
        Ok(format!("({} {})", lir_op, a))
    }

    let result = match op {
        // Arithmetic
        "+" => Some(binary_op(expr, "+", "add", args)?),
        "-" => Some(binary_op(expr, "-", "sub", args)?),
        "*" => Some(binary_op(expr, "*", "mul", args)?),
        "/" => Some(binary_op(expr, "/", "sdiv", args)?),
        "rem" => Some(binary_op(expr, "rem", "srem", args)?),

        // Comparison
        "=" | "==" => Some(binary_op(expr, "=", "icmp eq", args)?),
        "!=" => Some(binary_op(expr, "!=", "icmp ne", args)?),
        "<" => Some(binary_op(expr, "<", "icmp slt", args)?),
        ">" => Some(binary_op(expr, ">", "icmp sgt", args)?),
        "<=" => Some(binary_op(expr, "<=", "icmp sle", args)?),
        ">=" => Some(binary_op(expr, ">=", "icmp sge", args)?),

        // Boolean
        "not" => Some(unary_op(expr, "not", "xor (i1 1)", args)?),
        "and" => Some(binary_op(expr, "and", "and", args)?),
        "or" => Some(binary_op(expr, "or", "or", args)?),

        // Ownership operations
        "alloc" => {
            // (alloc type) - allocate owned value
            // For now, default to i64 if no type info available
            Some("(alloc own i64)".to_string())
        }
        "drop" => Some(unary_op(expr, "drop", "drop", args)?),
        "move" => Some(unary_op(expr, "move", "move", args)?),

        // Reference counting
        "rc-new" => {
            // (rc-new value) - create new RC pointer
            if args.len() != 1 {
                return Err(CompileError::codegen(
                    expr.span,
                    "rc-new requires exactly 1 argument",
                ));
            }
            let value = generate_expr(&args[0])?;
            // Allocate RC, store value, return pointer
            Some(format!(
                "(let ((_rc (rc-alloc i64))) (store {} (rc-ptr _rc)) _rc)",
                value
            ))
        }
        "rc-clone" => Some(unary_op(expr, "rc-clone", "rc-clone", args)?),
        "rc-drop" => Some(unary_op(expr, "rc-drop", "rc-drop", args)?),

        // Share and clone (high-level RC operations)
        "share" => {
            // (share value) - create reference-counted shared value
            // Same as rc-new: allocates RC, stores value, returns RC pointer
            if args.len() != 1 {
                return Err(CompileError::codegen(
                    expr.span,
                    "share requires exactly 1 argument",
                ));
            }
            let value = generate_expr(&args[0])?;
            Some(format!(
                "(let ((_rc (rc-alloc i64))) (store {} (rc-ptr _rc)) _rc)",
                value
            ))
        }
        "clone" => {
            // (clone value) - create a copy of the value
            // For RC values: increments refcount and returns alias
            // For primitives: just returns the value (copy semantics)
            if args.len() != 1 {
                return Err(CompileError::codegen(
                    expr.span,
                    "clone requires exactly 1 argument",
                ));
            }
            let value = generate_expr(&args[0])?;
            // For now, use rc-clone which handles both RC and primitive values
            // rc-clone on primitives is a no-op that returns the value
            Some(format!("(rc-clone {})", value))
        }

        // Array operations
        "array" | "make-array" => {
            // (array size) or (make-array size)
            if args.len() != 1 {
                return Err(CompileError::codegen(
                    expr.span,
                    "array requires exactly 1 argument (size)",
                ));
            }
            let size = generate_expr(&args[0])?;
            // For now, assume i64 elements and extract literal size
            // Real impl would need type info
            Some(format!("(array-alloc i64 {})", size))
        }
        "array-get" | "aget" => {
            // (array-get arr idx)
            if args.len() != 2 {
                return Err(CompileError::codegen(
                    expr.span,
                    "array-get requires 2 arguments (array, index)",
                ));
            }
            let arr = generate_expr(&args[0])?;
            let idx = generate_expr(&args[1])?;
            // Need size info - for now use placeholder
            Some(format!("(array-get i64 _SIZE {} {})", arr, idx))
        }
        "array-set" | "aset" => {
            // (array-set arr idx value)
            if args.len() != 3 {
                return Err(CompileError::codegen(
                    expr.span,
                    "array-set requires 3 arguments (array, index, value)",
                ));
            }
            let arr = generate_expr(&args[0])?;
            let idx = generate_expr(&args[1])?;
            let val = generate_expr(&args[2])?;
            Some(format!("(array-set i64 _SIZE {} {} {})", arr, idx, val))
        }
        "array-len" | "alen" => {
            // (array-len arr) - for sized arrays, returns compile-time size
            Some("(array-len _SIZE)".to_string())
        }

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
        generate(&program).unwrap()
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
        // Should return last value
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
        // share creates a reference-counted shared value
        let lir = compile("(defun make-shared () (share 42))");
        assert!(lir.contains("rc-alloc"), "share should use rc-alloc");
        assert!(lir.contains("rc-ptr"), "share should use rc-ptr");
    }

    #[test]
    fn test_clone() {
        // clone creates a copy (for RC, increments refcount)
        let lir = compile("(defun clone-it (x) (clone x))");
        assert!(lir.contains("rc-clone"), "clone should use rc-clone");
    }

    #[test]
    fn test_atom_create() {
        let lir = compile("(defun make-counter () (atom 0))");
        assert!(lir.contains("rc-alloc"), "atom should use rc-alloc");
        assert!(
            lir.contains("atomic-store"),
            "atom should use atomic-store for initialization"
        );
    }

    #[test]
    fn test_atom_deref() {
        // @atom reads the current value
        let lir = compile("(defun read-atom (a) @a)");
        assert!(
            lir.contains("atomic-load"),
            "atom deref should use atomic-load"
        );
        assert!(lir.contains("seq_cst"), "should use seq_cst ordering");
    }

    #[test]
    fn test_atom_reset() {
        let lir = compile("(defun set-atom (a v) (reset! a v))");
        assert!(
            lir.contains("atomic-store"),
            "reset! should use atomic-store"
        );
    }

    #[test]
    fn test_atom_swap() {
        let lir = compile("(defun inc-atom (a) (swap! a inc))");
        assert!(
            lir.contains("atomic-load"),
            "swap! should load current value"
        );
        assert!(
            lir.contains("call @inc"),
            "swap! should call update function"
        );
        assert!(lir.contains("atomic-store"), "swap! should store new value");
    }

    #[test]
    fn test_atom_compare_and_set() {
        let lir = compile("(defun cas-atom (a old new) (compare-and-set! a old new))");
        assert!(
            lir.contains("cmpxchg"),
            "compare-and-set! should use cmpxchg"
        );
        assert!(
            lir.contains("extractvalue"),
            "should extract success flag from cmpxchg result"
        );
    }

    #[test]
    fn test_vector_literal() {
        let lir = compile("(defun make-vec () [1 2 3])");
        assert!(lir.contains("(vec"));
        assert!(lir.contains("(i64 1)"));
        assert!(lir.contains("(i64 2)"));
        assert!(lir.contains("(i64 3)"));
    }

    #[test]
    fn test_map_literal() {
        let lir = compile("(defun make-map () {:a 1 :b 2})");
        assert!(lir.contains("(map"));
        assert!(lir.contains("(keyword a)"));
        assert!(lir.contains("(i64 1)"));
    }

    #[test]
    fn test_keyword_literal() {
        let lir = compile("(defun get-key () :foo)");
        assert!(lir.contains("(keyword foo)"));
    }

    #[test]
    fn test_async() {
        let lir = compile("(defun fetch-data () (async (compute)))");
        assert!(lir.contains("(future"));
    }

    #[test]
    fn test_await() {
        let lir = compile("(defun wait-data (f) (await f))");
        assert!(lir.contains("(await"));
    }

    #[test]
    fn test_conv_vector_literal() {
        let lir = compile("(defun make-conv-vec () <[1 2 3]>)");
        assert!(lir.contains("(conv-vec"));
        assert!(lir.contains("(i64 1)"));
        assert!(lir.contains("(i64 2)"));
        assert!(lir.contains("(i64 3)"));
    }

    #[test]
    fn test_conv_map_literal() {
        let lir = compile("(defun make-conv-map () <{:a 1 :b 2}>)");
        assert!(lir.contains("(conv-map"));
        assert!(lir.contains("(keyword a)"));
        assert!(lir.contains("(i64 1)"));
    }

    #[test]
    fn test_dosync() {
        let lir = compile("(defun transfer () (dosync (alter a - 50)))");
        assert!(lir.contains("(stm-dosync"));
    }

    #[test]
    fn test_ref_set() {
        let lir = compile("(defun set-val (r) (ref-set r 10))");
        assert!(lir.contains("(stm-ref-set"));
    }

    #[test]
    fn test_alter() {
        let lir = compile("(defun update (r) (alter r + 1))");
        assert!(lir.contains("(stm-alter"));
    }

    #[test]
    fn test_commute() {
        let lir = compile("(defun inc (r) (commute r + 1))");
        assert!(lir.contains("(stm-commute"));
    }

    #[test]
    fn test_simd_vector_int() {
        let lir = compile("(defun make-vec () <<1 2 3 4>>)");
        assert!(lir.contains("(vector <4 x i64>"));
        assert!(lir.contains("(i64 1)"));
        assert!(lir.contains("(i64 4)"));
    }

    #[test]
    fn test_simd_vector_float() {
        let lir = compile("(defun make-vec () <<1.0 2.0 3.0 4.0>>)");
        assert!(lir.contains("(vector <4 x double>"));
        assert!(lir.contains("(double 1")); // May be 1 or 1.0
    }
}

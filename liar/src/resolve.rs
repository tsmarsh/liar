//! Name resolution
//!
//! Resolves variable references to their definitions.
//! Detects undefined variables and duplicate definitions.

use std::collections::HashMap;

use crate::ast::{
    Def, Defmacro, Defprotocol, Defstruct, Defun, Expr, ExtendProtocol, Item, LetBinding,
    Namespace, Program, QualifiedName, RequireSpec,
};
use crate::error::{CompileError, Errors, Result};
use crate::span::{Span, Spanned};

/// Unique identifier for a binding
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BindingId(pub u32);

/// Information about a binding
#[derive(Debug, Clone)]
pub struct BindingInfo {
    pub kind: BindingKind,
    pub span: Span,
    pub id: BindingId,
}

/// Kind of binding
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingKind {
    Local,
    Parameter,
    Function,
    Constant,
    Struct,
    Protocol,
    ProtocolMethod,
    Macro,
    ExternFn,
}

/// A scope containing bindings
#[derive(Debug)]
struct Scope {
    bindings: HashMap<String, BindingInfo>,
}

impl Scope {
    fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    fn get(&self, name: &str) -> Option<&BindingInfo> {
        self.bindings.get(name)
    }

    fn insert(&mut self, name: String, info: BindingInfo) -> Option<BindingInfo> {
        self.bindings.insert(name, info)
    }
}

/// Namespace resolution context
#[derive(Debug, Default)]
pub struct NamespaceContext {
    /// Current namespace name (default: "user")
    pub current: String,
    /// Alias -> full module name (from :as)
    pub aliases: HashMap<String, String>,
    /// Symbol -> source module (from :refer [sym])
    pub referred: HashMap<String, String>,
    /// Modules imported with :refer :all
    pub refer_all: Vec<String>,
}

impl NamespaceContext {
    fn new() -> Self {
        Self {
            current: "user".to_string(),
            aliases: HashMap::new(),
            referred: HashMap::new(),
            // Auto-import liar.core
            refer_all: vec!["liar.core".to_string()],
        }
    }

    /// Process a namespace declaration
    fn process_namespace(&mut self, ns: &Namespace) {
        self.current = ns.name.node.clone();

        for req in &ns.requires {
            match req {
                RequireSpec::Alias { module, alias } => {
                    self.aliases.insert(alias.node.clone(), module.node.clone());
                }
                RequireSpec::Refer { module, symbols } => {
                    for sym in symbols {
                        self.referred.insert(sym.node.clone(), module.node.clone());
                    }
                }
                RequireSpec::ReferAll { module } => {
                    self.refer_all.push(module.node.clone());
                }
                RequireSpec::Bare { module: _ } => {
                    // Bare requires just load the module, no imports
                }
            }
        }
    }
}

/// Name resolver state
pub struct Resolver {
    scopes: Vec<Scope>,
    errors: Errors,
    next_id: u32,
    /// Map from binding use sites to their definitions
    pub resolutions: HashMap<Span, BindingId>,
    /// Namespace context for qualified name resolution
    ns_ctx: NamespaceContext,
}

/// List of builtin functions that are always in scope
const BUILTINS: &[&str] = &[
    // Arithmetic (integer)
    "+",
    "-",
    "*",
    "/",
    "rem",
    // Arithmetic (float)
    "+.",
    "-.",
    "*.",
    "/.",
    "%.",
    "fadd",
    "fsub",
    "fmul",
    "fdiv",
    "frem",
    // Comparison (integer)
    "=",
    "==",
    "!=",
    "<",
    ">",
    "<=",
    ">=",
    // Comparison (float)
    "=.",
    "!=.",
    "<.",
    ">.",
    "<=.",
    ">=.",
    "f=",
    "f!=",
    "f<",
    "f>",
    "f<=",
    "f>=",
    // Boolean
    "not",
    "and",
    "or",
    // Bitwise operations
    "bit-and",
    "bit-or",
    "bit-xor",
    "bit-not",
    "bit-shift-left",
    "bit-shift-right",
    "shl",
    "shr",
    "arithmetic-shift-right",
    "ashr",
    "popcount",
    // Ownership/memory
    "alloc",
    "drop",
    "move",
    "share",
    "clone",
    "rc-new",
    "rc-clone",
    "rc-drop",
    // Arrays
    "array",
    "make-array",
    "array-get",
    "aget",
    "array-set",
    "aset",
    "array-len",
    "alen",
    // Heap arrays for persistent data structures
    "heap-array",
    "array-copy",
    // I/O (not yet implemented but reserved)
    "print",
    "println",
    // Type conversions
    "int",
    "float",
    "string",
    // Integer width conversions
    "trunc",
    "zext",
    "sext",
    // Float precision conversions
    "fptrunc",
    "fpext",
    // Float <-> int conversions
    "fptosi",
    "fptoui",
    "sitofp",
    "uitofp",
    // Nil check
    "nil?",
    // Byte operations (for string manipulation)
    "store-byte",
    "load-byte",
    // Pointer arithmetic
    "ptr+",
    // Type names (used as arguments to conversion functions)
    "i1",
    "i8",
    "i16",
    "i32",
    "i64",
    "double",
];

impl Resolver {
    pub fn new() -> Self {
        let mut resolver = Self {
            scopes: vec![Scope::new()],
            errors: Errors::new(),
            next_id: 0,
            resolutions: HashMap::new(),
            ns_ctx: NamespaceContext::new(),
        };

        // Define all builtins
        for &name in BUILTINS {
            resolver.define(name, Span::default(), BindingKind::Function);
        }

        resolver
    }

    fn fresh_id(&mut self) -> BindingId {
        let id = BindingId(self.next_id);
        self.next_id += 1;
        id
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn current_scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("no scope")
    }

    fn define(&mut self, name: &str, span: Span, kind: BindingKind) -> BindingId {
        let id = self.fresh_id();
        let info = BindingInfo { kind, span, id };

        // Check for duplicate in current scope only
        let existing_span = self
            .scopes
            .last()
            .and_then(|s| s.get(name))
            .map(|info| info.span);

        if let Some(existing) = existing_span {
            self.errors.push(CompileError::resolve(
                span,
                format!(
                    "duplicate definition of '{}' (previously defined at {}..{})",
                    name, existing.start, existing.end
                ),
            ));
        }

        self.current_scope().insert(name.to_string(), info);
        id
    }

    fn lookup(&mut self, name: &str, span: Span) -> Option<BindingId> {
        // Search from innermost to outermost scope
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(name) {
                self.resolutions.insert(span, info.id);
                return Some(info.id);
            }
        }

        self.errors.push(CompileError::resolve(
            span,
            format!("undefined variable: '{}'", name),
        ));
        None
    }

    /// Resolve a qualified name (handles namespace/symbol syntax)
    fn resolve_qualified_name(&mut self, qname: &QualifiedName, span: Span) -> Option<BindingId> {
        match &qname.qualifier {
            Some(qualifier) => {
                // Qualified name: first resolve the namespace alias
                let _resolved_ns = self
                    .ns_ctx
                    .aliases
                    .get(qualifier)
                    .cloned()
                    .unwrap_or_else(|| qualifier.clone());

                // For now, just look up the simple name
                // TODO: When multi-file loading is implemented, look up in the resolved namespace
                // For now, qualified references to other modules won't work until Phase 4
                self.lookup(&qname.name, span)
            }
            None => {
                // Unqualified name: try local scopes first
                for scope in self.scopes.iter().rev() {
                    if let Some(info) = scope.get(&qname.name) {
                        self.resolutions.insert(span, info.id);
                        return Some(info.id);
                    }
                }

                // Try referred symbols (from :refer)
                if self.ns_ctx.referred.contains_key(&qname.name) {
                    // Symbol was explicitly imported - look it up
                    return self.lookup(&qname.name, span);
                }

                // Fall back to regular lookup (includes builtins)
                self.lookup(&qname.name, span)
            }
        }
    }

    /// Resolve names in a program
    pub fn resolve(
        mut self,
        program: &Program,
    ) -> std::result::Result<ResolvedProgram, Vec<CompileError>> {
        // First pass: collect all top-level definitions (for forward references)
        for item in &program.items {
            match &item.node {
                Item::Defun(defun) => {
                    self.define(&defun.name.node, defun.name.span, BindingKind::Function);
                }
                Item::Def(def) => {
                    self.define(&def.name.node, def.name.span, BindingKind::Constant);
                }
                Item::Defstruct(defstruct) => {
                    self.define(
                        &defstruct.name.node,
                        defstruct.name.span,
                        BindingKind::Struct,
                    );
                }
                Item::Defprotocol(defprotocol) => {
                    self.define(
                        &defprotocol.name.node,
                        defprotocol.name.span,
                        BindingKind::Protocol,
                    );
                    // Also register all method names as callable
                    for method in &defprotocol.methods {
                        self.define(
                            &method.name.node,
                            method.name.span,
                            BindingKind::ProtocolMethod,
                        );
                    }
                }
                Item::ExtendProtocol(_) => {
                    // extend-protocol doesn't define a new name
                }
                Item::ExtendProtocolDefault(_) => {
                    // extend-protocol-default doesn't define a new name
                }
                Item::Defmacro(defmacro) => {
                    self.define(&defmacro.name.node, defmacro.name.span, BindingKind::Macro);
                }
                Item::Extern(ext) => {
                    self.define(&ext.name.node, ext.name.span, BindingKind::ExternFn);
                }
                Item::Namespace(ns) => {
                    // Process namespace declaration to set up imports
                    self.ns_ctx.process_namespace(ns);
                }
            }
        }

        // Second pass: resolve all references within bodies
        for item in &program.items {
            self.resolve_item(item);
        }

        self.errors.into_result(ResolvedProgram {
            resolutions: self.resolutions,
        })
    }

    fn resolve_item(&mut self, item: &Spanned<Item>) {
        match &item.node {
            Item::Defun(defun) => self.resolve_defun(defun),
            Item::Def(def) => self.resolve_def(def),
            Item::Defstruct(defstruct) => self.resolve_defstruct(defstruct),
            Item::Defprotocol(defprotocol) => self.resolve_defprotocol(defprotocol),
            Item::ExtendProtocol(extend) => self.resolve_extend_protocol(extend),
            Item::ExtendProtocolDefault(extend) => {
                // Verify both protocols exist
                self.lookup(&extend.protocol.node, extend.protocol.span);
                self.lookup(&extend.source_protocol.node, extend.source_protocol.span);

                // Resolve method implementations
                for method in &extend.implementations {
                    self.push_scope();
                    for param in &method.params {
                        self.define(&param.node, param.span, BindingKind::Parameter);
                    }
                    self.resolve_expr(&method.body);
                    self.pop_scope();
                }
            }
            Item::Defmacro(defmacro) => self.resolve_defmacro(defmacro),
            Item::Extern(_) => {
                // Extern declarations have no body to resolve
            }
            Item::Namespace(_) => {
                // Namespace declarations are handled in a separate phase
            }
        }
    }

    fn resolve_defmacro(&mut self, defmacro: &Defmacro) {
        self.push_scope();

        // Define parameters
        for param in &defmacro.params {
            self.define(&param.node, param.span, BindingKind::Parameter);
        }

        // Resolve body
        self.resolve_expr(&defmacro.body);

        self.pop_scope();
    }

    fn resolve_defun(&mut self, defun: &Defun) {
        self.push_scope();

        // Define parameters
        for param in &defun.params {
            self.define(&param.name.node, param.name.span, BindingKind::Parameter);
        }

        // Resolve body
        self.resolve_expr(&defun.body);

        self.pop_scope();
    }

    fn resolve_def(&mut self, def: &Def) {
        self.resolve_expr(&def.value);
    }

    fn resolve_defstruct(&mut self, _defstruct: &Defstruct) {
        // Struct definitions don't have expressions to resolve
        // Field types are resolved during type checking
    }

    fn resolve_defprotocol(&mut self, _defprotocol: &Defprotocol) {
        // Protocol definitions only contain method signatures
        // Method names and parameters are not resolved as expressions
    }

    fn resolve_extend_protocol(&mut self, extend: &ExtendProtocol) {
        // Verify the protocol exists
        self.lookup(&extend.protocol.node, extend.protocol.span);

        // Verify the type exists (optional - could be a built-in type)
        // For now we just resolve method bodies

        // Resolve method implementations
        for method in &extend.implementations {
            self.push_scope();
            // Define parameters including self
            for param in &method.params {
                self.define(&param.node, param.span, BindingKind::Parameter);
            }
            self.resolve_expr(&method.body);
            self.pop_scope();
        }
    }

    fn resolve_expr(&mut self, expr: &Spanned<Expr>) {
        match &expr.node {
            Expr::Int(_) | Expr::Float(_) | Expr::Bool(_) | Expr::String(_) | Expr::Nil => {
                // Literals have no names to resolve
            }

            Expr::Var(name) => {
                self.resolve_qualified_name(name, expr.span);
            }

            Expr::Call(func, args) => {
                self.resolve_expr(func);
                for arg in args {
                    self.resolve_expr(arg);
                }
            }

            Expr::Lambda(params, body) => {
                self.push_scope();
                for param in params {
                    self.define(&param.name.node, param.name.span, BindingKind::Parameter);
                }
                self.resolve_expr(body);
                self.pop_scope();
            }

            Expr::Let(bindings, body) => {
                self.push_scope();
                for binding in bindings {
                    self.resolve_let_binding(binding);
                }
                self.resolve_expr(body);
                self.pop_scope();
            }

            Expr::Plet(bindings, body) => {
                // Same as let for name resolution purposes
                self.push_scope();
                for binding in bindings {
                    self.resolve_let_binding(binding);
                }
                self.resolve_expr(body);
                self.pop_scope();
            }

            Expr::If(cond, then_, else_) => {
                self.resolve_expr(cond);
                self.resolve_expr(then_);
                self.resolve_expr(else_);
            }

            Expr::Do(exprs) => {
                for expr in exprs {
                    self.resolve_expr(expr);
                }
            }

            Expr::Set(name, value) => {
                self.lookup(&name.node, name.span);
                self.resolve_expr(value);
            }

            Expr::Ref(inner) | Expr::RefMut(inner) | Expr::Deref(inner) | Expr::Unsafe(inner) => {
                self.resolve_expr(inner);
            }

            Expr::Struct(name, fields) => {
                // The struct name should be defined
                self.lookup(name, expr.span);
                for (_, value) in fields {
                    self.resolve_expr(value);
                }
            }

            Expr::Field(obj, _field) => {
                self.resolve_expr(obj);
                // Field names are resolved during type checking
            }

            Expr::Quote(_) => {
                // Quoted symbols don't need resolution
            }

            // Atom expressions (ADR-011)
            Expr::Atom(value) => {
                self.resolve_expr(value);
            }
            Expr::AtomDeref(atom) => {
                self.resolve_expr(atom);
            }
            Expr::Reset(atom, value) => {
                self.resolve_expr(atom);
                self.resolve_expr(value);
            }
            Expr::Swap(atom, func) => {
                self.resolve_expr(atom);
                self.resolve_expr(func);
            }
            Expr::CompareAndSet { atom, old, new } => {
                self.resolve_expr(atom);
                self.resolve_expr(old);
                self.resolve_expr(new);
            }

            // Persistent collections (ADR-018)
            Expr::Vector(elements) => {
                for elem in elements {
                    self.resolve_expr(elem);
                }
            }
            Expr::Map(pairs) => {
                for (k, v) in pairs {
                    self.resolve_expr(k);
                    self.resolve_expr(v);
                }
            }
            Expr::Keyword(_) => {}

            // Conventional mutable collections (ADR-018)
            Expr::ConvVector(elements) => {
                for elem in elements {
                    self.resolve_expr(elem);
                }
            }
            Expr::ConvMap(pairs) => {
                for (k, v) in pairs {
                    self.resolve_expr(k);
                    self.resolve_expr(v);
                }
            }

            // Async/await (ADR-014)
            Expr::Async(body) => {
                self.resolve_expr(body);
            }
            Expr::Await(future) => {
                self.resolve_expr(future);
            }

            // SIMD vectors (ADR-016)
            Expr::SimdVector(elements) => {
                for elem in elements {
                    self.resolve_expr(elem);
                }
            }

            // STM (ADR-012)
            Expr::Dosync(exprs) => {
                for expr in exprs {
                    self.resolve_expr(expr);
                }
            }
            Expr::RefSetStm(ref_expr, value) => {
                self.resolve_expr(ref_expr);
                self.resolve_expr(value);
            }
            Expr::Alter {
                ref_expr,
                fn_expr,
                args,
            } => {
                self.resolve_expr(ref_expr);
                self.resolve_expr(fn_expr);
                for arg in args {
                    self.resolve_expr(arg);
                }
            }
            Expr::Commute {
                ref_expr,
                fn_expr,
                args,
            } => {
                self.resolve_expr(ref_expr);
                self.resolve_expr(fn_expr);
                for arg in args {
                    self.resolve_expr(arg);
                }
            }

            // Iterators
            Expr::Iter(coll) => {
                self.resolve_expr(coll);
            }
            Expr::Collect(iter) => {
                self.resolve_expr(iter);
            }

            // Byte arrays and regex are literals - no resolution needed
            Expr::ByteArray(_) | Expr::Regex { .. } => {}

            // Overflow handling - recurse into inner expression
            Expr::Boxed(inner) | Expr::Wrapping(inner) => {
                self.resolve_expr(inner);
            }

            // Macro expressions - should be expanded before resolution, but handle them gracefully
            Expr::Quasiquote(inner) | Expr::Unquote(inner) | Expr::UnquoteSplicing(inner) => {
                self.resolve_expr(inner);
            }
            Expr::Gensym(_) => {
                // Gensym generates a unique symbol - no resolution needed
            }

            // Generated by closure conversion pass
            Expr::ClosureLit { env, .. } => {
                if let Some(e) = env {
                    self.resolve_expr(e);
                }
            }
            Expr::HeapEnvAlloc { fields, .. } | Expr::StackEnvAlloc { fields, .. } => {
                for (_, value) in fields {
                    self.resolve_expr(value);
                }
            }
        }
    }

    fn resolve_let_binding(&mut self, binding: &LetBinding) {
        // Resolve value first (in outer scope, before the binding is visible)
        // Note: The value is already resolved before we add the binding
        // But we need to handle recursive lets differently if desired
        self.resolve_expr(&binding.value);

        // Then define the name
        self.define(&binding.name.node, binding.name.span, BindingKind::Local);
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Self::new()
    }
}

/// The result of name resolution
#[derive(Debug)]
pub struct ResolvedProgram {
    /// Map from use sites (spans) to their binding definitions
    pub resolutions: HashMap<Span, BindingId>,
}

/// Resolve names in a program
pub fn resolve(program: &Program) -> Result<()> {
    let resolver = Resolver::new();
    match resolver.resolve(program) {
        Ok(_) => Ok(()),
        Err(errors) => Err(errors.into_iter().next().expect("at least one error")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn resolve_source(source: &str) -> std::result::Result<ResolvedProgram, Vec<CompileError>> {
        let mut parser = Parser::new(source).expect("lexer failed");
        let program = parser.parse_program().expect("parser failed");
        Resolver::new().resolve(&program)
    }

    #[test]
    fn test_simple_function() {
        let result = resolve_source(
            r#"
            (defun add (a b)
              (+ a b))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_undefined_variable() {
        let result = resolve_source(
            r#"
            (defun foo ()
              undefined)
            "#,
        );
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors[0].message.contains("undefined variable"));
    }

    #[test]
    fn test_let_binding() {
        let result = resolve_source(
            r#"
            (defun foo ()
              (let ((x 1))
                x))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_nested_scope() {
        let result = resolve_source(
            r#"
            (defun foo ()
              (let ((x 1))
                (let ((y 2))
                  (+ x y))))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_forward_reference() {
        // Functions can reference each other (forward references)
        let result = resolve_source(
            r#"
            (defun foo ()
              (bar))
            (defun bar ()
              (foo))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_duplicate_definition() {
        let result = resolve_source(
            r#"
            (defun foo () 1)
            (defun foo () 2)
            "#,
        );
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors[0].message.contains("duplicate definition"));
    }

    #[test]
    fn test_lambda_scope() {
        let result = resolve_source(
            r#"
            (defun foo ()
              (let ((f (fn (x) x)))
                (f 1)))
            "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_lambda_captures() {
        let result = resolve_source(
            r#"
            (defun foo ()
              (let ((x 1))
                (let ((f (fn (y) (+ x y))))
                  (f 2))))
            "#,
        );
        assert!(result.is_ok());
    }
}

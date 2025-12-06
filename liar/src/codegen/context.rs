//! Codegen context - replaces thread-local state with explicit context
//!
//! All codegen functions take `&mut CodegenContext` as their first parameter.

use lir_core::ast as lir;
use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

/// Struct field information for codegen
#[derive(Clone, Debug)]
pub struct StructInfo {
    pub fields: Vec<(String, lir::ParamType)>,
}

// Fresh variable counter for generating unique temporaries
static VAR_COUNTER: AtomicUsize = AtomicUsize::new(0);

/// Codegen context containing all state needed during code generation
pub struct CodegenContext {
    /// Function return types for type inference
    pub func_return_types: HashMap<String, lir::ReturnType>,
    /// Struct definitions
    pub struct_defs: HashMap<String, StructInfo>,
    /// Variable to struct type mapping (for field access)
    pub var_struct_types: HashMap<String, String>,
    /// Protocol method to protocol name mapping
    pub protocol_methods: HashMap<String, String>,
    /// Protocol implementations: (type_name, method_name) -> impl_fn_name
    pub protocol_impls: HashMap<(String, String), String>,
    /// Whether malloc declaration is needed
    pub needs_malloc: bool,
}

impl Default for CodegenContext {
    fn default() -> Self {
        Self::new()
    }
}

impl CodegenContext {
    /// Create a new empty context
    pub fn new() -> Self {
        Self {
            func_return_types: HashMap::new(),
            struct_defs: HashMap::new(),
            var_struct_types: HashMap::new(),
            protocol_methods: HashMap::new(),
            protocol_impls: HashMap::new(),
            needs_malloc: false,
        }
    }

    /// Reset all context state (for testing)
    pub fn reset(&mut self) {
        self.func_return_types.clear();
        self.struct_defs.clear();
        self.var_struct_types.clear();
        self.protocol_methods.clear();
        self.protocol_impls.clear();
        self.needs_malloc = false;
    }

    /// Generate a fresh variable name with the given prefix
    pub fn fresh_var(&self, prefix: &str) -> String {
        let n = VAR_COUNTER.fetch_add(1, Ordering::SeqCst);
        format!("_{}_{}", prefix, n)
    }

    /// Register a struct definition
    pub fn register_struct(&mut self, name: &str, info: StructInfo) {
        self.struct_defs.insert(name.to_string(), info);
    }

    /// Look up a struct definition
    pub fn lookup_struct(&self, name: &str) -> Option<&StructInfo> {
        self.struct_defs.get(name)
    }

    /// Register a variable's struct type
    pub fn register_var_struct_type(&mut self, var_name: &str, struct_name: &str) {
        self.var_struct_types
            .insert(var_name.to_string(), struct_name.to_string());
    }

    /// Look up a variable's struct type
    pub fn lookup_var_struct_type(&self, var_name: &str) -> Option<&String> {
        self.var_struct_types.get(var_name)
    }

    /// Register a protocol method
    pub fn register_protocol_method(&mut self, method_name: &str, protocol_name: &str) {
        self.protocol_methods
            .insert(method_name.to_string(), protocol_name.to_string());
    }

    /// Check if a name is a protocol method
    pub fn is_protocol_method(&self, name: &str) -> Option<&String> {
        self.protocol_methods.get(name)
    }

    /// Register a protocol implementation
    pub fn register_protocol_impl(
        &mut self,
        type_name: &str,
        method_name: &str,
        impl_fn_name: &str,
    ) {
        self.protocol_impls.insert(
            (type_name.to_string(), method_name.to_string()),
            impl_fn_name.to_string(),
        );
    }

    /// Look up a protocol implementation
    pub fn lookup_protocol_impl(&self, type_name: &str, method_name: &str) -> Option<&String> {
        self.protocol_impls
            .get(&(type_name.to_string(), method_name.to_string()))
    }

    /// Register a function's return type
    pub fn register_func_return_type(&mut self, name: &str, return_type: lir::ReturnType) {
        self.func_return_types.insert(name.to_string(), return_type);
    }

    /// Look up a function's return type
    pub fn lookup_func_return_type(&self, name: &str) -> Option<&lir::ReturnType> {
        self.func_return_types.get(name)
    }

    /// Mark that malloc is needed
    pub fn set_needs_malloc(&mut self) {
        self.needs_malloc = true;
    }

    /// Take the needs_malloc flag (returns true if set, then clears it)
    pub fn take_needs_malloc(&mut self) -> bool {
        std::mem::take(&mut self.needs_malloc)
    }
}

/// Reset the variable counter (for testing)
pub fn reset_var_counter() {
    VAR_COUNTER.store(0, Ordering::SeqCst);
}

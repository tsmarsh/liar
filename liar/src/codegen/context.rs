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
    /// Type ID counter for runtime type dispatch
    type_id_counter: i64,
    /// Struct name to type ID mapping
    struct_type_ids: HashMap<String, i64>,

    // Block management for multi-block functions (if with br/phi)
    /// Completed basic blocks
    blocks: Vec<lir::BasicBlock>,
    /// Label of the current block being built
    current_block_label: String,
    /// Counter for generating unique block labels
    block_counter: usize,
    /// Pending phi bindings that should be emitted at the next block end
    /// These are phi nodes from if expressions that must be placed at the start
    /// of the current block before any other instructions
    pending_phis: Vec<(String, Box<lir::Expr>)>,
    /// Entry bindings that must be emitted BEFORE any branches in the entry block
    /// Used for closure env access - these bindings need to be available in all branches
    entry_bindings: Vec<(String, Box<lir::Expr>)>,
    /// Whether the current expression is in tail position
    /// When true, direct function calls should be generated as tail calls
    in_tail_position: bool,
    /// Whether tail calls can be emitted (false inside if branches)
    /// Tail calls are terminators and can't be used inside expressions
    can_emit_tailcall: bool,
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
            type_id_counter: 1, // 0 reserved for nil
            struct_type_ids: HashMap::new(),
            blocks: Vec::new(),
            current_block_label: "entry".to_string(),
            block_counter: 0,
            pending_phis: Vec::new(),
            entry_bindings: Vec::new(),
            in_tail_position: false,
            can_emit_tailcall: true,
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
        self.type_id_counter = 1;
        self.struct_type_ids.clear();
        self.blocks.clear();
        self.current_block_label = "entry".to_string();
        self.block_counter = 0;
        self.pending_phis.clear();
        self.entry_bindings.clear();
        self.in_tail_position = false;
        self.can_emit_tailcall = true;
    }

    // ========== Block Management ==========

    /// Initialize for a new function - clears blocks and starts at "entry"
    pub fn start_function(&mut self) {
        self.blocks.clear();
        self.current_block_label = "entry".to_string();
        self.block_counter = 0;
        self.pending_phis.clear();
        self.entry_bindings.clear();
        self.in_tail_position = false;
        self.can_emit_tailcall = true;
    }

    /// Generate a unique block label with the given prefix
    pub fn fresh_block(&mut self, prefix: &str) -> String {
        self.block_counter += 1;
        format!("{}_{}", prefix, self.block_counter)
    }

    /// Get the current block label
    pub fn current_block(&self) -> &str {
        &self.current_block_label
    }

    /// End the current block with a terminator and start a new block
    /// Returns the completed block
    /// If there are pending phi bindings, they are wrapped around the terminator
    /// If this is the entry block and there are entry bindings, they are emitted first
    pub fn end_block(&mut self, terminator: lir::Expr) -> lir::BasicBlock {
        // Take any pending phi bindings
        let pending = std::mem::take(&mut self.pending_phis);

        // If there are pending phis, wrap them around the terminator
        let mut instr = if pending.is_empty() {
            terminator
        } else {
            lir::Expr::Let {
                bindings: pending,
                body: vec![terminator],
            }
        };

        // If this is the entry block and there are entry bindings, wrap those first
        // Entry bindings are for captured variables that must be available in all branches
        if self.current_block_label == "entry" && !self.entry_bindings.is_empty() {
            let entry = std::mem::take(&mut self.entry_bindings);
            instr = lir::Expr::Let {
                bindings: entry,
                body: vec![instr],
            };
        }

        let block = lir::BasicBlock {
            label: self.current_block_label.clone(),
            instructions: vec![instr],
        };
        self.blocks.push(block.clone());
        block
    }

    /// Start a new block with the given label
    pub fn start_block(&mut self, label: &str) {
        self.current_block_label = label.to_string();
    }

    /// Add a phi binding to be emitted at the start of the current block
    /// when the block is ended. Returns the variable name for the phi.
    pub fn add_pending_phi(&mut self, var_name: String, phi_expr: lir::Expr) {
        self.pending_phis.push((var_name, Box::new(phi_expr)));
    }

    /// Take pending phi bindings (for finalizing the last block)
    pub fn take_pending_phis(&mut self) -> Vec<(String, Box<lir::Expr>)> {
        std::mem::take(&mut self.pending_phis)
    }

    /// Add an entry binding - these are emitted at the START of the entry block
    /// before any branches. Used for closure captured variable extraction.
    pub fn add_entry_binding(&mut self, name: String, value: lir::Expr) {
        self.entry_bindings.push((name, Box::new(value)));
    }

    /// Take entry bindings (for single-block functions that don't use end_block)
    pub fn take_entry_bindings(&mut self) -> Vec<(String, Box<lir::Expr>)> {
        std::mem::take(&mut self.entry_bindings)
    }

    /// Take all completed blocks (for function finalization)
    pub fn take_blocks(&mut self) -> Vec<lir::BasicBlock> {
        std::mem::take(&mut self.blocks)
    }

    /// Check if any blocks have been emitted (indicates multi-block function)
    pub fn has_blocks(&self) -> bool {
        !self.blocks.is_empty()
    }

    // ========== Tail Call Optimization ==========

    /// Check if we're currently in tail position
    pub fn is_tail_position(&self) -> bool {
        self.in_tail_position
    }

    /// Set tail position and return the previous value
    /// Use this to temporarily enter tail position:
    /// ```ignore
    /// let was_tail = ctx.set_tail_position(true);
    /// // generate body in tail position
    /// ctx.set_tail_position(was_tail);
    /// ```
    pub fn set_tail_position(&mut self, tail: bool) -> bool {
        let old = self.in_tail_position;
        self.in_tail_position = tail;
        old
    }

    /// Check if tail calls can be emitted
    /// Returns false inside if branches where tailcall would break phi nodes
    pub fn can_emit_tailcall(&self) -> bool {
        self.can_emit_tailcall
    }

    /// Set whether tail calls can be emitted and return the previous value
    pub fn set_can_emit_tailcall(&mut self, can: bool) -> bool {
        let old = self.can_emit_tailcall;
        self.can_emit_tailcall = can;
        old
    }

    /// Generate a fresh variable name with the given prefix
    pub fn fresh_var(&self, prefix: &str) -> String {
        let n = VAR_COUNTER.fetch_add(1, Ordering::SeqCst);
        format!("_{}_{}", prefix, n)
    }

    /// Register a struct definition and assign it a type ID
    pub fn register_struct(&mut self, name: &str, info: StructInfo) {
        self.struct_defs.insert(name.to_string(), info);
        // Assign a unique type ID (0 is reserved for nil)
        let type_id = self.type_id_counter;
        self.type_id_counter += 1;
        self.struct_type_ids.insert(name.to_string(), type_id);
    }

    /// Look up a struct definition
    pub fn lookup_struct(&self, name: &str) -> Option<&StructInfo> {
        self.struct_defs.get(name)
    }

    /// Get the type ID for a struct (for runtime dispatch)
    pub fn get_struct_type_id(&self, name: &str) -> Option<i64> {
        self.struct_type_ids.get(name).copied()
    }

    /// Get all type implementations for a protocol method
    /// Returns Vec of (type_name, type_id, impl_fn_name)
    pub fn get_method_implementations(&self, method_name: &str) -> Vec<(String, i64, String)> {
        let mut impls = Vec::new();
        for ((type_name, method), impl_fn) in &self.protocol_impls {
            if method == method_name {
                if let Some(type_id) = self.struct_type_ids.get(type_name) {
                    impls.push((type_name.clone(), *type_id, impl_fn.clone()));
                }
            }
        }
        impls
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

    /// Find which struct has a given field (for inferring struct type from field access)
    /// Skips __type_id which is present in all structs
    pub fn find_struct_with_field(&self, field_name: &str) -> Option<String> {
        for (struct_name, info) in &self.struct_defs {
            for (name, _ty) in &info.fields {
                // Skip the internal type_id field
                if name == "__type_id" {
                    continue;
                }
                if name == field_name {
                    return Some(struct_name.clone());
                }
            }
        }
        None
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

//! Module loader for namespace-based compilation
//!
//! Handles loading modules from files based on namespace names.
//! Namespace `my.namespace` maps to file `my.namespace.liar`.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use crate::ast::{Item, Program, RequireSpec, Target};
use crate::error::{CompileError, Result};
use crate::parser::Parser;
use crate::span::Span;

/// Module loader with dependency resolution
pub struct ModuleLoader {
    /// Directories to search for modules
    search_paths: Vec<PathBuf>,
    /// Cached parsed programs by namespace
    cache: HashMap<String, Program>,
    /// Namespaces currently being loaded (for cycle detection)
    loading: HashSet<String>,
    /// Order in which modules were loaded (for correct compilation order)
    load_order: Vec<String>,
}

impl ModuleLoader {
    /// Create a new module loader with given search paths
    pub fn new(search_paths: Vec<PathBuf>) -> Self {
        Self {
            search_paths,
            cache: HashMap::new(),
            loading: HashSet::new(),
            load_order: Vec::new(),
        }
    }

    /// Find the file for a given namespace
    /// `my.namespace` -> `my.namespace.liar`
    fn find_module(&self, namespace: &str) -> Option<PathBuf> {
        let filename = format!("{}.liar", namespace);
        for dir in &self.search_paths {
            let path = dir.join(&filename);
            if path.exists() {
                return Some(path);
            }
        }
        None
    }

    /// Load a module and its dependencies
    pub fn load(&mut self, namespace: &str, span: Span) -> Result<()> {
        // Already loaded?
        if self.cache.contains_key(namespace) {
            return Ok(());
        }

        // Cycle detection
        if self.loading.contains(namespace) {
            return Err(CompileError::resolve(
                span,
                format!("Circular dependency detected: {}", namespace),
            ));
        }

        // Find the module file
        let path = self.find_module(namespace).ok_or_else(|| {
            CompileError::resolve(span, format!("Module not found: {}", namespace))
        })?;

        // Mark as loading
        self.loading.insert(namespace.to_string());

        // Read and parse the file
        let source = fs::read_to_string(&path).map_err(|e| {
            CompileError::resolve(span, format!("Failed to read {}: {}", path.display(), e))
        })?;

        let mut parser = Parser::new(&source)?;
        let program = parser.parse_program()?;

        // Load dependencies first
        self.load_dependencies(&program, span)?;

        // Done loading this module
        self.loading.remove(namespace);
        self.cache.insert(namespace.to_string(), program);
        self.load_order.push(namespace.to_string());

        Ok(())
    }

    /// Load all dependencies from a program's namespace declaration
    fn load_dependencies(&mut self, program: &Program, span: Span) -> Result<()> {
        // Find namespace declaration
        for item in &program.items {
            if let Item::Namespace(ns) = &item.node {
                for req in &ns.requires {
                    let dep_ns = match req {
                        RequireSpec::Alias { module, .. } => &module.node,
                        RequireSpec::Refer { module, .. } => &module.node,
                        RequireSpec::ReferAll { module } => &module.node,
                        RequireSpec::Bare { module } => &module.node,
                    };
                    // Recursively load the dependency
                    self.load(dep_ns, span)?;
                }
                // Only process first namespace declaration
                break;
            }
        }
        Ok(())
    }

    /// Get all loaded programs merged in dependency order
    ///
    /// Returns a single program with all items from all loaded modules,
    /// ordered so that dependencies come before dependents.
    pub fn into_merged_program(self) -> Program {
        let mut items = Vec::new();

        // Process modules in load order (dependencies first)
        for namespace in self.load_order {
            if let Some(program) = self.cache.get(&namespace) {
                items.extend(program.items.clone());
            }
        }

        Program { items }
    }

    /// Get modules with their namespace names preserved
    ///
    /// Returns (namespace, program) pairs in dependency order.
    /// This allows the resolver to maintain per-module scopes.
    pub fn into_modules(self) -> Vec<(String, Program)> {
        self.load_order
            .into_iter()
            .filter_map(|ns| self.cache.get(&ns).map(|p| (ns, p.clone())))
            .collect()
    }

    /// Get the namespace name from a program (if it has one)
    pub fn extract_namespace(program: &Program) -> Option<String> {
        for item in &program.items {
            if let Item::Namespace(ns) = &item.node {
                return Some(ns.name.node.clone());
            }
        }
        None
    }
}

/// Compile a file with module resolution
///
/// Loads the file and all its dependencies, then compiles them together.
pub fn compile_file(
    path: &Path,
    lib_paths: &[PathBuf],
) -> std::result::Result<String, Vec<CompileError>> {
    // Build search paths: file's directory + lib paths
    let mut search_paths = Vec::new();
    if let Some(parent) = path.parent() {
        search_paths.push(parent.to_path_buf());
    }
    search_paths.extend(lib_paths.iter().cloned());

    // Read and parse the main file
    let source = fs::read_to_string(path).map_err(|e| {
        vec![CompileError::resolve(
            Span::new(0, 0),
            format!("Failed to read {}: {}", path.display(), e),
        )]
    })?;

    let mut parser = Parser::new(&source).map_err(|e| vec![e])?;
    let program = parser.parse_program().map_err(|e| vec![e])?;

    // Check if this file has a namespace
    let namespace = ModuleLoader::extract_namespace(&program);

    if let Some(ns) = namespace {
        // Load dependencies via ModuleLoader
        let mut loader = ModuleLoader::new(search_paths);

        // Pre-seed the cache with the already-parsed main file
        // This avoids re-reading and parsing it
        loader.cache.insert(ns.clone(), program.clone());

        // Load dependencies first (they will be added to load_order)
        loader
            .load_dependencies(&program, Span::new(0, 0))
            .map_err(|e| vec![e])?;

        // Add main module to load_order AFTER dependencies
        loader.load_order.push(ns);

        // Get merged program
        let merged = loader.into_merged_program();

        // Compile the merged program
        crate::compile_program(merged)
    } else {
        // No namespace - compile as a standalone file
        crate::compile(&source)
    }
}

/// Compile a file with module resolution for a specific target
///
/// Loads the file and all its dependencies, then compiles them together
/// with target-aware conditional compilation.
pub fn compile_file_with_target(
    path: &Path,
    lib_paths: &[PathBuf],
    target: Target,
) -> std::result::Result<String, Vec<CompileError>> {
    // Build search paths: file's directory + lib paths
    let mut search_paths = Vec::new();
    if let Some(parent) = path.parent() {
        search_paths.push(parent.to_path_buf());
    }
    search_paths.extend(lib_paths.iter().cloned());

    // Read and parse the main file
    let source = fs::read_to_string(path).map_err(|e| {
        vec![CompileError::resolve(
            Span::new(0, 0),
            format!("Failed to read {}: {}", path.display(), e),
        )]
    })?;

    let mut parser = Parser::new(&source).map_err(|e| vec![e])?;
    let program = parser.parse_program().map_err(|e| vec![e])?;

    // Check if this file has a namespace
    let namespace = ModuleLoader::extract_namespace(&program);

    if let Some(ns) = namespace {
        // Load dependencies via ModuleLoader
        let mut loader = ModuleLoader::new(search_paths);

        // Pre-seed the cache with the already-parsed main file
        loader.cache.insert(ns.clone(), program.clone());

        // Load dependencies first
        loader
            .load_dependencies(&program, Span::new(0, 0))
            .map_err(|e| vec![e])?;

        // Add main module to load_order AFTER dependencies
        loader.load_order.push(ns);

        // Get merged program
        let merged = loader.into_merged_program();

        // Compile with target
        crate::compile_program_with_target(merged, target)
    } else {
        // No namespace - compile as a standalone file with target
        crate::compile_with_target(&source, target)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_find_module_not_found() {
        // Use an empty search path to ensure nothing is found
        let loader = ModuleLoader::new(vec![]);
        let found = loader.find_module("nonexistent");
        assert!(found.is_none());
    }

    #[test]
    fn test_extract_namespace() {
        let source = "(ns my.namespace)";
        let mut parser = Parser::new(source).unwrap();
        let program = parser.parse_program().unwrap();

        let ns = ModuleLoader::extract_namespace(&program);
        assert_eq!(ns, Some("my.namespace".to_string()));
    }

    #[test]
    fn test_extract_namespace_none() {
        let source = "(defun test () 42)";
        let mut parser = Parser::new(source).unwrap();
        let program = parser.parse_program().unwrap();

        let ns = ModuleLoader::extract_namespace(&program);
        assert!(ns.is_none());
    }

    #[test]
    fn test_extract_namespace_with_requires() {
        let source = "(ns my.app (:require [other :as o]))";
        let mut parser = Parser::new(source).unwrap();
        let program = parser.parse_program().unwrap();

        let ns = ModuleLoader::extract_namespace(&program);
        assert_eq!(ns, Some("my.app".to_string()));
    }
}

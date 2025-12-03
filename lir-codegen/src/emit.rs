//! Output emission for AOT compilation

use crate::codegen::{CodeGen, CodeGenError, Result};
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple,
};
use inkwell::OptimizationLevel;

/// Optimization level for code generation
#[derive(Debug, Clone, Copy, Default)]
pub enum OptLevel {
    #[default]
    None, // -O0
    Less,       // -O1
    Default,    // -O2
    Aggressive, // -O3
}

impl From<OptLevel> for OptimizationLevel {
    fn from(level: OptLevel) -> Self {
        match level {
            OptLevel::None => OptimizationLevel::None,
            OptLevel::Less => OptimizationLevel::Less,
            OptLevel::Default => OptimizationLevel::Default,
            OptLevel::Aggressive => OptimizationLevel::Aggressive,
        }
    }
}

/// Output format for compilation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFormat {
    Object,   // .o
    Assembly, // .s
    LlvmIr,   // .ll
    Bitcode,  // .bc
}

impl<'ctx> CodeGen<'ctx> {
    /// Initialize LLVM targets for the native platform
    pub fn initialize_native_target() {
        Target::initialize_native(&InitializationConfig::default())
            .expect("Failed to initialize native target");
    }

    /// Initialize all LLVM targets (for cross-compilation)
    pub fn initialize_all_targets() {
        Target::initialize_all(&InitializationConfig::default());
    }

    /// Get list of available target triples
    pub fn available_targets() -> Vec<String> {
        let mut targets = Vec::new();
        let mut target = Target::get_first();
        while let Some(t) = target {
            if let Ok(name) = t.get_name().to_str() {
                targets.push(name.to_string());
            }
            target = t.get_next();
        }
        targets
    }

    /// Get the default target triple for this host
    pub fn default_target_triple() -> String {
        TargetMachine::get_default_triple()
            .as_str()
            .to_string_lossy()
            .to_string()
    }

    /// Create a target machine for code generation
    fn create_target_machine(
        &self,
        triple: Option<&str>,
        opt_level: OptLevel,
        reloc_mode: RelocMode,
    ) -> Result<TargetMachine> {
        let triple = match triple {
            Some(t) => TargetTriple::create(t),
            None => TargetMachine::get_default_triple(),
        };

        let target = Target::from_triple(&triple).map_err(|e| {
            CodeGenError::CodeGen(format!("failed to get target for triple: {}", e))
        })?;

        let cpu = TargetMachine::get_host_cpu_name();
        let features = TargetMachine::get_host_cpu_features();

        target
            .create_target_machine(
                &triple,
                cpu.to_str().unwrap_or("generic"),
                features.to_str().unwrap_or(""),
                opt_level.into(),
                reloc_mode,
                CodeModel::Default,
            )
            .ok_or_else(|| CodeGenError::CodeGen("failed to create target machine".to_string()))
    }

    /// Emit output in the specified format
    pub fn emit(
        &self,
        format: OutputFormat,
        triple: Option<&str>,
        opt_level: OptLevel,
        pic: bool,
    ) -> Result<Vec<u8>> {
        match format {
            OutputFormat::LlvmIr => {
                let ir = self.module.print_to_string();
                Ok(ir.to_bytes().to_vec())
            }
            OutputFormat::Bitcode => {
                let buffer = self.module.write_bitcode_to_memory();
                Ok(buffer.as_slice().to_vec())
            }
            OutputFormat::Assembly | OutputFormat::Object => {
                let reloc_mode = if pic {
                    RelocMode::PIC
                } else {
                    RelocMode::Default
                };
                let target_machine = self.create_target_machine(triple, opt_level, reloc_mode)?;

                let file_type = match format {
                    OutputFormat::Assembly => FileType::Assembly,
                    OutputFormat::Object => FileType::Object,
                    _ => unreachable!(),
                };

                let buffer = target_machine
                    .write_to_memory_buffer(&self.module, file_type)
                    .map_err(|e| CodeGenError::CodeGen(format!("failed to emit: {}", e)))?;

                Ok(buffer.as_slice().to_vec())
            }
        }
    }

    /// Get LLVM IR as string (convenience method)
    pub fn emit_llvm_ir(&self) -> String {
        self.module.print_to_string().to_string()
    }

    /// Set the target triple for the module
    pub fn set_target_triple(&self, triple: &str) {
        self.module.set_triple(&TargetTriple::create(triple));
    }
}

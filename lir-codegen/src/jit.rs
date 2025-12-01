//! JIT execution engine

use inkwell::context::Context;

pub struct JitEngine<'ctx> {
    pub context: &'ctx Context,
}

impl<'ctx> JitEngine<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        Self { context }
    }
}

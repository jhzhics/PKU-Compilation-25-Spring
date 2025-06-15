//! In this part, we do local optimizations on SSA form in one block. (Easy and without dataflow analysis)
//! The optimizations we perform are:
//! 1. Constant propagation and folding
//! 2. Common subexpression elimination
//! 3. Replication elimination
//! 4. Dead code elimination

pub mod constant_opt;
pub mod cse_opt;
pub mod dce_opt;
pub mod repl_elm;

use super::active_aly;
use super::riscv;
use super::ssa_form;
use super::ssa_pass2;

pub fn pass(func: &mut ssa_pass2::SSAFunc) {
    func.blocks.iter_mut().for_each(|(_, block)| {
        constant_opt::pass(block);
        cse_opt::pass(block);
        repl_elm::pass(block);
        dce_opt::pass(block);
    });
}

mod active_aly;
mod dbe_pass;
mod misc;
mod riscv;
mod ssa_form;
mod ssa_local_opt_pass;
mod ssa_pass1;
mod ssa_pass2;

use koopa;
use std::collections::LinkedList;

pub fn compile(prog: koopa::ir::Program) -> String {
    koopa::ir::Type::set_ptr_size(4);

    let mut inst_list: LinkedList<String> = misc::generate_dataseg(&prog);

    let pass1funcs = prog
        .funcs()
        .iter()
        .map(|(f, _)| ssa_pass1::pass(&prog, f.clone()))
        .collect::<Vec<ssa_pass1::Pass1Func>>();

    let mut ssa_pass1funcs = pass1funcs
        .iter()
        .cloned()
        .filter(|f| f.entry.is_some())
        .collect::<Vec<ssa_pass1::Pass1Func>>();

    ssa_pass1funcs.iter_mut().for_each(|f| dbe_pass::pass(f));

    let ssa_funcs = ssa_pass1funcs
        .iter()
        .cloned()
        .map(|f| ssa_pass2::pass(f.clone()))
        .collect::<Vec<ssa_pass2::SSAFunc>>();

    ssa_funcs.iter().for_each(|func| {
        inst_list.extend(func.dump());
        inst_list.push_back("".to_string());
    });

    inst_list
        .iter()
        .map(|s| s.as_str())
        .collect::<Vec<&str>>()
        .join("\n")
}

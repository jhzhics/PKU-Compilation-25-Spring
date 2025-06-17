mod ssa_active_aly;
mod dbe_pass;
mod misc;
mod riscv;
mod ssa_form;
mod ssa_local_opt_pass;
mod ssa_pass1;
mod ssa_pass2;
mod rv_pass1;
mod rv_pass2;
mod rv_pass3;
mod rv_active_aly;

use koopa;
use std::collections::LinkedList;

pub fn compile(prog: koopa::ir::Program) -> String {
    koopa::ir::Type::set_ptr_size(4);

    let mut inst_list: LinkedList<String> = misc::generate_dataseg(&prog, "global_");

    let pass1funcs = prog
        .funcs()
        .iter()
        .map(|(f, _)| ssa_pass1::pass(&prog, f.clone()))
        .collect::<Vec<ssa_pass1::SSAPass1Func>>();
    
    let mut ssa_pass1funcs = pass1funcs
    .iter()
    .cloned()
    .filter(|f| f.entry.is_some())
    .collect::<Vec<ssa_pass1::SSAPass1Func>>();

    ssa_pass1funcs.iter_mut().for_each(|f| dbe_pass::pass(f));
    
    let ssa_funcs = ssa_pass1funcs
    .iter()
    .cloned()
    .map(|f| ssa_pass2::pass(f.clone()))
    .collect::<Vec<ssa_pass2::SSAFunc>>();

    let mut rv_pass1_funcs = ssa_funcs
    .iter()
    .map(|f| rv_pass1::pass(f))
    .collect::<Vec<rv_pass1::RVPass1Func>>();


    rv_pass1_funcs.iter_mut().for_each(|f| {
        rv_pass2::pass(f);
        rv_pass3::pass(f);
    });

    rv_pass1_funcs.iter().for_each(|func| {
        inst_list.extend(func.release_dump());
        inst_list.push_back("".to_string());
    });

    inst_list
    .iter()
    .map(|s| s.as_str())
    .collect::<Vec<&str>>()
            .join("\n")
}

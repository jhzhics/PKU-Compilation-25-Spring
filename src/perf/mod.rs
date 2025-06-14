mod misc;
mod riscv;
mod active_aly;
mod ssa_form;
mod pass1;
mod pass2;

use koopa;
use std::collections::LinkedList;


pub fn compile(prog: koopa::ir::Program) -> String {
    koopa::ir::Type::set_ptr_size(4);

    let mut inst_list: LinkedList<String> = misc::generate_dataseg(&prog);

    let pass1funcs = prog
        .funcs()
        .iter()
        .map(|(f, _)| pass1::pass1(&prog, f.clone()))
        .collect::<Vec<pass1::Pass1Func>>();

    let pass1funcs = pass1funcs.iter().filter(|f| {
    f.entry.is_some() });


    let pass2funcs = pass1funcs
        .map(|f| pass2::pass2(f.clone()))
        .collect::<Vec<pass2::Pass2Func>>();

    pass2funcs.iter().for_each(|func| {
        inst_list.extend(func.dump());
        inst_list.push_back("".to_string());
    });

    
    inst_list
        .iter()
        .map(|s| s.as_str())
        .collect::<Vec<&str>>()
        .join("\n")
}
mod misc;
mod riscv;
mod pass1;

use koopa;
use std::collections::LinkedList;


pub fn compile(prog: koopa::ir::Program) -> String {
    koopa::ir::Type::set_ptr_size(4);

    let mut inst_list: LinkedList<String> = misc::generate_dataseg(&prog);

    let phase1funcs = prog
        .funcs()
        .iter()
        .map(|(f, _)| pass1::pass1(&prog, f.clone()))
        .collect::<Vec<pass1::Pass1Func>>();

    phase1funcs.iter().for_each(|func| {
        if func.entry.is_none() {
            return;
        }

        inst_list.extend(func.dump());
    });

    inst_list.push_back("\n".to_string());
    
    inst_list
        .iter()
        .map(|s| s.as_str())
        .collect::<Vec<&str>>()
        .join("\n")
}
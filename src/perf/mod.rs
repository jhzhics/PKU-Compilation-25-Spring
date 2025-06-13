mod misc;
mod riscv;
mod phase1;

use koopa;
use std::collections::LinkedList;


pub fn compile(mut prog: koopa::ir::Program) -> String {
    koopa::ir::Type::set_ptr_size(4);

    let mut inst_list: LinkedList<String> = misc::generate_dataseg(&prog);

    let phase1funcs = prog
        .funcs_mut()
        .iter_mut()
        .map(|(_, f)| phase1::phase1(f))
        .collect::<Vec<phase1::Phase1Func>>();


    phase1funcs.iter().for_each(|func| {
        inst_list.push_back("".to_string());
        inst_list.push_back(".text".to_string());
        inst_list.push_back(format!(".globl {}", func.name));
        func.blocks.iter().for_each(|(_name, block)| {
            let block_inst_list = block.block.dump();
            inst_list.extend(block_inst_list.into_iter());
        });
    });
    inst_list.push_back("\n".to_string());
    
    inst_list
        .iter()
        .map(|s| s.as_str())
        .collect::<Vec<&str>>()
        .join("\n")
}
//! This module coverts SSA form to a basic RV form (infinite registers, but without any virtual instructions).

use std::cmp::max;
use std::collections::LinkedList;

use crate::perf::riscv::fit_in_imm12;
use crate::perf::rv_pass1;

use super::riscv::{self, Instr};
use super::ssa_form;
use super::ssa_pass2;

#[derive(Debug, Clone)]
pub struct RVPass1Func {
    pub name: String,
    pub blocks: Vec<RVPass1Block>,
    pub args: Vec<String>,
    pub is_return_i32: bool,
    stack_size: usize,

    // None if there is no function call, Some(n) if there is a function call with max n arguments
    pub func_call_argc: Option<usize>,
    pub prologue: String,
    pub epilogue: String,
}

#[derive(Debug, Clone)]
pub struct RVPass1Block {
    pub block: riscv::Block,
    pub next: Vec<String>,
}

static mut TMP_REG_OFFSET: Option<i32> = None;
use riscv::TMP_REG;

fn get_sp_offset_reg(offset: i32, block: &mut RVPass1Block) -> String {
    if fit_in_imm12(offset) {
        format!("{}({})", offset, riscv::RV_SP_REG)
    } else {
        unsafe {
            match TMP_REG_OFFSET {
                None => {
                    block
                        .block
                        .instrs
                        .push(Instr::new(&format!("li {}, {}", TMP_REG, offset)));
                    block.block.instrs.push(Instr::new(&format!(
                        "add {}, {}, {}",
                        TMP_REG,
                        riscv::RV_SP_REG,
                        TMP_REG
                    )));
                    TMP_REG_OFFSET = Some(offset);
                    TMP_REG.to_string()
                }
                Some(tmp_offset) => {
                    let new_offset = offset - tmp_offset;
                    if fit_in_imm12(new_offset) {
                        format!("{}({})", new_offset, TMP_REG)
                    } else {
                        block
                            .block
                            .instrs
                            .push(Instr::new(&format!("li {}, {}", TMP_REG, new_offset)));
                        block.block.instrs.push(Instr::new(&format!(
                            "add {}, {}, {}",
                            TMP_REG,
                            riscv::RV_SP_REG,
                            TMP_REG
                        )));
                        TMP_REG.to_string()
                    }
                }
            }
        }
    }
}

fn get_is_return_i32(ssa_func: &ssa_pass2::SSAFunc) -> bool {
    let mut is_return = None;
    ssa_func.blocks.iter().for_each(|block| {
        let last_instr = block
            .1
            .block
            .instrs
            .last()
            .expect("Block should have at least one instruction");
        if last_instr.op == "Ret" {
            assert!(
                last_instr.operands.len() <= 1,
                "Return instruction should have at most one operand"
            );
            let local_is_return = last_instr.operands.len() == 1;
            if let Some(prev_is_return) = is_return {
                assert_eq!(
                    prev_is_return, local_is_return,
                    "All return instructions should have the same number of operands"
                );
            } else {
                is_return = Some(local_is_return);
            }
        }
    });
    is_return.expect("Function should have at least one return instruction")
}

fn get_func_call_args(ssa_func: &ssa_pass2::SSAFunc) -> Option<usize> {
    let mut max_argc: Option<usize> = None;
    ssa_func.blocks.iter().for_each(|block| {
        block.1.block.instrs.iter().for_each(|instr| {
            let local_argc: usize;
            if instr.op == "Call_1" {
                local_argc = instr.operands.len() - 1; // Exclude the function name
            } else if instr.op == "Call_2" {
                local_argc = instr.operands.len() - 2; // Exclude the function name and the return register
            } else {
                return; // Not a function call instruction
            }
            max_argc = match max_argc {
                Some(max) => Some(max.max(local_argc)),
                None => Some(local_argc),
            };
        });
    });
    max_argc
}

pub fn pass(ssa_func: &ssa_pass2::SSAFunc) -> RVPass1Func {
    unsafe {
        TMP_REG_OFFSET = None; // Reset the temporary register offset
    }

    let mut func = RVPass1Func {
        name: ssa_func.name[1..].to_string(), // Remove the leading '@'
        blocks: Vec::new(),                   // To be updated with blocks
        args: ssa_func.args.clone(),
        is_return_i32: get_is_return_i32(ssa_func),
        stack_size: 0, // To be updated with stack size
        func_call_argc: get_func_call_args(ssa_func),
        prologue: format!("L_Prologue_{}", &ssa_func.name[1..]),
        epilogue: format!("L_Epilogue_{}", &ssa_func.name[1..]),
    };
    let mut state = State {
        stack_size: max(
            0,
            func.func_call_argc.map_or(0,
             |argc| 4 * argc.saturating_sub(8) as i32) + 4,
        ),
    };
    for (_, block) in &ssa_func.blocks {
        let rv_block = transform_block(block, &mut state, &func.epilogue);
        func.blocks.push(rv_block);
    }
    func.stack_size = state.stack_size as usize;
    fill_proloque(&mut func, ssa_func);
    fill_epilogue(&mut func, ssa_func);
    func
}

fn fill_proloque(func: &mut RVPass1Func, ssa_func: &ssa_pass2::SSAFunc) {
    let mut prologue = rv_pass1::RVPass1Block {
        block: riscv::Block {
            name: func.prologue.clone(),
            instrs: Vec::new(),
        },
        next: vec![ssa_func.entry.clone()],
    };
    for (i, arg) in func.args.iter().enumerate() {
        if i < 8 {
            prologue
                .block
                .instrs
                .push(Instr::new(&format!("mv {}, a{}", arg, i)));
        } else {
            let offset = 8 + 4 * (i - 8) as i32;
            let offset_reg = get_sp_offset_reg(offset, &mut prologue);
            prologue
                .block
                .instrs
                .push(Instr::new(&format!("sw {}, {}", arg, offset_reg)));
        }
    }
    prologue.block.instrs.push(Instr::new(&format!(
        "j {}",
        ssa_func.entry.clone()
    )));
    func.blocks.insert(0, prologue);
}

fn fill_epilogue(func: &mut RVPass1Func, _ssa_func: &ssa_pass2::SSAFunc) {
    let mut epilogue = rv_pass1::RVPass1Block {
        block: riscv::Block {
            name: func.epilogue.clone(),
            instrs: Vec::new(),
        },
        next: vec![],
    };
    epilogue
        .block
        .instrs
        .push(Instr::new(&format!("ret # {}", func.is_return_i32)));
    func.blocks.push(epilogue);
}

struct State {
    pub stack_size: i32,
}

fn transform_block(block: &ssa_form::SSABlock, state: &mut State, epilogue: &str) -> RVPass1Block {
    let mut rv_block = RVPass1Block {
        block: riscv::Block {
            name: block.block.name.clone(),
            instrs: Vec::new(),
        },
        next: block
            .next
            .iter()
            .map(|(next_block, _)| next_block.clone())
            .collect(),
    };

    for instr in &block.block.instrs {
        if instr.op == "Alloc" {
            let alloc_size = instr.operands[1]
                .parse::<usize>()
                .expect("Alloc instruction should have a valid size");
            state.stack_size += alloc_size as i32;
            if riscv::fit_in_imm12(state.stack_size) {
                rv_block.block.instrs.push(Instr::new(&format!(
                    "addi {}, {}, {}",
                    instr.operands[0],
                    riscv::RV_SP_REG,
                    state.stack_size
                )));
            } else {
                // If the stack size is too large, we need to use a temporary register
                rv_block
                    .block
                    .instrs
                    .push(Instr::new(&format!("li {}, {}", TMP_REG, state.stack_size)));
                rv_block.block.instrs.push(Instr::new(&format!(
                    "add {}, {}, {}",
                    instr.operands[0],
                    riscv::RV_SP_REG,
                    TMP_REG
                )));
            }
        } else if instr.op == "Call_1" {
            for (i, arg) in instr.operands[1..].iter().enumerate() {
                if i < 8 {
                    rv_block
                        .block
                        .instrs
                        .push(Instr::new(&format!("mv a{}, {}", i, arg)));
                } else {
                    let offset = 4 + 4 * (i - 8) as i32;
                    let offset_reg = get_sp_offset_reg(offset, &mut rv_block);
                    rv_block
                        .block
                        .instrs
                        .push(Instr::new(&format!("sw {}, {}", arg, offset_reg)));
                }
                unsafe {
                    TMP_REG_OFFSET = None; // Reset the temporary register offset for each instruction
                }
            }
            rv_block.block.instrs.push(Instr::new(&format!(
                "call {} # {}",
                &instr.operands[0][1..],
                instr.operands.len() - 1
            )));
        } else if instr.op == "Call_2" {
            for (i, arg) in instr.operands[2..].iter().enumerate() {
                if i < 8 {
                    rv_block
                        .block
                        .instrs
                        .push(Instr::new(&format!("mv a{}, {}", i, arg)));
                } else {
                    let offset = 4 + 4 * (i - 8) as i32;
                    let offset_reg = get_sp_offset_reg(offset, &mut rv_block);
                    rv_block
                        .block
                        .instrs
                        .push(Instr::new(&format!("sw {}, {}", arg, offset_reg)));
                }
            }
            rv_block.block.instrs.push(Instr::new(&format!(
                "call {} # {}",
                &instr.operands[1][1..],
                instr.operands.len() - 2
            )));
            rv_block
                .block
                .instrs
                .push(Instr::new(&format!("mv {}, a0", instr.operands[0])));
        } else if instr.op == "Br" {
            rv_block.block.instrs.push(Instr::new(&format!(
                "beqz {}, {}",
                instr.operands[0], instr.operands[1]
            )));
            rv_block
                .block
                .instrs
                .push(Instr::new(&format!("j {}", instr.operands[2])));
        } else if instr.op == "Ret" {
            if instr.operands.len() == 1 {
                rv_block
                    .block
                    .instrs
                    .push(Instr::new(&format!("mv a0, {}", instr.operands[0])));
            }
            rv_block
                .block
                .instrs
                .push(Instr::new(&format!("j {}", epilogue)));
            rv_block.next = vec![epilogue.to_string()];
        } else {
            rv_block.block.instrs.push(instr.clone());
        }
    }
    rv_block
}

impl RVPass1Func {
    #[allow(dead_code)]
    pub fn dump(&self) -> LinkedList<String> {
        let mut inst_list = LinkedList::new();
        inst_list.push_back(format!(".text"));
        inst_list.push_back(format!(".globl {}", self.name));
        inst_list.push_back(format!("{}:", self.name));
        for block in &self.blocks {
            inst_list.push_front("".to_string());
            inst_list.extend(block.block.dump());
        }
        inst_list
    }
}

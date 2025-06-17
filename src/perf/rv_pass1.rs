//! This module coverts SSA form to a basic RV form (infinite registers)
//! The main purpose is convert the virtual "Call" to "call"

use std::cmp::max;
use std::collections::{HashMap, HashSet, LinkedList};
use std::os::linux::raw::stat;

use crate::perf::riscv::fit_in_imm12;
use crate::perf::rv_pass1;

use super::riscv::{self, Instr};
use super::ssa_form;
use super::ssa_pass2;

#[derive(Debug, Clone)]
pub struct RVPass1Func {
    pub name: String,
    pub blocks: HashMap<String, RVPass1Block>,
    pub args: Vec<String>,
    pub is_return_i32: bool,
    pub stack_size: usize,

    // None if there is no function call, Some(n) if there is a function call with max n arguments
    pub func_call_argc: Option<usize>,
    pub prologue: String,
    pub epilogue: String,
}

#[derive(Debug, Clone)]
pub struct RVPass1Block {
    pub block: riscv::Block,
    pub next: Vec<String>, // Next blocks, with operands
    pub prev: Vec<String>, // Previous blocks, with operands
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
    let mut func = RVPass1Func {
        name: ssa_func.name[1..].to_string(), // Remove the leading '@'
        blocks: HashMap::new(),                   // To be updated with blocks
        args: ssa_func.args.clone(),
        is_return_i32: get_is_return_i32(ssa_func),
        stack_size: 0, // To be updated with stack size
        func_call_argc: get_func_call_args(ssa_func),
        prologue: format!("L_Prologue_{}", &ssa_func.name[1..]),
        epilogue: format!("L_Epilogue_{}", &ssa_func.name[1..]),
    };
    let mut state = State::new();
    state.stack_size = max(
            0,
            func.func_call_argc.map_or(0,
             |argc| 4 * argc.saturating_sub(8) as i32),
        );
    for (_, block) in &ssa_func.blocks {
        let rv_block = transform_block(ssa_func, block, &mut state, &func.epilogue);
        func.blocks.insert(
            rv_block.block.name.clone(),
            rv_block,
        );
    }
    func.stack_size = state.stack_size as usize;
    fill_prologue(&mut func, ssa_func, &mut state);
    fill_epilogue(&mut func, ssa_func, &mut state);
    func
}

fn fill_prologue(func: &mut RVPass1Func, ssa_func: &ssa_pass2::SSAFunc, state: &mut State) {
    let entry_block = func.blocks.get_mut(&ssa_func.entry)
        .expect("Entry block should exist in RVPass1Func");
    entry_block.prev.push(func.prologue.clone());

    state.reset_tmp_reg();
    let mut prologue = rv_pass1::RVPass1Block {
        block: riscv::Block {
            name: func.prologue.clone(),
            instrs: Vec::new(),
        },
        next: Vec::new(),
        prev: Vec::new(),
    };
    let vertices_set = func.get_vertices_set();
    prologue.next.push(ssa_func.entry.clone());

    for (i, arg) in func.args.iter().enumerate() {
        if !vertices_set.contains(arg) {
            continue; // Skip arguments that are not used in the function
        }
        if i < 8 {
            prologue
                .block
                .instrs
                .push(Instr::new(&format!("mv {}, a{}", arg, i)));
        } else {
            let offset = 4 * (i - 8) as i32;
            let offset_reg = state.get_sp_offset_reg(offset, &mut prologue);
            prologue
                .block
                .instrs
                .push(Instr::new(&format!("lw {}, {}", arg, offset_reg)));
        }
    }
    let tmp = state.get_write_tmp_reg();
    prologue.block.instrs.push(Instr::new(&format!(
        "li {}, $NEG_STACK_SIZE",
        tmp
    )));
    prologue.block.instrs.push(Instr::new(&format!(
        "add {}, {}, {}",
        riscv::RV_SP_REG,
        riscv::RV_SP_REG,
        tmp
    )));

    prologue.block.instrs.push(Instr::new(&format!(
        "j {}",
        ssa_func.entry.clone()
    )));
    func.blocks.insert(
        prologue.block.name.clone(),
        prologue,
    );

}

fn fill_epilogue(func: &mut RVPass1Func, _ssa_func: &ssa_pass2::SSAFunc, state: &mut State) {
    state.reset_tmp_reg();
    let mut epilogue = rv_pass1::RVPass1Block {
        block: riscv::Block {
            name: func.epilogue.clone(),
            instrs: Vec::new(),
        },
        next: vec![],
        prev: vec![],
    };
    epilogue.prev = func.blocks
        .values()
        .filter_map(|block| {
            if block.next.iter().any(|name| name == &func.epilogue) {
                Some(block.block.name.clone())
            } else {
                None
            }
        })
        .collect();

    let tmp = state.get_write_tmp_reg();
    epilogue.block.instrs.push(Instr::new(&format!(
        "li {}, $STACK_SIZE",
        tmp
    )));
    epilogue.block.instrs.push(Instr::new(&format!(
        "add {}, {}, {}",
        riscv::RV_SP_REG,
        riscv::RV_SP_REG,
        tmp
    )));
    if func.is_return_i32 {
        epilogue
            .block
            .instrs
            .push(Instr::new("Ret a0"));
    }
    else {
        epilogue.block.instrs.push(Instr::new("Ret"));
    }
    func.blocks.insert(
        epilogue.block.name.clone(),
        epilogue,
    );
}

struct State {
    pub stack_size: i32,
    next_idx: usize,
    sp_offset: Option<i32>,
}

impl State {
    const TMP_REG: &'static str = "rv_pass1_tmp";

    pub fn new() -> Self {
        State {
            stack_size: 0,
            next_idx: 0,
            sp_offset: None,
        }
    }
    
    pub fn reset_tmp_reg(&mut self) {
        self.next_idx += 1;
        self.sp_offset = None;
    }

    pub fn get_sp_offset_reg(&mut self, offset: i32, block: &mut RVPass1Block) -> String {
        if fit_in_imm12(offset) {
            format!("{}({})", offset, riscv::RV_SP_REG)
        } else {
            match self.sp_offset {
                None => {
                    block.block.instrs.push(Instr::new(&format!("li {}, {}", self.get_write_tmp_reg(), offset)));
                    let tmp = self.get_write_tmp_reg();
                    block.block.instrs.push(Instr::new(&format!(
                        "add {}, {}, {}",
                        tmp,
                        riscv::RV_SP_REG,
                        tmp
                    )));
                    self.sp_offset = Some(offset);
                    tmp
                }
                Some(tmp_offset) => {
                    let new_offset = offset - tmp_offset;
                    if fit_in_imm12(new_offset) {
                        format!("{}({})", new_offset, self.get_read_tmp_reg())
                    } else {
                        block.block.instrs.push(Instr::new(&format!("li {}, {}", self.get_write_tmp_reg(), new_offset)));
                        let tmp = self.get_write_tmp_reg();
                        block.block.instrs.push(Instr::new(&format!(
                            "add {}, {}, {}",
                            tmp,
                            riscv::RV_SP_REG,
                            tmp
                        )));
                        self.sp_offset = Some(new_offset);
                        tmp
                    }
                }
            }
        }
    }

    pub fn get_write_tmp_reg(&mut self) -> String {
        self.next_idx += 1;
        self.sp_offset = None; // Reset the offset for the next use
        format!("{}_{}", Self::TMP_REG, self.next_idx)
    }

    fn get_read_tmp_reg(&mut self) -> String {
        format!("{}_{}", Self::TMP_REG, self.next_idx)
    }
}

fn transform_block(func: &ssa_pass2::SSAFunc, block: &ssa_form::SSABlock, state: &mut State, epilogue: &str)
-> RVPass1Block {
    state.reset_tmp_reg();
    let mut rv_block = RVPass1Block {
        block: riscv::Block {
            name: block.block.name.clone(),
            instrs: Vec::new(),
        },
        next: block.next.iter()
            .map(|(name, _)| name.clone()).collect(),
        prev: block.prev.clone()
    };

    for instr in &block.block.instrs {
        if instr.op == "Alloc" {
            let alloc_size = instr.operands[1]
                .parse::<usize>()
                .expect("Alloc instruction should have a valid size");
            let old_stack_size = state.stack_size;
            state.stack_size += alloc_size as i32;
            assert!(
                alloc_size > 4,
                "Size == 4 should be in virtual register, not in stack"
            );
    
            if riscv::fit_in_imm12(old_stack_size) {
                rv_block.block.instrs.push(Instr::new(&format!(
                    "addi {}, {}, {}",
                    instr.operands[0],
                    riscv::RV_SP_REG,
                    old_stack_size
                )));
            } else {
                // If the stack size is too large, we need to use a temporary register
                let tmp_reg = state.get_write_tmp_reg();
                rv_block
                    .block
                    .instrs
                    .push(Instr::new(&format!("li {}, {}", tmp_reg, old_stack_size)));
                rv_block.block.instrs.push(Instr::new(&format!(
                    "add {}, {}, {}",
                    instr.operands[0],
                    riscv::RV_SP_REG,
                    tmp_reg
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
                    let offset = 4 * (i - 8) as i32;
                    let offset_reg = state.get_sp_offset_reg(offset, &mut rv_block);
                    rv_block
                        .block
                        .instrs
                        .push(Instr::new(&format!("sw {}, {}", arg, offset_reg)));
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
                    let offset = 4 * (i - 8) as i32;
                    let offset_reg = state.get_sp_offset_reg(offset, &mut rv_block);
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
            rv_block.next = vec![
                epilogue.to_string()
            ];
        }
        else if instr.op == "j"
        {
            for (arg, param) in block.next.first().expect("Block should have at least one next block")
                .1.iter().zip(
                    func.blocks.get(&instr.operands[0])
                        .expect("Next block should exist in RVPass1Func")
                        .params.iter()
                )
            {
                rv_block.block.instrs.push(Instr::new(&format!(
                    "mv {}, {}",
                    param, arg
                )));
            }
            rv_block.block.instrs.push(instr.clone());
        }
        else if instr.op == "Br"
        {
            for (arg, param) in block.next.first().expect("Block should have at least one next block")
                .1.iter().zip(
                    func.blocks.get(&instr.operands[1])
                        .expect("Next block should exist in RVPass1Func")
                        .params.iter()
                )
            {
                rv_block.block.instrs.push(Instr::new(&format!(
                    "mv {}, {}",
                    param, arg
                )));
            }
            rv_block.block.instrs.push(Instr::new(&format!(
                "bnez {}, {}",
                instr.operands[0],
                instr.operands[1]
            )));
            for (arg, param) in block.next.iter().skip(1).next()
                .expect("Block should have at least one next block")
                .1.iter().zip(
                    func.blocks.get(&instr.operands[2])
                        .expect("Next block should exist in RVPass1Func")
                        .params.iter()
                )
            {
                rv_block.block.instrs.push(Instr::new(&format!(
                    "mv {}, {}",
                    param, arg
                )));
            }
            rv_block.block.instrs.push(Instr::new(&format!(
                "j {}",
                instr.operands[2]
            )));
        }
        else
        {
            rv_block.block.instrs.push(instr.clone());
        }
    }
    rv_block
}

impl RVPass1Func {
    pub fn get_vertices_set(&self) -> HashSet<String> {
        self.blocks
        .values()
        .flat_map(|block| block.block.instrs.iter())
        .flat_map(|instr| {
            instr.gen_vars().iter().cloned().chain(
                instr.kill_vars().iter().cloned()
            ).collect::<Vec<_>>()
        }).collect()
    }

    #[allow(dead_code)]
    pub fn dump(&self) -> LinkedList<String> {
        let mut inst_list = LinkedList::new();
        inst_list.push_back(format!(".text"));
        inst_list.push_back(format!(".globl {}", self.name));
        inst_list.push_back(format!("{}:", self.name));
        for (_, block) in &self.blocks {
            inst_list.push_back(format!(
                "{}:",
                block.block.name
            ));

            inst_list.extend({
                let mut insts = block.block.dump();
                insts.pop_front();
                insts
            });

            inst_list.push_back("".to_string());
        }
        inst_list
    }
}

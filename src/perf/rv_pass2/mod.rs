//! This module implements the second pass of the RV (RISC-V) assembler.
//! It does register allocation

mod shader;

use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::LinkedList;

use crate::perf::riscv::fit_in_imm12;
use crate::perf::riscv::is_real_reg;
use crate::perf::riscv::Instr;
use crate::perf::rv_pass1;

use super::riscv;
use super::rv_pass1::*;
use super::rv_active_aly::rv_active_analyze;

pub fn pass(func: &mut RVPass1Func)
{
    while let Err(spill_var) = try_shade(func) {
        exec_spill_var(func, spill_var);
    }
    complete_prologue_epilogue(func);
}

fn get_used_callee_saved_regs(func: &RVPass1Func) -> Vec<String>
{
    let mut writen = HashSet::new();
    func.blocks.iter()
        .for_each(|(_, block)| {
            block.block.instrs.iter()
                .for_each(|instr| {
                    writen.extend(instr.kill_vars());
                });
        });
    writen.retain(|reg| {
        assert!(is_real_reg(reg.as_str()));
        riscv::RV_CALLEE_SAVE_REGS.contains(&reg.as_str())
        || reg.as_str() == riscv::RA_REG
    });
    writen.into_iter().collect()
}

fn complete_prologue_epilogue(func: &mut RVPass1Func)
{
    let used_callee_saved_regs = get_used_callee_saved_regs(func);
    func.stack_size += 4 * used_callee_saved_regs.len();
    let prologue = func.blocks.get_mut(&func.prologue).expect("Prologue block not found");
    prologue.block.instrs = used_callee_saved_regs.iter().enumerate()
            .map(|(i, reg)| {
                Instr::new(&format!("sw {}, {}({})", 
                    reg,  - 4 * (i as i32 + 1), riscv::RV_SP_REG))
            }).chain(
                prologue.block.instrs.iter().cloned()
            ).collect();
    prologue.block.instrs.iter_mut()
        .filter(|instr| instr.op == "li").for_each(|instr| {
            if instr.operands[1] == "$NEG_STACK_SIZE"
            {
                instr.operands[1] = (-(func.stack_size as i32)).to_string();
            }
        });
    let epilogue = func.blocks.get_mut(&func.epilogue).expect("Epilogue block not found");
    epilogue.block.instrs = epilogue.block.instrs[..epilogue.block.instrs.len()-1].iter().cloned()
        .chain(used_callee_saved_regs.iter().rev().enumerate()
            .map(|(i, reg)| {
                Instr::new(&format!("lw {}, {}({})", 
                    reg, - 4 * (used_callee_saved_regs.len() as i32 - i as i32), riscv::RV_SP_REG))
            })).chain(
                std::iter::once(Instr::new(&format!("ret # {}", func.is_return_i32 as i32 ))))
        .collect();
    epilogue.block.instrs.iter_mut()
        .filter(|instr| instr.op == "li").for_each(|instr| {
            if instr.operands[1] == "$STACK_SIZE"
            {
                instr.operands[1] = func.stack_size.to_string();
            }
        }
    );
}


fn exec_spill_var(func: &mut RVPass1Func, spill_var: String)
{
    assert!(!is_real_reg(&spill_var));
    let offset = func.stack_size as i32;
    func.stack_size = func.stack_size + 4;
    let load_insts = {
        let mut load_insts = LinkedList::new();
        if fit_in_imm12(offset) 
        {
            load_insts.push_back(
                Instr::new(&format!("lw {}, {}({})", 
                    spill_var, offset, riscv::RV_SP_REG)));
        }
        else
        {
            load_insts.push_back(
                Instr::new(&format!("li {}, {}", spill_var, offset)));
            load_insts.push_back(
                Instr::new(&format!("add {}, {}, {}", 
                    spill_var, spill_var, riscv::RV_SP_REG)));
            load_insts.push_back(
                Instr::new(&format!("lw {}, 0({})", 
                    spill_var, spill_var))); 
        }
        load_insts
    };
    let store_insts = {
        let mut store_insts = LinkedList::new();
        if fit_in_imm12(offset) 
        {
            store_insts.push_back(
                Instr::new(&format!("sw {}, {}({})", 
                    spill_var, offset, riscv::RV_SP_REG)));
        }
        else
        {
            let tmp_addr_var = format!("{}_s", spill_var);
            store_insts.push_back(
                Instr::new(&format!("li {}, {}", tmp_addr_var, offset)));
            store_insts.push_back(
                Instr::new(&format!("add {}, {}, {}", 
                    tmp_addr_var, tmp_addr_var, riscv::RV_SP_REG)));
            store_insts.push_back(
                Instr::new(&format!("sw {}, 0({})", 
                    spill_var, tmp_addr_var))); 
        }
        store_insts
    };

    func.blocks.iter_mut()
        .for_each(|(_, block)| {
                let old_block_instrs = block.block.instrs.clone();
                block.block.instrs.clear();
                old_block_instrs.into_iter().for_each(|instr|
                    {
                        if instr.gen_vars().contains(&spill_var)
                        {
                            block.block.instrs.extend(load_insts.iter().cloned());
                        }
                        block.block.instrs.push(instr.clone());
                        if instr.kill_vars().contains(&spill_var)
                        {
                            block.block.instrs.extend(store_insts.iter().cloned());
                        }
                    }
                );
            }       
        );
}

/// # Returns
/// - `Ok(())` if the shading was successful.
/// - `Err(String)` if shading failed, containing the virtual register with the greatest degree that needs to be spilled.
fn try_shade(func: &mut RVPass1Func) -> Result<(), String> {
    let mut active_buffer: HashMap<String, HashSet<String>> =
    func.blocks
        .iter()
        .map(|(name, _)| {
            (name.clone(), HashSet::new())
        })
        .collect();
    let mut to_be_updated: BTreeSet<String> = func.blocks.keys().cloned().collect();
    while !to_be_updated.is_empty() {
        let block_name = to_be_updated.pop_first().unwrap();
        let block = func.blocks.get(&block_name)
            .expect("Block not found in RVPass1Func");
        let out = block.next
            .iter()
            .flat_map(|next_name| {
                active_buffer.get(next_name)
                    .expect("Next block not found in active buffer")
                    .iter()
                    .cloned()
            }).collect::<HashSet<String>>();
        let in_set = rv_active_analyze(&func, &block, out, None);
        let old_in_set = active_buffer.get_mut(&block_name)
            .expect("Block not found in active buffer");
        if old_in_set == &in_set {
            continue; // No change in active variables
        }
        *old_in_set = in_set.clone();
        block.prev.iter()
            .for_each(|prev_name| {
                to_be_updated.insert(prev_name.clone());
            });
    }
    let mut vertices_set = func.get_vertices_set();
    vertices_set.remove(riscv::RV_SP_REG);
    let mut shader = shader::Shader::new(vertices_set);
    for (_, block) in &func.blocks {
        let out = block.next
            .iter()
            .flat_map(|next_name| {
                active_buffer.get(next_name)
                    .expect("Next block not found in active buffer")
                    .iter()
                    .cloned()
            }).collect::<HashSet<String>>();
        let mut conflicts: LinkedList<HashSet<String>> = LinkedList::new();
        rv_active_analyze(func, block, out, Some(&mut conflicts));
        conflicts.iter()
            .for_each(|conflict| shader.add_conflict(conflict));
    }
    
    let hints = func.get_hint_regs();
    let color_map = shader.try_shade(&Some(hints))?;
    func.substitute_vars(&color_map);
    Ok(())
}

impl rv_pass1::RVPass1Func {
    pub fn get_hint_regs(&self) -> HashMap<String, HashSet<String>> {
        let mut hints: HashMap<String, HashSet<String>> = HashMap::new();
        for block in self.blocks.values() {
            for instr in &block.block.instrs {
                if instr.op == "mv"
                {
                    hints.entry(instr.operands[0].clone())
                        .or_insert_with(HashSet::new)
                        .insert(instr.operands[1].clone());
                    hints.entry(instr.operands[1].clone())
                        .or_insert_with(HashSet::new)
                        .insert(instr.operands[0].clone());
                }
            }
        }
        hints
    }
}

impl rv_pass1::RVPass1Block {
    pub fn substitute_vars(&mut self, color_map: &HashMap<String, String>) {
        for instr in self.block.instrs.iter_mut() {
            instr.map_read_vars(|var| {
                color_map.get(&var)
                    .cloned().unwrap()
            });
            instr.map_write_vars(|var| {
                color_map.get(&var)
                    .cloned().unwrap()
            });
        }
    }
}

impl rv_pass1::RVPass1Func {
    pub fn substitute_vars(&mut self, color_map: &HashMap<String, String>) {
        for block in self.blocks.values_mut() {
            block.substitute_vars(color_map);
        }
    }
}
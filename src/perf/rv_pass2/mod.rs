//! This module implements the second pass of the RV (RISC-V) assembler.
//! It does register allocation

mod shader;

use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::LinkedList;

use crate::perf::rv_pass1;

use super::riscv;
use super::rv_pass1::*;
use super::rv_active_aly::active_analyze;

pub fn pass(func: &mut RVPass1Func)
{
    while let Err(spill_var) = try_shade(func) {
        todo!("Spill variable: {}", spill_var);
    }
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
            .flat_map(|(next_name, _)| {
                active_buffer.get(next_name)
                    .expect("Next block not found in active buffer")
                    .iter()
                    .cloned()
            }).collect::<HashSet<String>>();
        let in_set = active_analyze(&func, &block, out, None);
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
            .flat_map(|(next_name, _)| {
                active_buffer.get(next_name)
                    .expect("Next block not found in active buffer")
                    .iter()
                    .cloned()
            }).collect::<HashSet<String>>();
        let mut conflicts: LinkedList<HashSet<String>> = LinkedList::new();
        active_analyze(func, block, out, Some(&mut conflicts));
        conflicts.iter()
            .for_each(|conflict| shader.add_conflict(conflict));
    }
    
    let color_map = shader.try_shade()?;
    func.substitute_vars(&color_map);
    Ok(())
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
        self.next.iter_mut()
            .for_each(|(_, args)| {
                *args = args.iter()
                    .map(|arg| {
                        color_map.get(arg)
                            .cloned().unwrap()
                    }).collect();
        });
        self.params.iter_mut()
            .for_each(|param| {
                *param = color_map.get(param)
                    .cloned().unwrap();
        });
        
    }
}

impl rv_pass1::RVPass1Func {
    pub fn substitute_vars(&mut self, color_map: &HashMap<String, String>) {
        for block in self.blocks.values_mut() {
            block.substitute_vars(color_map);
        }
    }
}
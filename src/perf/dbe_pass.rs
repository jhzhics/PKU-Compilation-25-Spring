//! This module actions on ssa_form1, it is to eliminate dead basic blocks and instructions.
//! With constant propagation for conditional branches.
//! Will delete dead basic blocks.

use std::collections::{HashMap, HashSet, VecDeque};

use super::riscv::Instr;
use super::ssa_pass1;

pub fn pass(func: &mut ssa_pass1::SSAPass1Func) {
    let mut modified = true;
    while modified {
        modified = false;
        for (_, block) in func.blocks.iter_mut() {
            if eliminate_dead_branches(block) {
                modified = true;
            }
        }
        if block_merge_eliminate(func) {
            modified = true;
        }
    }
}

fn block_merge_eliminate(func: &mut ssa_pass1::SSAPass1Func) -> bool {
    let mut modified = false;
    let mut prev: HashMap<String, Vec<String>> = HashMap::new();
    let mut reachable: HashSet<String> = HashSet::new();
    let mut queue: VecDeque<String> = VecDeque::new();
    let entry = func
        .entry
        .as_ref()
        .expect("Function should have an entry block");
    queue.push_back(entry.clone());
    while let Some(block_name) = queue.pop_front() {
        if reachable.contains(&block_name) {
            continue;
        }
        reachable.insert(block_name.clone());
        let block = func
            .blocks
            .get_mut(&block_name)
            .expect("Block should exist");
        for next in &block.next {
            queue.push_back(next.clone());
            prev.entry(next.clone())
                .or_default()
                .push(block_name.clone());
        }
    }
    if reachable.len() != func.blocks.len() {
        modified = true;
        func.blocks.retain(|name, _| reachable.contains(name));
    }
    let mut check_merge = true;
    while check_merge {
        check_merge = false;
        let block_names = func.blocks.keys().cloned().collect::<Vec<String>>();
        for block_name in block_names {
            let block = func.blocks.get(&block_name).expect("Block should exist");
            if block.next.len() != 1 {
                continue;
            }
            let next_block_name = block.next[0].clone();
            let next_prev = prev
                .get(&next_block_name)
                .expect("Next block should have predecessors");
            if next_prev.len() != 1 {
                continue;
            }
            assert!(
                next_block_name != block_name,
                "Next block should not be the same as current block"
            );
            let new_block = merge_block(
                block,
                func.blocks
                    .get(&next_block_name)
                    .expect("Next block should exist"),
            );
            assert!(
                new_block.block.name == block_name,
                "Merged block name should match previous block name"
            );
            prev.remove(&next_block_name);
            prev.iter_mut().for_each(|(_name, preds)| {
                if let Some(pos) = preds.iter().position(|x| x == &next_block_name) {
                    preds[pos] = new_block.block.name.clone();
                }
            });
            func.blocks.remove(&next_block_name);
            func.blocks.insert(new_block.block.name.clone(), new_block);
            check_merge = true;
            modified = true;
            break;
        }
    }
    return modified;
}

fn merge_block(
    prev_block: &ssa_pass1::SSAPass1Block,
    next_block: &ssa_pass1::SSAPass1Block,
) -> ssa_pass1::SSAPass1Block {
    let mut merged_block = prev_block.clone();
    merged_block.block.instrs.pop(); // Remove the jmp instruction from the previous block
    merged_block
        .block
        .instrs
        .extend(next_block.block.instrs.clone());
    merged_block.next = next_block.next.clone();
    merged_block
}

/// This function eliminates dead branches in a basic block by performing constant propagation
/// # Returns
/// Returns true if the last instruction is a branch and it can be determined to be dead, false otherwise.
fn eliminate_dead_branches(block: &mut ssa_pass1::SSAPass1Block) -> bool {
    let last_inst = block
        .block
        .instrs
        .last()
        .expect("Block should have at least one instruction");
    if last_inst.op != "Br" {
        return false;
    }

    let mut constant_map: HashMap<String, i32> = HashMap::new();

    for inst in &block.block.instrs {
        if inst.op == "li" {
            assert!(
                inst.operands.len() == 2,
                "li instruction must have exactly two operands: {}",
                inst
            );
            constant_map.insert(
                inst.operands[0].clone(),
                inst.operands[1].parse::<i32>().unwrap(),
            );
        } else if inst.op == "xor" {
            assert!(
                inst.operands.len() == 3,
                "xor instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = val1 ^ val2;
                    constant_map.insert(inst.operands[0].clone(), result);
                }
            }
        } else if inst.op == "seqz" {
            assert!(
                inst.operands.len() == 2,
                "seqz instruction must have exactly two operands: {}",
                inst
            );
            if let Some(val) = constant_map.get(&inst.operands[1]) {
                let result = if *val == 0 { 1 } else { 0 };
                constant_map.insert(inst.operands[0].clone(), result);
            }
        } else if inst.op == "add" {
            assert!(
                inst.operands.len() == 3,
                "add instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = val1 + val2;
                    constant_map.insert(inst.operands[0].clone(), result);
                }
            }
        } else if inst.op == "sub" {
            assert!(
                inst.operands.len() == 3,
                "sub instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = val1 - val2;
                    constant_map.insert(inst.operands[0].clone(), result);
                }
            }
        } else if inst.op == "mul" {
            assert!(
                inst.operands.len() == 3,
                "mul instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = val1 * val2;
                    constant_map.insert(inst.operands[0].clone(), result);
                }
            } else if let Some(val) = constant_map.get(&inst.operands[1]) {
                if *val == 0 {
                    constant_map.insert(inst.operands[0].clone(), 0);
                }
            } else if let Some(val) = constant_map.get(&inst.operands[2]) {
                if *val == 0 {
                    constant_map.insert(inst.operands[0].clone(), 0);
                }
            }
        } else if inst.op == "div" {
            assert!(
                inst.operands.len() == 3,
                "div instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    assert!(*val2 != 0, "Division by zero in instruction: {}", inst);
                    let result = val1 / val2;
                    constant_map.insert(inst.operands[0].clone(), result);
                }
            }
        } else if inst.op == "rem" {
            assert!(
                inst.operands.len() == 3,
                "rem instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    assert!(*val2 != 0, "Division by zero in instruction: {}", inst);
                    let result = val1 % val2;
                    constant_map.insert(inst.operands[0].clone(), result);
                }
            }
        } else if inst.op == "slt" {
            assert!(
                inst.operands.len() == 3,
                "slt instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = if val1 < val2 { 1 } else { 0 };
                    constant_map.insert(inst.operands[0].clone(), result);
                }
            }
        } else if inst.op == "sgt" {
            assert!(
                inst.operands.len() == 3,
                "sgt instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = if val1 > val2 { 1 } else { 0 };
                    constant_map.insert(inst.operands[0].clone(), result);
                }
            }
        } else if inst.op == "xori" {
            assert!(
                inst.operands.len() == 3,
                "xori instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Ok(val2) = inst.operands[2].parse::<i32>() {
                    let result = val1 ^ val2;
                    constant_map.insert(inst.operands[0].clone(), result);
                }
            }
        } else if inst.op == "snez" {
            assert!(
                inst.operands.len() == 2,
                "snez instruction must have exactly two operands: {}",
                inst
            );
            if let Some(val) = constant_map.get(&inst.operands[1]) {
                let result = if *val != 0 { 1 } else { 0 };
                constant_map.insert(inst.operands[0].clone(), result);
            }
        } else if inst.op == "and" {
            assert!(
                inst.operands.len() == 3,
                "and instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = val1 & val2;
                    constant_map.insert(inst.operands[0].clone(), result);
                }
            } else if let Some(val) = constant_map.get(&inst.operands[1]) {
                if *val == 0 {
                    constant_map.insert(inst.operands[0].clone(), 0);
                }
            } else if let Some(val) = constant_map.get(&inst.operands[2]) {
                if *val == 0 {
                    constant_map.insert(inst.operands[0].clone(), 0);
                }
            }
        } else if inst.op == "or" {
            assert!(
                inst.operands.len() == 3,
                "or instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = val1 | val2;
                    constant_map.insert(inst.operands[0].clone(), result);
                }
            } else if let Some(val) = constant_map.get(&inst.operands[1]) {
                if *val == -1 {
                    constant_map.insert(inst.operands[0].clone(), -1);
                }
            } else if let Some(val) = constant_map.get(&inst.operands[2]) {
                if *val == -1 {
                    constant_map.insert(inst.operands[0].clone(), -1);
                }
            }
        } else if inst.op == "sw" {
            assert!(
                inst.operands.len() == 2,
                "sw instruction must have exactly two operands: {}",
                inst
            );
        } else if inst.op == "mv" {
            assert!(
                inst.operands.len() == 2,
                "mv instruction must have exactly two operands: {}",
                inst
            );
            if let Some(val) = constant_map.get(&inst.operands[1]) {
                constant_map.insert(inst.operands[0].clone(), *val);
            }
        } else if inst.op == "la" {
            assert!(
                inst.operands.len() == 2,
                "la instruction must have exactly two operands: {}",
                inst
            );
        } else if inst.op == "lw" {
            assert!(
                inst.operands.len() == 2,
                "lw instruction must have exactly two operands: {}",
                inst
            );
        } else if inst.op == "j" {
            assert!(
                inst.operands.len() == 1,
                "j instruction must have exactly one operand: {}",
                inst
            );
        } else if inst.op == "Ret" {
            assert!(
                inst.operands.len() <= 1,
                "Ret instruction must have <= 1 operand: {}",
                inst
            );
        } else if inst.op == "Alloc" {
            assert!(
                inst.operands.len() == 2,
                "Alloc instruction must have exactly two operands: {}",
                inst
            );
        } else if inst.op == "Br" {
            assert!(
                inst.operands.len() == 3,
                "Br instruction must have exactly three operands: {}",
                inst
            );
        } else if inst.op == "Call_1" {
            assert!(
                inst.operands.len() >= 1,
                "Call_1 instruction must have at least one operand: {}",
                inst
            );
        } else if inst.op == "Call_2" {
            assert!(
                inst.operands.len() >= 2,
                "Call_2 instruction must have at least two operands: {}",
                inst
            );
        } else {
            panic!("Unknown instruction: {}", inst);
        }
    }

    let last_inst = block
        .block
        .instrs
        .last_mut()
        .expect("Block should have at least one instruction");
    let cond = last_inst.operands[0].clone();

    if let Some(val) = constant_map.get(&cond) {
        let target = if *val != 0 {
            last_inst.operands[1].clone()
        } else {
            last_inst.operands[2].clone()
        };
        *last_inst = Instr::new(&format!("j {}", target));
        block.next = vec![target];
        return true;
    } else {
        return false;
    }
}

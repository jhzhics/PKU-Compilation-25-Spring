use std::collections::{BTreeSet, HashMap, HashSet, LinkedList};

use crate::perf::ssa_active_aly::req_active_analyze;

use super::rv_pass1::*;

pub fn pass(func: &mut RVPass1Func) {
    func.blocks.iter_mut().for_each(|(_, block)| {
        block.block.instrs.retain(|instr| !{
            instr.op == "addi" && instr.operands[0] == instr.operands[1] && instr.operands[2] == "0" ||
            instr.op == "xori" && instr.operands[0] == instr.operands[1] && instr.operands[2] == "0" ||
            instr.op == "andi" && instr.operands[0] == instr.operands[1] && instr.operands[2] == "-1" ||
            instr.op == "slli" && instr.operands[0] == instr.operands[1] && instr.operands[2] == "0" ||
            instr.op == "ori" && instr.operands[0] == instr.operands[1] && instr.operands[2] == "0" ||
            instr.op == "mv" && instr.operands[0] == instr.operands[1]
        })
    });

    let mut in_sets: HashMap<String, HashSet<String>> =
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
                in_sets.get(next_name)
                    .expect("Next block not found in active buffer")
                    .iter()
                    .cloned()
            }).collect::<HashSet<String>>();
        let in_set = req_active_analyze(&block.block, &out, None);
        let old_in_set = in_sets.get_mut(&block_name)
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

    func.blocks.iter_mut().for_each(|(_, block)| {
        let out = block.next
            .iter()
            .flat_map(|next_name| {
                in_sets.get(next_name)
                    .expect("Next block not found in active buffer")
                    .iter()
                    .cloned()
            }).collect::<HashSet<String>>();
        dce_for_block(block, &out);
    });

}

fn dce_for_block(block: &mut RVPass1Block, out: &HashSet<String>) {
    let mut actives = LinkedList::new();
    req_active_analyze(&block.block, out, Some(&mut actives));
    let mut to_remove: Vec<usize> = Vec::new();

    let mut active_iter = actives.iter().skip(1);

    for (i, instr) in block.block.instrs.iter().enumerate() {
        if !instr.is_side_effecting() {
            if let Some(active) = active_iter.next() {
                if active.contains(instr.operands[0].as_str()) {
                    continue;
                }
                to_remove.push(i);
            } else {
                panic!("No active analysis item found for instruction: {}", instr);
            }
        } else {
            // Side-effecting instructions are always kept
            active_iter.next(); // Skip the next active item since side-effecting instructions are not removed
            continue;
        }
    }
    for i in to_remove.iter().rev() {
        block.block.instrs.remove(*i);
    }
}

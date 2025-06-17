use std::collections::{HashSet, LinkedList};

use super::ssa_active_aly::req_active_analyze;

pub fn pass(block: &mut super::ssa_form::SSABlock) -> bool {
    let mut changed = false;
    let out = block
        .next
        .iter()
        .flat_map(|(_, params)| params.iter().cloned())
        .collect::<HashSet<_>>();
    let mut actives = LinkedList::new();
    req_active_analyze(&block.block, &out, Some(&mut actives));
    let mut to_remove: Vec<usize> = Vec::new();

    let mut active_iter = actives.iter().skip(1);

    for (i, instr) in block.block.instrs.iter().enumerate() {
        if !instr.is_side_effecting() {
            if let Some(active) = active_iter.next() {
                if active.contains(instr.operands[0].as_str()) {
                    continue;
                }
                to_remove.push(i);
                changed = true;
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
    changed
}

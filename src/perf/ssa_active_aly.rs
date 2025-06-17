//! This module does active variable analysis for the Koopa IR.

use std::collections::HashSet;
use std::collections::LinkedList;


use super::riscv::Block;

pub fn ssa_active_analyze(
    block: &Block,
    out: HashSet<String>,
    conflicts: Option<&mut LinkedList<HashSet<String>>>,
) -> HashSet<String> {
    let mut binding = LinkedList::new();
    let conflicts = conflicts.unwrap_or(&mut binding);
    let mut now = out.clone();
    conflicts.clear();
    conflicts.push_back(now.clone());
    for i in (0..block.instrs.len()).rev() {
        let instr = &block.instrs[i];
        let kill_vars = instr.kill_vars();
        let gen_vars = instr.gen_vars();
        for var in kill_vars {
            now.remove(&var);
        }

        for var in gen_vars {
            now.insert(var);
        }

        conflicts.push_back(now.clone());
    }
    now
}

pub fn req_active_analyze(
    block: &Block,
    out: &HashSet<String>,
    actives: Option<&mut LinkedList<HashSet<String>>>,
) -> HashSet<String> {
    // This function is similar to `active_analyze`, but it does not deem value not needed as active
    let mut binding = LinkedList::new();
    let actives = actives.unwrap_or(&mut binding);
    let mut now = out.clone();
    actives.clear();
    for i in (0..block.instrs.len()).rev() {
        actives.push_front(now.clone());
        let instr = &block.instrs[i];
        let kill_vars = instr.kill_vars();
        let gen_vars = instr.gen_vars();
        let mut killed = false;
        for var in kill_vars {
            killed = killed || now.remove(&var);
        }

        if killed || instr.is_side_effecting() {
            for var in gen_vars {
                now.insert(var);
            }
        }
    }
    now
}

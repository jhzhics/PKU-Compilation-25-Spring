//! This is the final pass of the RV compiler. First will will merge blocks that can be merged,
//! Then we try to do optimizations aware of Risc-V architecture.
//! Briefly, we do the following:
//!     1. constant substitution: if 0, use zero. if fit in imm12, use addi, etc. (peephole optimization)
//!     2. Do a global conservative active analysis and remove dead code.
//! Finally, we will provide a release_dump of the function.

use std::collections::BTreeSet;
use std::collections::LinkedList;
use super::riscv;
use super::rv_active_aly;
use super::rv_pass1;
mod peephole;
mod dce_opt;


impl rv_pass1::RVPass1Func {
    pub fn release_dump(&self) -> LinkedList<String>
    {
        let mut inst_list = LinkedList::new();
        inst_list.push_back(format!(".text"));
        inst_list.push_back(format!(".globl {}", self.name));
        inst_list.push_back(format!("{}:", self.name));
        inst_list.extend(self.blocks.get(&self.prologue)
            .expect("Prologue block not found")
            .block.dump());
        let mut to_dump = self.blocks.keys().cloned().collect::<BTreeSet<_>>();
        to_dump.remove(&self.prologue);
        let mut last_dumped = self.prologue.clone();
        while !to_dump.is_empty() {
            let mut next_dumped = None;
            for block_name in self.blocks.get(&last_dumped)
                .expect("Last dumped block not found")
                .next
                .iter()
            {
                if to_dump.contains(block_name) {
                    next_dumped = Some(block_name.clone());
                    break;
                }
            }
            if self.blocks.get(&last_dumped)
                .expect("Last dumped block not found")
                .next.len() == 1 && next_dumped.is_some()
                {
                    inst_list.pop_back(); // Remove the unneeded jmp
                }
            let next_dumped = next_dumped.unwrap_or(to_dump.first().unwrap().clone());
            inst_list.extend(self.blocks.get(&next_dumped).expect("Next dumped block not found")
                .block.dump());
            to_dump.remove(&next_dumped);
            last_dumped = next_dumped;            
        }
        inst_list
    }

}

pub fn pass(func: &mut rv_pass1::RVPass1Func) {
    merge_blocks(func);

    func.blocks.iter_mut().for_each(|(_, block)| {
        peephole::pass(block);
    });

    dce_opt::pass(func);

    func.blocks.iter_mut().for_each(|(_, block)| {
        block.block.instrs.iter_mut().for_each(|inst| {
            if inst.op == "la"
            {
                assert!(inst.operands.len() == 2);
                inst.operands[1] = format!("global_{}", &inst.operands[1][1..]);
            }
        });
    });
}

fn merge_blocks(func: &mut rv_pass1::RVPass1Func) {
    let mut modified = false;
    loop {
        let block_names = func.blocks.keys().cloned().collect::<Vec<_>>();
        let mut prev_block_name = String::new();
        let mut next_block_name = String::new();
        for block_name in block_names {
            let block = func.blocks.get_mut(&block_name).unwrap();
            if block.next.len() == 1 {
                let next_name = block.next[0].clone();
                let next_block = func.blocks.get_mut(&next_name).unwrap();
                if next_block.prev.len() == 1
                {
                    assert!(next_block.prev[0] == block_name);
                    modified = true;
                    prev_block_name = block_name.clone();
                    next_block_name = next_name.clone();
                    break;
                }
            }
        }
        if !modified {
            break;
        }
        let next_block = func.blocks.get(&next_block_name).unwrap().clone();
        let prev_block = func.blocks.get_mut(&prev_block_name).unwrap();
        prev_block.next = next_block.next.clone();
        prev_block.block.instrs = 
            prev_block.block.instrs[..prev_block.block.instrs.len() - 1]
            .iter()
            .chain(next_block.block.instrs.iter())
            .cloned()
            .collect();
        func.blocks.remove(&next_block_name);
        func.blocks.iter_mut().for_each(|(_, block)| {
            block.prev.iter_mut().for_each(|prev_name| {
                if prev_name == &next_block_name {
                    *prev_name = prev_block_name.clone();
                }
            });
        });
        func.prologue = if func.prologue == next_block_name {
            prev_block_name.clone()
        } else {
            func.prologue.clone()
        };
        func.epilogue = if func.epilogue == next_block_name {
            prev_block_name.clone()
        } else {
            func.epilogue.clone()
        };
        modified = false;
    }
}
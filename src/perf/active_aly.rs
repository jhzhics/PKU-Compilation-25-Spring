//! This module does active variable analysis for the Koopa IR.

use std::collections::HashSet;
use std::collections::LinkedList;

use crate::perf::riscv;

use super::riscv::Block;
use super::riscv::Instr;

// Extracts the number and register from a memory operand string.
fn extract_register_from_mem_operand(operand_str: &str) -> Option<(String, String)> {
    // Look for the last '(' and first ')' to handle cases like 'offset(register)'
    if let Some(open_paren_idx) = operand_str.rfind('(') {
        if let Some(close_paren_idx) = operand_str.rfind(')') {
            // Ensure the close parenthesis is after the open parenthesis
            if close_paren_idx > open_paren_idx {
                let number_part = &operand_str[..open_paren_idx].trim();
                let register_part = &operand_str[open_paren_idx + 1..close_paren_idx];
                // Check if the extracted part is not empty
                if !register_part.is_empty() {
                    return Some((number_part.to_string(), register_part.trim().to_string()));
                }
            }
        }
    }
    None
}

impl Instr {
    pub fn map_read_vars(&mut self, mut f: impl FnMut(String) -> String) {
        if self.op == "li" {
            assert!(
                self.operands.len() == 2,
                "li instruction must have exactly two operands: {}",
                self
            );
        } else if self.op == "xor" {
            assert!(
                self.operands.len() == 3,
                "xor instruction must have exactly three operands: {}",
                self
            );
            self.operands[1] = f(self.operands[1].clone());
            self.operands[2] = f(self.operands[2].clone());
        } else if self.op == "seqz" {
            assert!(
                self.operands.len() == 2,
                "seqz instruction must have exactly two operands: {}",
                self
            );
            self.operands[1] = f(self.operands[1].clone());
        } else if self.op == "add" {
            assert!(
                self.operands.len() == 3,
                "add instruction must have exactly three operands: {}",
                self
            );
            self.operands[1] = f(self.operands[1].clone());
            self.operands[2] = f(self.operands[2].clone());
        } else if self.op == "sub" {
            assert!(
                self.operands.len() == 3,
                "sub instruction must have exactly three operands: {}",
                self
            );
            self.operands[1] = f(self.operands[1].clone());
            self.operands[2] = f(self.operands[2].clone());
        } else if self.op == "mul" {
            assert!(
                self.operands.len() == 3,
                "mul instruction must have exactly three operands: {}",
                self
            );
            self.operands[1] = f(self.operands[1].clone());
            self.operands[2] = f(self.operands[2].clone());
        } else if self.op == "div" {
            assert!(
                self.operands.len() == 3,
                "div instruction must have exactly three operands: {}",
                self
            );
            self.operands[1] = f(self.operands[1].clone());
            self.operands[2] = f(self.operands[2].clone());
        } else if self.op == "rem" {
            assert!(
                self.operands.len() == 3,
                "rem instruction must have exactly three operands: {}",
                self
            );
            self.operands[1] = f(self.operands[1].clone());
            self.operands[2] = f(self.operands[2].clone());
        } else if self.op == "slt" {
            assert!(
                self.operands.len() == 3,
                "slt instruction must have exactly three operands: {}",
                self
            );
            self.operands[1] = f(self.operands[1].clone());
            self.operands[2] = f(self.operands[2].clone());
        } else if self.op == "sgt" {
            assert!(
                self.operands.len() == 3,
                "sgt instruction must have exactly three operands: {}",
                self
            );
            self.operands[1] = f(self.operands[1].clone());
            self.operands[2] = f(self.operands[2].clone());
        } else if self.op == "xori" {
            assert!(
                self.operands.len() == 3,
                "xori instruction must have exactly three operands: {}",
                self
            );
            self.operands[1] = f(self.operands[1].clone());
        } else if self.op == "snez" {
            assert!(
                self.operands.len() == 2,
                "snez instruction must have exactly two operands: {}",
                self
            );
            self.operands[1] = f(self.operands[1].clone());
        } else if self.op == "and" {
            assert!(
                self.operands.len() == 3,
                "and instruction must have exactly three operands: {}",
                self
            );
            self.operands[1] = f(self.operands[1].clone());
            self.operands[2] = f(self.operands[2].clone());
        } else if self.op == "or" {
            assert!(
                self.operands.len() == 3,
                "or instruction must have exactly three operands: {}",
                self
            );
            self.operands[1] = f(self.operands[1].clone());
            self.operands[2] = f(self.operands[2].clone());
        } else if self.op == "sw" {
            assert!(
                self.operands.len() == 2,
                "sw instruction must have exactly two operands: {}",
                self
            );
            self.operands[0] = f(self.operands[0].clone());
            if let Some((o, v)) = extract_register_from_mem_operand(&self.operands[1]) {
                self.operands[1] = format!("{}({})", o, f(v));
            } else {
                panic!(
                    "Invalid memory operand in sw instruction: {}",
                    self.operands[1]
                );
            }
        } else if self.op == "mv" {
            assert!(
                self.operands.len() == 2,
                "mv instruction must have exactly two operands: {}",
                self
            );
            self.operands[1] = f(self.operands[1].clone());
        } else if self.op == "la" {
            assert!(
                self.operands.len() == 2,
                "la instruction must have exactly two operands: {}",
                self
            );
        } else if self.op == "lw" {
            assert!(
                self.operands.len() == 2,
                "lw instruction must have exactly two operands: {}",
                self
            );
            if let Some((o, v)) = extract_register_from_mem_operand(&self.operands[1]) {
                self.operands[1] = format!("{}({})", o, f(v));
            } else {
                panic!(
                    "Invalid memory operand in lw instruction: {}",
                    self.operands[1]
                );
            }
        } else if self.op == "j" {
            assert!(
                self.operands.len() == 1,
                "j instruction must have exactly one operand: {}",
                self
            );
        } else if self.op == "Ret" {
            assert!(
                self.operands.len() <= 1,
                "Ret instruction must have <= 1 operand: {}",
                self
            );
            if !self.operands.is_empty() {
                self.operands[0] = f(self.operands[0].clone());
            }
        } else if self.op == "Alloc" {
            assert!(
                self.operands.len() == 2,
                "Alloc instruction must have exactly two operands: {}",
                self
            );
        } else if self.op == "Br" {
            assert!(
                self.operands.len() == 3,
                "Br instruction must have exactly three operands: {}",
                self
            );
            self.operands[0] = f(self.operands[0].clone());
        } else if self.op == "Call_1" {
            assert!(
                self.operands.len() >= 1,
                "Call_1 instruction must have at least one operand: {}",
                self
            );
            for i in 1..self.operands.len() {
                self.operands[i] = f(self.operands[i].clone());
            }
        } else if self.op == "Call_2" {
            assert!(
                self.operands.len() >= 2,
                "Call_2 instruction must have at least two operands: {}",
                self
            );
            for i in 2..self.operands.len() {
                self.operands[i] = f(self.operands[i].clone());
            }
            // After this are instructions for riscv passes
        } else if self.op == "call" 
        {
            assert!(self.operands.len() == 1, "call instruction must have exactly one operand: {}", self);
        }
        else {
            panic!("Unknown instruction: {}", self);
        }
    }

    pub fn map_write_vars(&mut self, f: impl FnOnce(String) -> String) {
        if self.op == "li" {
            assert!(
                self.operands.len() == 2,
                "li instruction must have exactly two operands: {}",
                self
            );
            self.operands[0] = f(self.operands[0].clone());
        } else if self.op == "xor" {
            assert!(
                self.operands.len() == 3,
                "xor instruction must have exactly three operands: {}",
                self
            );
            self.operands[0] = f(self.operands[0].clone());
        } else if self.op == "seqz" {
            assert!(
                self.operands.len() == 2,
                "seqz instruction must have exactly two operands: {}",
                self
            );
            self.operands[0] = f(self.operands[0].clone());
        } else if self.op == "add" {
            assert!(
                self.operands.len() == 3,
                "add instruction must have exactly three operands: {}",
                self
            );
            self.operands[0] = f(self.operands[0].clone());
        } else if self.op == "sub" {
            assert!(
                self.operands.len() == 3,
                "sub instruction must have exactly three operands: {}",
                self
            );
            self.operands[0] = f(self.operands[0].clone());
        } else if self.op == "mul" {
            assert!(
                self.operands.len() == 3,
                "mul instruction must have exactly three operands: {}",
                self
            );
            self.operands[0] = f(self.operands[0].clone());
        } else if self.op == "div" {
            assert!(
                self.operands.len() == 3,
                "div instruction must have exactly three operands: {}",
                self
            );
            self.operands[0] = f(self.operands[0].clone());
        } else if self.op == "rem" {
            assert!(
                self.operands.len() == 3,
                "rem instruction must have exactly three operands: {}",
                self
            );
            self.operands[0] = f(self.operands[0].clone());
        } else if self.op == "slt" {
            assert!(
                self.operands.len() == 3,
                "slt instruction must have exactly three operands: {}",
                self
            );
            self.operands[0] = f(self.operands[0].clone());
        } else if self.op == "sgt" {
            assert!(
                self.operands.len() == 3,
                "sgt instruction must have exactly three operands: {}",
                self
            );
            self.operands[0] = f(self.operands[0].clone());
        } else if self.op == "xori" {
            assert!(
                self.operands.len() == 3,
                "xori instruction must have exactly three operands: {}",
                self
            );
            self.operands[0] = f(self.operands[0].clone());
        } else if self.op == "snez" {
            assert!(
                self.operands.len() == 2,
                "snez instruction must have exactly two operands: {}",
                self
            );
            self.operands[0] = f(self.operands[0].clone());
        } else if self.op == "and" {
            assert!(
                self.operands.len() == 3,
                "and instruction must have exactly three operands: {}",
                self
            );
            self.operands[0] = f(self.operands[0].clone());
        } else if self.op == "or" {
            assert!(
                self.operands.len() == 3,
                "or instruction must have exactly three operands: {}",
                self
            );
            self.operands[0] = f(self.operands[0].clone());
        } else if self.op == "sw" {
            assert!(
                self.operands.len() == 2,
                "sw instruction must have exactly two operands: {}",
                self
            );
        } else if self.op == "mv" {
            assert!(
                self.operands.len() == 2,
                "mv instruction must have exactly two operands: {}",
                self
            );
            self.operands[0] = f(self.operands[0].clone());
        } else if self.op == "la" {
            assert!(
                self.operands.len() == 2,
                "la instruction must have exactly two operands: {}",
                self
            );
            self.operands[0] = f(self.operands[0].clone());
        } else if self.op == "lw" {
            assert!(
                self.operands.len() == 2,
                "lw instruction must have exactly two operands: {}",
                self
            );
            self.operands[0] = f(self.operands[0].clone());
        } else if self.op == "j" {
            assert!(
                self.operands.len() == 1,
                "j instruction must have exactly one operand: {}",
                self
            );
        } else if self.op == "Ret" {
            assert!(
                self.operands.len() <= 1,
                "Ret instruction must have <= 1 operand: {}",
                self
            );
        } else if self.op == "Alloc" {
            assert!(
                self.operands.len() == 2,
                "Alloc instruction must have exactly two operands: {}",
                self
            );
            self.operands[0] = f(self.operands[0].clone());
        } else if self.op == "Br" {
            assert!(
                self.operands.len() == 3,
                "Br instruction must have exactly three operands: {}",
                self
            );
        } else if self.op == "Call_1" {
            assert!(
                self.operands.len() >= 1,
                "Call_1 instruction must have at least one operand: {}",
                self
            );
        } else if self.op == "Call_2" {
            assert!(
                self.operands.len() >= 2,
                "Call_2 instruction must have at least two operands: {}",
                self
            );
            self.operands[0] = f(self.operands[0].clone());
            // After this are instructions for riscv passes
        } else if self.op == "call" 
        {
            assert!(self.operands.len() == 1, "call instruction must have exactly one operand: {}", self);
            self.operands[0] = f(self.operands[0].clone());
        } else
        {
            panic!("Unknown instruction: {}", self);
        }
    }

    pub fn kill_vars(&self) -> Vec<String> {
        if self.op == "li" {
            assert!(
                self.operands.len() == 2,
                "li instruction must have exactly two operands: {}",
                self
            );
            vec![self.operands[0].clone()]
        } else if self.op == "xor" {
            assert!(
                self.operands.len() == 3,
                "xor instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[0].clone()]
        } else if self.op == "seqz" {
            assert!(
                self.operands.len() == 2,
                "seqz instruction must have exactly two operands: {}",
                self
            );
            vec![self.operands[0].clone()]
        } else if self.op == "add" {
            assert!(
                self.operands.len() == 3,
                "add instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[0].clone()]
        } else if self.op == "sub" {
            assert!(
                self.operands.len() == 3,
                "sub instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[0].clone()]
        } else if self.op == "mul" {
            assert!(
                self.operands.len() == 3,
                "mul instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[0].clone()]
        } else if self.op == "div" {
            assert!(
                self.operands.len() == 3,
                "div instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[0].clone()]
        } else if self.op == "rem" {
            assert!(
                self.operands.len() == 3,
                "rem instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[0].clone()]
        } else if self.op == "slt" {
            assert!(
                self.operands.len() == 3,
                "slt instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[0].clone()]
        } else if self.op == "sgt" {
            assert!(
                self.operands.len() == 3,
                "sgt instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[0].clone()]
        } else if self.op == "xori" {
            assert!(
                self.operands.len() == 3,
                "xori instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[0].clone()]
        } else if self.op == "snez" {
            assert!(
                self.operands.len() == 2,
                "snez instruction must have exactly two operands: {}",
                self
            );
            vec![self.operands[0].clone()]
        } else if self.op == "and" {
            assert!(
                self.operands.len() == 3,
                "and instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[0].clone()]
        } else if self.op == "or" {
            assert!(
                self.operands.len() == 3,
                "or instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[0].clone()]
        } else if self.op == "sw" {
            assert!(
                self.operands.len() == 2,
                "sw instruction must have exactly two operands: {}",
                self
            );
            vec![]
        } else if self.op == "mv" {
            assert!(
                self.operands.len() == 2,
                "mv instruction must have exactly two operands: {}",
                self
            );
            vec![self.operands[0].clone()]
        } else if self.op == "la" {
            assert!(
                self.operands.len() == 2,
                "la instruction must have exactly two operands: {}",
                self
            );
            vec![self.operands[0].clone()]
        } else if self.op == "lw" {
            assert!(
                self.operands.len() == 2,
                "lw instruction must have exactly two operands: {}",
                self
            );
            vec![self.operands[0].clone()]
        } else if self.op == "j" {
            assert!(
                self.operands.len() == 1,
                "j instruction must have exactly one operand: {}",
                self
            );
            vec![]
        } else if self.op == "Ret" {
            assert!(
                self.operands.len() <= 1,
                "Ret instruction must have <= 1 operand: {}",
                self
            );
            vec![]
        } else if self.op == "Alloc" {
            assert!(
                self.operands.len() == 2,
                "Alloc instruction must have exactly two operands: {}",
                self
            );
            vec![self.operands[0].clone()]
        } else if self.op == "Br" {
            assert!(
                self.operands.len() == 3,
                "Br instruction must have exactly three operands: {}",
                self
            );
            vec![]
        } else if self.op == "Call_1" {
            assert!(
                self.operands.len() >= 1,
                "Call_1 instruction must have at least one operand: {}",
                self
            );
            vec![]
        } else if self.op == "Call_2" {
            assert!(
                self.operands.len() >= 2,
                "Call_2 instruction must have at least two operands: {}",
                self
            );
            vec![self.operands[0].clone()]
            // After this are instructions for riscv passes
        } else if self.op ==  "call"
        {
            assert!(self.operands.len() == 1, "call instruction must have exactly one operand: {}", self);
            riscv::RV_CALLER_SAVE_REGS
                .iter()
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
        } else
        {
            panic!("Unknown instruction: {}", self);
        }
    }

    pub fn gen_vars(&self) -> Vec<String> {
        if self.op == "li" {
            assert!(
                self.operands.len() == 2,
                "li instruction must have exactly two operands: {}",
                self
            );
            vec![]
        } else if self.op == "xor" {
            assert!(
                self.operands.len() == 3,
                "xor instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[1].clone(), self.operands[2].clone()]
        } else if self.op == "seqz" {
            assert!(
                self.operands.len() == 2,
                "seqz instruction must have exactly two operands: {}",
                self
            );
            vec![self.operands[1].clone()]
        } else if self.op == "add" {
            assert!(
                self.operands.len() == 3,
                "add instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[1].clone(), self.operands[2].clone()]
        } else if self.op == "sub" {
            assert!(
                self.operands.len() == 3,
                "sub instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[1].clone(), self.operands[2].clone()]
        } else if self.op == "mul" {
            assert!(
                self.operands.len() == 3,
                "mul instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[1].clone(), self.operands[2].clone()]
        } else if self.op == "div" {
            assert!(
                self.operands.len() == 3,
                "div instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[1].clone(), self.operands[2].clone()]
        } else if self.op == "rem" {
            assert!(
                self.operands.len() == 3,
                "rem instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[1].clone(), self.operands[2].clone()]
        } else if self.op == "slt" {
            assert!(
                self.operands.len() == 3,
                "slt instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[1].clone(), self.operands[2].clone()]
        } else if self.op == "sgt" {
            assert!(
                self.operands.len() == 3,
                "sgt instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[1].clone(), self.operands[2].clone()]
        } else if self.op == "xori" {
            assert!(
                self.operands.len() == 3,
                "xori instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[1].clone()]
        } else if self.op == "snez" {
            assert!(
                self.operands.len() == 2,
                "snez instruction must have exactly two operands: {}",
                self
            );
            vec![self.operands[1].clone()]
        } else if self.op == "and" {
            assert!(
                self.operands.len() == 3,
                "and instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[1].clone(), self.operands[2].clone()]
        } else if self.op == "or" {
            assert!(
                self.operands.len() == 3,
                "or instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[1].clone(), self.operands[2].clone()]
        } else if self.op == "sw" {
            assert!(
                self.operands.len() == 2,
                "sw instruction must have exactly two operands: {}",
                self
            );
            let mut used_vars = vec![self.operands[0].clone()];
            if let Some((_, s)) = extract_register_from_mem_operand(&self.operands[1]) {
                used_vars.push(s);
            } else {
                panic!(
                    "Invalid memory operand in sw instruction: {}",
                    self.operands[1]
                );
            }
            used_vars
        } else if self.op == "mv" {
            assert!(
                self.operands.len() == 2,
                "mv instruction must have exactly two operands: {}",
                self
            );
            vec![self.operands[1].clone()]
        } else if self.op == "la" {
            assert!(
                self.operands.len() == 2,
                "la instruction must have exactly two operands: {}",
                self
            );
            vec![]
        } else if self.op == "lw" {
            assert!(
                self.operands.len() == 2,
                "lw instruction must have exactly two operands: {}",
                self
            );
            let mut used_vars = vec![];
            if let Some((_, s)) = extract_register_from_mem_operand(&self.operands[1]) {
                used_vars.push(s);
            } else {
                panic!(
                    "Invalid memory operand in lw instruction: {}",
                    self.operands[1]
                );
            }
            used_vars
        } else if self.op == "j" {
            assert!(
                self.operands.len() == 1,
                "j instruction must have exactly one operand: {}",
                self
            );
            vec![]
        } else if self.op == "Ret" {
            assert!(
                self.operands.len() <= 1,
                "Ret instruction must have <= 1 operand: {}",
                self
            );
            if !self.operands.is_empty() {
                vec![self.operands[0].clone()]
            } else {
                vec![]
            }
        } else if self.op == "Alloc" {
            assert!(
                self.operands.len() == 2,
                "Alloc instruction must have exactly two operands: {}",
                self
            );
            vec![]
        } else if self.op == "Br" {
            assert!(
                self.operands.len() == 3,
                "Br instruction must have exactly three operands: {}",
                self
            );
            vec![self.operands[0].clone()]
        } else if self.op == "Call_1" {
            assert!(
                self.operands.len() >= 1,
                "Call_1 instruction must have at least one operand: {}",
                self
            );
            self.operands[1..].to_vec()
        } else if self.op == "Call_2" {
            assert!(
                self.operands.len() >= 2,
                "Call_2 instruction must have at least two operands: {}",
                self
            );
            self.operands[2..].to_vec()
            // After this are instructions for riscv passes
        } else if self.op == "call" 
        {
            assert!(self.operands.len() == 1, "call instruction must have exactly one operand: {}", self);
            let argc = self.comment.trim().parse::<usize>().expect("Invalid argument count in call instruction comment");
            assert!(argc <= 8, "Argument count must be <= 8 for call instruction: {}", argc);
            [
                "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"
            ].iter()
                .take(argc)
                .map(|s| s.to_string())
                .collect::<Vec<String>>()
        } else
        {
            panic!("Unknown instruction: {}", self);
        }
    }
}

pub fn active_analyze(
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
    out: HashSet<String>,
    actives: Option<&mut LinkedList<HashSet<String>>>,
) -> HashSet<String> {
    // This function is similar to `active_analyze`, but it does not deem value not needed as active
    let mut binding = LinkedList::new();
    let actives = actives.unwrap_or(&mut binding);
    let mut now = out.clone();
    actives.clear();
    actives.push_back(now.clone());
    for i in (0..block.instrs.len()).rev() {
        let instr = &block.instrs[i];
        let kill_vars = instr.kill_vars();
        let gen_vars = instr.gen_vars();
        let mut killed = true;
        assert!(kill_vars.len() <= 1, "Kill variables must <= 1");
        for var in kill_vars {
            killed = now.remove(&var);
        }

        if killed || instr.is_side_effecting() {
            for var in gen_vars {
                now.insert(var);
            }
        }

        actives.push_front(now.clone());
    }
    now
}

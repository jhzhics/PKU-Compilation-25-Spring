use std::collections::LinkedList;

use koopa::ir::layout::InstList;

#[derive(Debug, Clone)]
pub struct Instr
{
    op: String,
    operands: Vec<String>,
}

impl Instr {
    pub fn new(s: &str) -> Self {
        let mut operands = Vec::new();
        let mut parts = s.split(',');
        let mut part1 = parts.next().expect("Instruction must have an operator").trim().split_whitespace();
        let op = part1.next().expect("Instruction must have an operator").to_string();
        if let Some(oprand1) = part1.next() {
            operands.push(oprand1.to_string());
        }
        assert!(part1.next().is_none(), "Incorrect instruction format: {}", s);
        for part in parts {
            assert!(!operands.is_empty(), "Incorrect instruction format: {}", s);
            let trimmed = part.trim();
            assert!(!trimmed.is_empty(), "Empty operand in instruction: {}", s);
            assert!(!trimmed.contains(' '), "Operand should not contain spaces: {}", trimmed);
            operands.push(trimmed.to_string());
        }
        Instr {
            op,
            operands,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Block
{
    pub name: String,
    pub instrs: Vec<Instr>,
}

impl Block {
    pub fn new(name: &str) -> Self {
        assert!(!name.is_empty(), "Block name cannot be empty");
        assert!(!name.contains(' '), "Block name should not contain spaces: {}", name);
        Block {
            name: name.to_string(),
            instrs: Vec::new(),
        }
    }

    pub fn dump(&self) -> LinkedList<String> {
        let mut inst_list = LinkedList::new();
        inst_list.push_back(format!("{}:", self.name));
        for instr in &self.instrs {
            inst_list.push_back(format!("  {} {}\n", instr.op, instr.operands.join(", ")));
        }
        inst_list
    }
}
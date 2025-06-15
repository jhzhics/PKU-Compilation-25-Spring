use std::{collections::LinkedList, fmt::Display};

#[derive(Debug, Clone)]
pub struct Instr {
    pub op: String,
    pub operands: Vec<String>,
}

impl Instr {
    pub fn new(s: &str) -> Self {
        let mut operands = Vec::new();
        let mut parts = s.split(',');
        let mut part1 = parts
            .next()
            .expect("Instruction must have an operator")
            .trim()
            .split_whitespace();
        let op = part1
            .next()
            .expect("Instruction must have an operator")
            .to_string();
        if let Some(oprand1) = part1.next() {
            operands.push(oprand1.to_string());
        }
        assert!(
            part1.next().is_none(),
            "Incorrect instruction format: {}",
            s
        );
        for part in parts {
            assert!(!operands.is_empty(), "Incorrect instruction format: {}", s);
            let trimmed = part.trim();
            assert!(!trimmed.is_empty(), "Empty operand in instruction: {}", s);
            assert!(
                !trimmed.contains(' '),
                "Operand should not contain spaces: {}",
                trimmed
            );
            operands.push(trimmed.to_string());
        }
        Instr { op, operands }
    }

    pub fn is_side_effecting(&self) -> bool {
        match self.op.as_str() {
            "li" | "xor" | "seqz" | "add" | "sub" | "mul" | "div" | "rem" | "slt" | "sgt"
            | "xori" | "snez" | "and" | "or" | "mv" | "la" | "lw" => false,
            "sw" | "j" | "Ret" | "Alloc" | "Br" | "Call_1" | "Call_2" => true,
            _ => panic!("Unknown instruction in is_side_effecting: {}", self.op),
        }
    }
}

impl Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.op, self.operands.join(", "))
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub name: String,
    pub instrs: Vec<Instr>,
}

impl Block {
    pub fn new(name: String) -> Self {
        assert!(!name.is_empty(), "Block name cannot be empty");
        assert!(
            !name.contains(' '),
            "Block name should not contain spaces: {}",
            name
        );
        Block {
            name,
            instrs: Vec::new(),
        }
    }

    pub fn dump(&self) -> LinkedList<String> {
        let mut inst_list = LinkedList::new();
        inst_list.push_back(format!("{}:", self.name));
        for instr in &self.instrs {
            inst_list.push_back(format!("  {} {}", instr.op, instr.operands.join(", ")));
        }
        inst_list
    }
}

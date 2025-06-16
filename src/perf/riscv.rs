use std::{collections::LinkedList, fmt::Display};

#[allow(dead_code)]
pub const RV_CALLER_SAVE_REGS: [&str; 15] = [
    "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "t0", "t1", "t2", "t3", "t4", "t5", "t6",
];
#[allow(dead_code)]
pub const RV_CALLEE_SAVE_REGS: [&str; 12] = [
    "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11",
];
#[allow(dead_code)]
pub const RA_REG: &str = "ra";

#[allow(dead_code)]
pub const RV_SP_REG: &str = "sp";

#[allow(dead_code)]
pub const RV_ZERO_REG: &str = "zero";

#[allow(dead_code)]
pub fn is_real_reg(name: &str) -> bool {
    RV_CALLER_SAVE_REGS.contains(&name)
        || RV_CALLEE_SAVE_REGS.contains(&name)
        || name == RA_REG
        || name == RV_SP_REG
        || name == RV_ZERO_REG
}

pub const TMP_REG: &str = "rv_pass_tmp";

#[derive(Debug, Clone)]
pub struct Instr {
    pub op: String,
    pub operands: Vec<String>,
    #[allow(dead_code)]
    pub comment: String,
}

pub fn fit_in_imm12(value: i32) -> bool {
    value >= -2048 && value <= 2047
}

impl Instr {
    pub fn new(s: &str) -> Self {
        let (s, comment) = if let Some(idx) = s.find('#') {
            (&s[..idx], s[idx + 1..].trim().to_string())
        } else {
            (s, String::new())
        };
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
        Instr {
            op,
            operands,
            comment,
        }
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
    #[allow(dead_code)]
    pub fn dump(&self) -> LinkedList<String> {
        let mut inst_list = LinkedList::new();
        inst_list.push_back(format!("{}:", self.name));
        for instr in &self.instrs {
            inst_list.push_back(format!("  {} {}", instr.op, instr.operands.join(", ")));
        }
        inst_list
    }
}

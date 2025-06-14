use std::{collections::{HashSet, LinkedList}, fmt::Display};

use super::riscv::{Block};

#[derive(Debug, Clone)]
pub struct SSABlock {
    pub block: Block,
    pub prev: Vec<String>, // (name, operands)
    pub next: Vec<(String, Vec<String>)>, // (name, operands) 
    pub params: HashSet<String>, // parameters
}

impl SSABlock {
    pub fn new(name: String) -> Self {
        assert!(!name.is_empty(), "Block name cannot be empty");
        assert!(!name.contains(' '), "Block name should not contain spaces: {}", name);
        SSABlock {
            block: Block::new(name),
            prev: Vec::new(),
            next: Vec::new(),
            params: HashSet::new(),
        }
    }

    pub fn dump(&self) -> LinkedList<String> {
        let mut inst_list = self.block.dump();
        inst_list.pop_front(); // Remove the block name line
        inst_list.push_front(format!("{}({}):", self.block.name, self.params.iter().cloned().collect::<Vec<String>>().join(", ")));
        inst_list.push_back(format!("# {}",
        self.next.iter()
            .map(|(name, ops)| format!("{}({})", name, ops.join(", ")))
            .collect::<Vec<String>>()
            .join(", ")
        ));
        inst_list
    }
}

impl Default for SSABlock {
    fn default() -> Self {
        SSABlock::new("SSA_BLOCK".to_string())
    }
}

impl Display for SSABlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.dump().iter().map(String::as_str).collect::<Vec<&str>>().join("\n"))
    }
}
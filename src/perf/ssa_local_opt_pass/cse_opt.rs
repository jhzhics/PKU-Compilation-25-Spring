use super::riscv::Instr;

impl PartialEq for Instr {
    fn eq(&self, other: &Self) -> bool {
        if self.op == "li" {
            assert!(
                self.operands.len() == 2,
                "li instruction must have exactly two operands: {}",
                self
            );
            if other.op != "li" {
                return false;
            }
            self.operands[1] == other.operands[1]
        } else if self.op == "xor" {
            assert!(
                self.operands.len() == 3,
                "xor instruction must have exactly three operands: {}",
                self
            );
            if other.op != "xor" {
                return false;
            }
            self.operands[1] == other.operands[1] && self.operands[2] == other.operands[2]
                || self.operands[1] == other.operands[2] && self.operands[2] == other.operands[1]
        } else if self.op == "seqz" {
            assert!(
                self.operands.len() == 2,
                "seqz instruction must have exactly two operands: {}",
                self
            );
            if other.op != "seqz" {
                return false;
            }
            self.operands[1] == other.operands[1]
        } else if self.op == "add" {
            assert!(
                self.operands.len() == 3,
                "add instruction must have exactly three operands: {}",
                self
            );
            if other.op != "add" {
                return false;
            }
            
            self.operands[1] == other.operands[1] && self.operands[2] == other.operands[2]
            || self.operands[1] == other.operands[2] && self.operands[2] == other.operands[1]
        } else if self.op == "sub" {
            assert!(
                self.operands.len() == 3,
                "sub instruction must have exactly three operands: {}",
                self
            );
            if other.op != "sub" {
                return false;
            }
            self.operands[1] == other.operands[1] && self.operands[2] == other.operands[2]
        } else if self.op == "mul" {
            assert!(
                self.operands.len() == 3,
                "mul instruction must have exactly three operands: {}",
                self
            );
            if other.op != "mul" {
                return false;
            }
            self.operands[1] == other.operands[1] && self.operands[2] == other.operands[2]
                || self.operands[1] == other.operands[2] && self.operands[2] == other.operands[1]
        } else if self.op == "div" {
            assert!(
                self.operands.len() == 3,
                "div instruction must have exactly three operands: {}",
                self
            );
            if other.op != "div" {
                return false;
            }
            self.operands[1] == other.operands[1] && self.operands[2] == other.operands[2]
        } else if self.op == "rem" {
            assert!(
                self.operands.len() == 3,
                "rem instruction must have exactly three operands: {}",
                self
            );
            if other.op != "rem" {
                return false;
            }
            self.operands[1] == other.operands[1] && self.operands[2] == other.operands[2]
        } else if self.op == "slt" {
            assert!(
                self.operands.len() == 3,
                "slt instruction must have exactly three operands: {}",
                self
            );
            if other.op == "slt" {
                self.operands[1] == other.operands[1] && self.operands[2] == other.operands[2]
            } else if other.op == "sgt" {
                self.operands[1] == other.operands[2] && self.operands[2] == other.operands[1]
            } else {
                false
            }
        } else if self.op == "sgt" {
            assert!(
                self.operands.len() == 3,
                "sgt instruction must have exactly three operands: {}",
                self
            );
            if other.op == "sgt" {
                self.operands[1] == other.operands[1] && self.operands[2] == other.operands[2]
            } else if other.op == "slt" {
                self.operands[1] == other.operands[2] && self.operands[2] == other.operands[1]
            } else {
                false
            }
        } else if self.op == "xori" {
            assert!(
                self.operands.len() == 3,
                "xori instruction must have exactly three operands: {}",
                self
            );
            if other.op != "xori" {
                return false;
            }
            self.operands[1] == other.operands[1] && self.operands[2] == other.operands[2]
        } else if self.op == "snez" {
            assert!(
                self.operands.len() == 2,
                "snez instruction must have exactly two operands: {}",
                self
            );
            if other.op != "snez" {
                return false;
            }
            self.operands[1] == other.operands[1]
        } else if self.op == "and" {
            assert!(
                self.operands.len() == 3,
                "and instruction must have exactly three operands: {}",
                self
            );
            if other.op != "and" {
                return false;
            }
            self.operands[1] == other.operands[1] && self.operands[2] == other.operands[2]
        } else if self.op == "or" {
            assert!(
                self.operands.len() == 3,
                "or instruction must have exactly three operands: {}",
                self
            );
            if other.op != "or" {
                return false;
            }
            self.operands[1] == other.operands[1] && self.operands[2] == other.operands[2]
        } else if self.op == "sw" {
            assert!(
                self.operands.len() == 2,
                "sw instruction must have exactly two operands: {}",
                self
            );
            false
        } else if self.op == "mv" {
            assert!(
                self.operands.len() == 2,
                "mv instruction must have exactly two operands: {}",
                self
            );
            if other.op != "mv" {
                return false;
            }
            self.operands[1] == other.operands[1]
        } else if self.op == "la" {
            assert!(
                self.operands.len() == 2,
                "la instruction must have exactly two operands: {}",
                self
            );
            if other.op != "la" {
                return false;
            }
            self.operands[1] == other.operands[1]
        } else if self.op == "lw" {
            assert!(
                self.operands.len() == 2,
                "lw instruction must have exactly two operands: {}",
                self
            );
            false
        } else if self.op == "j" {
            assert!(
                self.operands.len() == 1,
                "j instruction must have exactly one operand: {}",
                self
            );
            false
        } else if self.op == "Ret" {
            assert!(
                self.operands.len() <= 1,
                "Ret instruction must have <= 1 operand: {}",
                self
            );
            false
        } else if self.op == "Alloc" {
            assert!(
                self.operands.len() == 2,
                "Alloc instruction must have exactly two operands: {}",
                self
            );
            false
        } else if self.op == "Br" {
            assert!(
                self.operands.len() == 3,
                "Br instruction must have exactly three operands: {}",
                self
            );
            false
        } else if self.op == "Call_1" {
            assert!(
                self.operands.len() >= 1,
                "Call_1 instruction must have at least one operand: {}",
                self
            );
            false
        } else if self.op == "Call_2" {
            assert!(
                self.operands.len() >= 2,
                "Call_2 instruction must have at least two operands: {}",
                self
            );
            false
        } else if self.op == "addi" {
            assert!(
                self.operands.len() == 3,
                "addi instruction must have exactly three operands: {}",
                self
            );
            if other.op != "addi" {
                return false;
            }
            self.operands[1] == other.operands[1] && self.operands[2] == other.operands[2]
        } else {
            panic!("Unknown instruction: {}", self);
        }
    }
}

fn is_expr_instr(instr: &Instr) -> bool {
    match instr.op.as_str() {
        "li" | "xor" | "seqz" | "add" | "sub" | "mul" | "div" | "rem" | "slt" | "sgt" | "xori"
        | "snez" | "and" | "or" | "mv" | "la" | "addi" => true,
        _ => false,
    }
}

pub fn pass(block: &mut super::ssa_form::SSABlock) -> bool {
    let mut exprs: Vec<Instr> = Vec::new();
    let mut changed = false;
    block.block.instrs.iter_mut().for_each(|instr| {
        if is_expr_instr(instr) {
            // Check if the instruction is already in the list of expressions
            if let Some(pos) = exprs.iter().position(|e| e.eq(instr)) {
                let old_rd = exprs[pos].operands[0].clone();
                let new_rd = instr.operands[0].clone();
                *instr = Instr::new(&format!("mv {}, {}", new_rd, old_rd));
                changed = true;
            } else {
                exprs.push(instr.clone());
            }
        }
    });
    changed
}

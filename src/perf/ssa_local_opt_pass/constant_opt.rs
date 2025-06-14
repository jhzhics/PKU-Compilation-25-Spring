//! This function performs constant propagation and folding on SSA form.

use std::collections::HashMap;
use super::riscv::Instr;

pub fn pass(block: &mut super::ssa_form::SSABlock) {
    let mut constant_map: HashMap<String, i32> = HashMap::new();
    for inst in &mut block.block.instrs
    {
        if inst.op == "li" {
            assert!(inst.operands.len() == 2, "li instruction must have exactly two operands: {}", inst);
            constant_map.insert(inst.operands[0].clone(), inst.operands[1].parse::<i32>().unwrap());
        } else if inst.op == "xor" {
            assert!(inst.operands.len() == 3, "xor instruction must have exactly three operands: {}", inst);
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = val1 ^ val2;
                    constant_map.insert(inst.operands[0].clone(), result);
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                }
            }
        } else if inst.op == "seqz" {
            assert!(inst.operands.len() == 2, "seqz instruction must have exactly two operands: {}", inst);
            if let Some(val) = constant_map.get(&inst.operands[1]) {
                let result = if *val == 0 { 1 } else { 0 };
                constant_map.insert(inst.operands[0].clone(), result);
                *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
            }
        } else if inst.op == "add" {
            assert!(inst.operands.len() == 3, "add instruction must have exactly three operands: {}", inst);
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = val1 + val2;
                    constant_map.insert(inst.operands[0].clone(), result);
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                }
            }
        } else if inst.op == "sub" {
            assert!(inst.operands.len() == 3, "sub instruction must have exactly three operands: {}", inst);
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = val1 - val2;
                    constant_map.insert(inst.operands[0].clone(), result);
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                }
            }
        } else if inst.op == "mul" {
            assert!(inst.operands.len() == 3, "mul instruction must have exactly three operands: {}", inst);
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = val1 * val2;
                    constant_map.insert(inst.operands[0].clone(), result);
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                }
            }
        } else if inst.op == "div" {
            assert!(inst.operands.len() == 3, "div instruction must have exactly three operands: {}", inst);
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    assert!(*val2 != 0, "Division by zero in instruction: {}", inst);
                    let result = val1 / val2;
                    constant_map.insert(inst.operands[0].clone(), result);
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                }
            }
        } else if inst.op == "rem" {
            assert!(inst.operands.len() == 3, "rem instruction must have exactly three operands: {}", inst);
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    assert!(*val2 != 0, "Division by zero in instruction: {}", inst);
                    let result = val1 % val2;
                    constant_map.insert(inst.operands[0].clone(), result);
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                }
            }
        } else if inst.op == "slt" {
            assert!(inst.operands.len() == 3, "slt instruction must have exactly three operands: {}", inst);
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = if val1 < val2 { 1 } else { 0 };
                    constant_map.insert(inst.operands[0].clone(), result);
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                }
            }
        } else if inst.op == "sgt" {
            assert!(inst.operands.len() == 3, "sgt instruction must have exactly three operands: {}", inst);
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = if val1 > val2 { 1 } else { 0 };
                    constant_map.insert(inst.operands[0].clone(), result);
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                }
            }
        } else if inst.op == "xori" {
            assert!(inst.operands.len() == 3, "xori instruction must have exactly three operands: {}", inst);
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Ok(val2) = inst.operands[2].parse::<i32>() {
                    let result = val1 ^ val2;
                    constant_map.insert(inst.operands[0].clone(), result);
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                }
            }
        } else if inst.op == "snez" {
            assert!(inst.operands.len() == 2, "snez instruction must have exactly two operands: {}", inst);
            if let Some(val) = constant_map.get(&inst.operands[1]) {
                let result = if *val != 0 { 1 } else { 0 };
                constant_map.insert(inst.operands[0].clone(), result);
                *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
            }
        } else if inst.op == "and" {
            assert!(inst.operands.len() == 3, "and instruction must have exactly three operands: {}", inst);
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = val1 & val2;
                    constant_map.insert(inst.operands[0].clone(), result);
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                }
            }
        } else if inst.op == "or" {
            assert!(inst.operands.len() == 3, "or instruction must have exactly three operands: {}", inst);
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = val1 | val2;
                    constant_map.insert(inst.operands[0].clone(), result);
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                }
            }
        } else if inst.op == "sw" {
            assert!(inst.operands.len() == 2, "sw instruction must have exactly two operands: {}", inst);
        } else if inst.op == "mv" {
            assert!(inst.operands.len() == 2, "mv instruction must have exactly two operands: {}", inst);
            if let Some(val) = constant_map.get(&inst.operands[1]) {
                constant_map.insert(inst.operands[0].clone(), *val);
            }
        } else if inst.op == "la" {
            assert!(inst.operands.len() == 2, "la instruction must have exactly two operands: {}", inst);
        } else if inst.op == "lw" {
            assert!(inst.operands.len() == 2, "lw instruction must have exactly two operands: {}", inst);
        } else if inst.op == "j" {
            assert!(inst.operands.len() == 1, "j instruction must have exactly one operand: {}", inst);
        } else if inst.op == "Ret" {
            assert!(inst.operands.len() <= 1, "Ret instruction must have <= 1 operand: {}", inst);
        } else if inst.op == "Alloc" {
            assert!(inst.operands.len() == 2, "Alloc instruction must have exactly two operands: {}", inst);
        } else if inst.op == "Br" {
            assert!(inst.operands.len() == 3, "Br instruction must have exactly three operands: {}", inst);
        } else if inst.op == "Call_1" {
            assert!(inst.operands.len() >= 1, "Call_1 instruction must have at least one operand: {}", inst);
        } else if inst.op == "Call_2" {
            assert!(inst.operands.len() >= 2, "Call_2 instruction must have at least two operands: {}", inst);
        } else {
            panic!("Unknown instruction: {}", inst);
        }
    }
}
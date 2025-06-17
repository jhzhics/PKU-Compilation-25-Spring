use std::collections::HashMap;

use crate::perf::riscv::{self, fit_in_imm12, fit_in_shamt};

use super::rv_pass1::RVPass1Block;
use super::riscv::Instr;
use super::rv_active_aly::extract_register_from_mem_operand;

fn is_power_of_two(n: i32) -> bool {
    n > 0 && (n & (n - 1)) == 0
}

pub fn pass(block: &mut RVPass1Block) {
    let mut constant_map: HashMap<String, i32> = HashMap::new();
    constant_map.insert(riscv::RV_ZERO_REG.to_string(), 0);
    for inst in &mut block.block.instrs {
        let mut known = false;
        if inst.op == "li" {
            assert!(
                inst.operands.len() == 2,
                "li instruction must have exactly two operands: {}",
                inst
            );
            constant_map.insert(inst.operands[0].clone(),inst.operands[1].parse::<i32>().unwrap());
			known = true;
        } else if inst.op == "xor" {
            assert!(
                inst.operands.len() == 3,
                "xor instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = val1 ^ val2;
                    constant_map.insert(inst.operands[0].clone(), result);
			known = true;
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                } else if fit_in_imm12(*val1) {
                    *inst = Instr::new(&format!("xori {}, {}, {}", inst.operands[0], inst.operands[2], val1));
                }
            } else if let Some(val) = constant_map.get(&inst.operands[2]) {
                if fit_in_imm12(*val) {
                    *inst = Instr::new(&format!("xori {}, {}, {}", inst.operands[0], inst.operands[1], val));
                }
            }
        } else if inst.op == "seqz" {
            assert!(
                inst.operands.len() == 2,
                "seqz instruction must have exactly two operands: {}",
                inst
            );
            if let Some(val) = constant_map.get(&inst.operands[1]) {
                let result = if *val == 0 { 1 } else { 0 };
                constant_map.insert(inst.operands[0].clone(), result);
			known = true;
                *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
            }
        } else if inst.op == "add" {
            assert!(
                inst.operands.len() == 3,
                "add instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = val1 + val2;
                    constant_map.insert(inst.operands[0].clone(), result);
			known = true;
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                } else if fit_in_imm12(*val1) {
                    *inst = Instr::new(&format!("addi {}, {}, {}", inst.operands[0], inst.operands[2], val1));
                }
            } else if let Some(val) = constant_map.get(&inst.operands[2]) {
                if fit_in_imm12(*val) {
                    *inst = Instr::new(&format!("addi {}, {}, {}", inst.operands[0], inst.operands[1], val));
                }
            }
        } else if inst.op == "sub" {
            assert!(
                inst.operands.len() == 3,
                "sub instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = val1 - val2;
                    constant_map.insert(inst.operands[0].clone(), result);
			known = true;
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                } else if *val1 == 0 {
                    inst.operands[1] = riscv::RV_ZERO_REG.to_string();
                }
            } else if let Some(val) = constant_map.get(&inst.operands[2]) {
                if fit_in_imm12(-(*val)) {
                    *inst = Instr::new(&format!("addi {}, {}, {}", inst.operands[0], inst.operands[1], -val));
                }
            }
        } else if inst.op == "mul" {
            assert!(
                inst.operands.len() == 3,
                "mul instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = val1 * val2;
                    constant_map.insert(inst.operands[0].clone(), result);
			known = true;
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                } else if *val1 == 0 {
                    constant_map.insert(inst.operands[0].clone(), 0);
			known = true;
                    *inst = Instr::new(&format!("li {}, 0", inst.operands[0]));
                } else if is_power_of_two(*val1) && fit_in_shamt(*val1) {
                    let shamt = val1.ilog(2);
                    *inst = Instr::new(&format!("slli {}, {}, {}", inst.operands[0], inst.operands[2], shamt));
                }
            } else if let Some(val) = constant_map.get(&inst.operands[2]) {
                if *val == 0 {
                    constant_map.insert(inst.operands[0].clone(), 0);
			known = true;
                    *inst = Instr::new(&format!("li {}, 0", inst.operands[0]));
                } else if is_power_of_two(*val) && fit_in_shamt(*val) {
                    let shamt = val.ilog(2);
                    *inst = Instr::new(&format!("slli {}, {}, {}", inst.operands[0], inst.operands[1], shamt));
                }
            }
        } else if inst.op == "div" {
            assert!(
                inst.operands.len() == 3,
                "div instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    assert!(*val2 != 0, "Division by zero in instruction: {}", inst);
                    let result = val1 / val2;
                    constant_map.insert(inst.operands[0].clone(), result);
			        known = true;
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                } else if *val1 == 0 {
                    *inst = Instr::new(&format!("div {}, {}, {}", inst.operands[0], riscv::RV_ZERO_REG, inst.operands[2]));
                }
            } else if let Some(val) = constant_map.get(&inst.operands[2]) {
                if *val == 1 {
                    *inst = Instr::new(&format!("mv {}, {}", inst.operands[0], inst.operands[1]));
                }
            }
        } else if inst.op == "rem" {
            assert!(
                inst.operands.len() == 3,
                "rem instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    assert!(*val2 != 0, "Division by zero in instruction: {}", inst);
                    let result = val1 % val2;
                    constant_map.insert(inst.operands[0].clone(), result);
			known = true;
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                } else if *val1 == 0 {
                    *inst = Instr::new(&format!("rem {}, {}, {}", inst.operands[0], riscv::RV_ZERO_REG, inst.operands[2]));
                }
            } else if let Some(val) = constant_map.get(&inst.operands[2]) {
                if *val == 1 {
                    constant_map.insert(inst.operands[0].clone(), 0);
			known = true;
                    *inst = Instr::new(&format!("li {}, 0", inst.operands[0]));
                }
            }
        } else if inst.op == "slt" {
            assert!(
                inst.operands.len() == 3,
                "slt instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = if val1 < val2 { 1 } else { 0 };
                    constant_map.insert(inst.operands[0].clone(), result);
			known = true;
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                } else if *val1 == 0 {
                    *inst = Instr::new(&format!("slt {}, {}, {}", inst.operands[0], riscv::RV_ZERO_REG, inst.operands[2]));
                }
            } else if let Some(val) = constant_map.get(&inst.operands[2]) {
                if fit_in_imm12(*val) {
                    *inst = Instr::new(&format!("slti {}, {}, {}", inst.operands[0], inst.operands[1], val));
                }
            }
        } else if inst.op == "sgt" {
            panic!("sgt instruction should not appears in peephole optimization: {}", inst);
        } else if inst.op == "xori" {
            assert!(
                inst.operands.len() == 3,
                "xori instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Ok(val2) = inst.operands[2].parse::<i32>() {
                    let result = val1 ^ val2;
                    constant_map.insert(inst.operands[0].clone(), result);
			known = true;
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                }
                else {
                    panic!("Invalid immediate value in xori instruction: {}", inst);
                }
            }
        } else if inst.op == "snez" {
            assert!(
                inst.operands.len() == 2,
                "snez instruction must have exactly two operands: {}",
                inst
            );
            if let Some(val) = constant_map.get(&inst.operands[1]) {
                let result = if *val != 0 { 1 } else { 0 };
                constant_map.insert(inst.operands[0].clone(), result);
			known = true;
                *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
            }
        } else if inst.op == "and" {
            assert!(
                inst.operands.len() == 3,
                "and instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = val1 & val2;
                    constant_map.insert(inst.operands[0].clone(), result);
			known = true;
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                } else if *val1 == 0 {
                    constant_map.insert(inst.operands[0].clone(), 0);
			known = true;
                    *inst = Instr::new(&format!("li {}, 0", inst.operands[0]));
                } else if fit_in_imm12(*val1) {
                    *inst = Instr::new(&format!("andi {}, {}, {}", inst.operands[0], inst.operands[2], val1));
                } else if *val1 == -1 {
                    *inst = Instr::new(&format!("mv {}, {}", inst.operands[0], inst.operands[2]));
                }
            }  else if let Some(val) = constant_map.get(&inst.operands[2]) {
                if *val == 0 {
                    constant_map.insert(inst.operands[0].clone(), 0);
			known = true;
                    *inst = Instr::new(&format!("li {}, 0", inst.operands[0]));
                } else if fit_in_imm12(*val) {
                    *inst = Instr::new(&format!("andi {}, {}, {}", inst.operands[0], inst.operands[1], val));
                } else if *val == -1 {
                    *inst = Instr::new(&format!("mv {}, {}", inst.operands[0], inst.operands[1]));
                }
            }
        } else if inst.op == "or" {
            assert!(
                inst.operands.len() == 3,
                "or instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Some(val2) = constant_map.get(&inst.operands[2]) {
                    let result = val1 | val2;
                    constant_map.insert(inst.operands[0].clone(), result);
			        known = true;
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                } else  if *val1 == -1 {
                    constant_map.insert(inst.operands[0].clone(), -1);
			        known = true;
                    *inst = Instr::new(&format!("li {}, -1", inst.operands[0]));
                } else if *val1 == 0 {
                    *inst = Instr::new(&format!("mv {}, {}", inst.operands[0], inst.operands[2]));
                } else if fit_in_imm12(*val1) {
                    *inst = Instr::new(&format!("ori {}, {}, {}", inst.operands[0], inst.operands[2], val1));
                }
            }  else if let Some(val) = constant_map.get(&inst.operands[2]) {
                if *val == -1 {
                    constant_map.insert(inst.operands[0].clone(), -1);
			        known = true;
                    *inst = Instr::new(&format!("li {}, -1", inst.operands[0]));
                }  else if *val == 0 {
                    *inst = Instr::new(&format!("mv {}, {}", inst.operands[0], inst.operands[1]));
                } else if fit_in_imm12(*val) {
                    *inst = Instr::new(&format!("ori {}, {}, {}", inst.operands[0], inst.operands[1], val));
                }
            }
        } else if inst.op == "sw" {
            assert!(
                inst.operands.len() == 2,
                "sw instruction must have exactly two operands: {}",
                inst
            );
        } else if inst.op == "mv" {
            assert!(
                inst.operands.len() == 2,
                "mv instruction must have exactly two operands: {}",
                inst
            );
            if let Some(&val) = constant_map.get(&inst.operands[1]) {
                constant_map.insert(inst.operands[0].clone(), val);
			known = true;
                *inst = Instr::new(&format!("li {}, {}", inst.operands[0], val));
            }
        } else if inst.op == "addi" {
            assert!(
                inst.operands.len() == 3,
                "addi instruction must have exactly three operands: {}",
                inst
            );
            if let Some(val1) = constant_map.get(&inst.operands[1]) {
                if let Ok(val2) = inst.operands[2].parse::<i32>() {
                    let result = val1 + val2;
                    constant_map.insert(inst.operands[0].clone(), result);
                    known = true;
                    *inst = Instr::new(&format!("li {}, {}", inst.operands[0], result));
                }
                else {
                    panic!("Invalid immediate value in addi instruction: {}", inst);
                }
            }
        } else if inst.op == "la" {
            assert!(
                inst.operands.len() == 2,
                "la instruction must have exactly two operands: {}",
                inst
            );
        } else if inst.op == "lw" {
            assert!(
                inst.operands.len() == 2,
                "lw instruction must have exactly two operands: {}",
                inst
            );
        } else if inst.op == "j" {
            assert!(
                inst.operands.len() == 1,
                "j instruction must have exactly one operand: {}",
                inst
            );
        } else if inst.op == "bnez" {
            assert!(
                inst.operands.len() == 2,
                "bne instruction must have exactly two operands: {}",
                inst
            );
        }
        else if inst.op == "call" {
            assert!(
                inst.operands.len() == 1,
                "call instruction must have exactly one operand: {}",
                inst
            );
        } else if inst.op == "ret" {
            assert!(inst.operands.is_empty(), "ret instruction must have no operands: {}", inst);
        } else 
        {
            panic!("Unknown instruction: {}", inst);
        }
        if !known {
            for kill in &inst.kill_vars() {
                constant_map.remove(kill);
            }
        }
    }

    for (i, inst) in block.block.instrs.clone().iter().enumerate() {
        if i == 0
        {
            continue;
        }
        if block.block.instrs[i - 1].op == "addi"{
            let last_inst = &block.block.instrs[i - 1];
            let last_imm = last_inst.operands[2].parse::<i32>().unwrap();
            let last_dst_reg = &last_inst.operands[0];
            let last_src_reg = &last_inst.operands[1];
            if last_dst_reg == last_src_reg {
                continue;
            }
            if inst.op == "sw"
            {
                assert!(
                    inst.operands.len() == 2,
                    "sw instruction must have exactly two operands: {}",
                    inst
                );
                if let Some((off, reg)) = extract_register_from_mem_operand(&inst.operands[1]) {
                    let offset = off.parse::<i32>().unwrap();
                    if &reg != last_dst_reg {
                        continue;
                    }
                    let new_new_offset = offset + last_imm;
                    if fit_in_imm12(new_new_offset) {
                        block.block.instrs[i] = Instr::new(&format!("sw {}, {}({})", inst.operands[0], new_new_offset, last_src_reg));
                    }
                } else {
                    panic!("Invalid memory operand in sw instruction: {}", inst);
                };

            }
            else if inst.op == "lw" {
                assert!(
                    inst.operands.len() == 2,
                    "lw instruction must have exactly two operands: {}",
                    inst
                );
                if let Some((off, reg)) = extract_register_from_mem_operand(&inst.operands[1]) {
                    let offset = off.parse::<i32>().unwrap();
                    if &reg != last_dst_reg {
                        continue;
                    }
                    let new_new_offset = offset + last_imm;
                    if fit_in_imm12(new_new_offset) {
                        block.block.instrs[i] = Instr::new(&format!("lw {}, {}({})", inst.operands[0], new_new_offset, last_src_reg));
                    }
                } else {
                    panic!("Invalid memory operand in lw instruction: {}", inst);
                };

            }
        }
    }   
}
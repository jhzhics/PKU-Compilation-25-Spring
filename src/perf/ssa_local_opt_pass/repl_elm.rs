use std::collections::HashMap;

pub fn pass(block: &mut super::ssa_form::SSABlock) {
    let mut mapping: HashMap<String, String> = HashMap::new();
    block.block.instrs.iter().for_each(|instr|
    {
        if instr.op == "mv" {
            assert!(instr.operands.len() == 2, "mv instruction must have exactly two operands: {}", instr);
            if let Some(replacement) = mapping.get(&instr.operands[1]) {
                mapping.insert(instr.operands[0].clone(), replacement.clone());
            } else {
                mapping.insert(instr.operands[0].clone(), instr.operands[1].clone());
            }
        }   
    });

    for (old, new) in mapping.iter() {
        super::ssa_pass2::block_substitute_var(block, &old, &new);
    }
    block.block.instrs.retain(|instr| {
        if instr.op == "mv" {
            assert!(instr.operands.len() == 2, "mv instruction must have exactly two operands: {}", instr);
            // Remove the mv instruction if it is a no-op
            instr.operands[0] != instr.operands[1]
        } else {
            true // Keep all other instructions
        }
    });
}
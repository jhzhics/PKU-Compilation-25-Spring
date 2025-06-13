//! In phase 1, we will convert koopa IR form into a basic SSA form using infinite registers.
//! We use some pesudo instructions, these pesudo instructions will begin with Capital letters.

use std::collections::HashMap;
use koopa::ir::{BinaryOp, FunctionData, Value, ValueKind};

use crate::perf::{phase1, riscv::Instr};

use super::riscv;

#[derive(Debug, Clone)]
pub struct Phase1Func
{
    pub blocks: HashMap<String, Phase1Block>,
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct Phase1Block
{
    pub block: riscv::Block,
    pub next: Vec<String>,
    pub prev: Vec<String>,
}

impl Phase1Block {
    fn push_instr(&mut self, instr: &str)
    {
        self.block.instrs.push(Instr::new(instr));
    }
}

#[derive(Debug, Clone)]
struct TempAllocator {
    nextvar: usize,
    nextlabel: usize,
}

impl TempAllocator {
    pub fn new() -> Self {
        TempAllocator { nextvar: 0, nextlabel: 0 }
    }

    pub fn get_temp_var(&mut self) -> String {
        let temp_var = format!("t{}", self.nextvar);
        self.nextvar += 1;
        temp_var
    }

    pub fn get_temp_label(&mut self) -> String {
        let temp_label = format!("L{}", self.nextlabel);
        self.nextlabel += 1;
        temp_label
    }
}
struct GlobalState {
    pub temp_allocator: TempAllocator,
    pub mem_to_reg: HashMap<String, String>,
}

trait Phase1Append {
    fn append(&self, func: &FunctionData, block: &mut Phase1Block);
}

pub fn phase1(func: &koopa::ir::FunctionData) -> Phase1Func {
    let mut global_state = GlobalState {
        temp_allocator: TempAllocator::new(),
        mem_to_reg: HashMap::new(),
    };
    let mut phase1_func = Phase1Func {
        blocks: HashMap::new(),
        name: func.name().to_string(),
    };


    for (bb, bb_node) in func.layout().bbs()
    {
        let block_name = func.dfg().bb(*bb).name().clone().expect("Block name should be set");
        let mut phase1_block = Phase1Block {
            block: riscv::Block::new(&block_name),
            next: Vec::new(),
            prev: Vec::new(),
        };
        bb_node.insts().keys().for_each(|instr| {
            // instr.append(func, &mut phase1_block);
        });
    }

    phase1_func
}

// impl Phase1Append for Value  {
//     fn append(&self, func: &FunctionData, block: &mut Phase1Block) {
//         let value_data = func.dfg().value(*self);
//         match value_data.kind() {
//             ValueKind::Return(ins) => {
//                 let value = ins.value();
//                 if let Some(value) = value
//                 {
//                     let value_data = func.dfg().value(value);
//                     let reg = value.get_load_reg(asm, context, func_state);
//                     asm.push_back(format!("mv a0, {}", reg));
//                     value.remove_reg(asm, context, func_state);
//                 };
//             },

//             ValueKind::Binary(ins) => {
//                 let lhs = ins.lhs().get_load_reg(asm, context, func_state);
//                 let rhs  = ins.rhs().get_load_reg(asm, context, func_state);
//                 let reg = backend::alloc_ins_reg(self);
                
//                 match ins.op() {
//                     BinaryOp::Eq =>
//                     {
//                         asm.push_back(format!("xor {}, {}, {}", reg, lhs, rhs));
//                         asm.push_back(format!("seqz {}, {}", reg, reg));
//                     },
//                     BinaryOp::Add =>
//                     {
//                         asm.push_back(format!("add {}, {}, {}", reg, lhs, rhs));
//                     }
//                     BinaryOp::Sub =>
//                     {
//                         asm.push_back(format!("sub {}, {}, {}", reg, lhs, rhs));
//                     },
//                     BinaryOp::Mul =>
//                     {
//                         asm.push_back(format!("mul {}, {}, {}", reg, lhs, rhs));
//                     },
//                     BinaryOp::Div =>
//                     {
//                         asm.push_back(format!("div {}, {}, {}", reg, lhs, rhs));
//                     },
//                     BinaryOp::Mod =>
//                     {
//                         asm.push_back(format!("rem {}, {}, {}", reg, lhs, rhs));
//                     },
//                     BinaryOp::Lt =>
//                     {
//                         asm.push_back(format!("slt {}, {}, {}", reg, lhs, rhs));
//                     },
//                     BinaryOp::Le =>
//                     {
//                         asm.push_back(format!("sgt {}, {}, {}", reg, lhs, rhs));
//                         asm.push_back(format!("xori {}, {}, 1", reg, reg));
//                     },
//                     BinaryOp::Gt =>
//                     {
//                         asm.push_back(format!("sgt {}, {}, {}", reg, lhs, rhs));
//                     },
//                     BinaryOp::Ge =>
//                     {
//                         asm.push_back(format!("slt {}, {}, {}", reg, lhs, rhs));
//                         asm.push_back(format!("xori {}, {}, 1", reg, reg));
//                     }
//                     BinaryOp::NotEq =>
//                     {
//                         if lhs == "x0"
//                         {
//                             asm.push_back(format!("snez {}, {}", reg, rhs));
//                         }
//                         else if rhs == "x0"
//                         {
//                             asm.push_back(format!("snez {}, {}", reg, lhs));
//                         }
//                         else
//                         {
//                             asm.push_back(format!("xor {}, {}, {}", reg, lhs, rhs));
//                             asm.push_back(format!("snez {}, {}", reg, reg));
//                         }

//                     },
//                     BinaryOp::And =>
//                     {
//                         asm.push_back(format!("and {}, {}, {}", reg, lhs, rhs));
//                     },
//                     BinaryOp::Or =>
//                     {
//                         asm.push_back(format!("or {}, {}, {}", reg, lhs, rhs));
//                     },
//                     other => panic!("Not implement binary op {:#?}", other)
//                 }
                
//                 let offset = func_state.get_offset(self.clone());
//                 store_word(asm, reg.as_str(), offset);

//                 ins.lhs().remove_reg(asm, context, func_state);
//                 ins.rhs().remove_reg(asm, context, func_state);
//                 backend::remove_reg(self);
//             },

//             ValueKind::Store(ins) =>
//             {
//                 let value = ins.value();
//                 let dest = ins.dest();
//                 let value_reg = value.get_load_reg(asm, context, func_state);
//                 let dest_reg = dest.get_load_reg(asm, context, func_state);
//                 asm.push_back(format!("sw {}, 0({})", value_reg, dest_reg));

//                 value.remove_reg(asm, context, func_state);
//                 dest.remove_reg(asm, context, func_state);
//             },

//             ValueKind::Load(ins) =>
//             {
//                 let reg = backend::alloc_ins_reg(self);
//                 let src_reg = ins.src().get_load_reg(asm, context, func_state);
//                 asm.push_back(format!("lw {}, 0({})", reg, src_reg));
//                 let offset = func_state.get_offset(self.clone());
//                 store_word(asm, reg.as_str(), offset);
//                 ins.src().remove_reg(asm, context, func_state);
//                 backend::remove_reg(self);
//             }

//             ValueKind::Alloc(_) => (),

//             ValueKind::Jump(ins) =>
//             {
//                 let label = backend::get_label(&ins.target()).expect("The label is not allocated");
//                 asm.push_back(format!("j {}", label));
//             },

//             ValueKind::Branch(ins) =>
//             {
//                 let temp_true_label = backend::alloc_tmp_label();
//                 let cond = ins.cond().get_load_reg(asm, context, func_state);
//                 let true_label = backend::get_label(&ins.true_bb()).expect("The label is not allocated");
//                 let false_label = backend::get_label(&ins.false_bb()).expect("The label is not allocated");
//                 asm.push_back(format!("bnez {}, {}", cond, temp_true_label));
//                 ins.cond().remove_reg(asm, context, func_state);
//                 asm.push_back(format!("j {}", false_label));
//                 asm.push_back(format!("{}:", temp_true_label));
//                 asm.push_back(format!("j {}", true_label));
//             },

//             ValueKind::Call(ins) =>
//             {
//                 let args = ins.args();
//                 for i in 0..args.len() {
//                     let arg = args[i];
//                     let reg = arg.get_load_reg(asm, context, func_state);
//                     if i < 8
//                     {
//                         asm.push_back(format!("mv a{}, {}", i, reg));
//                     }
//                     else
//                     {
//                         let offset:i32 = ((i - 8) * 4).try_into().unwrap();
//                         store_word(asm, reg.as_str(), offset);
//                     }
//                     arg.remove_reg(asm, context, func_state);
//                 }

//                 asm.push_back(format!("call {}", &context.program.func(ins.callee()).name()[1..]));
//                 if !value_data.ty().is_unit()
//                 {
//                     store_word(asm, "a0", func_state.get_offset(self.clone()));
//                 }
                    
//             },

//             ValueKind::GetElemPtr(ins) =>
//             {
//                 let src = ins.src();
//                 let src_reg = src.get_load_reg(asm, context, func_state);
//                 let reg = backend::alloc_ins_reg(self);
//                 let index = ins.index();
//                 let offset_reg = index.get_load_reg(asm, context, func_state);
//                 let elem_size = 
//                 {
//                     let ty = if src.is_global()
//                     {
//                         context.program().borrow_value(src).ty().clone()
//                     }
//                     else
//                     {
//                         context.dfg().value(src).ty().clone()
//                     };
//                     match ty.kind() {
//                         TypeKind::Pointer(t) =>
//                         {
//                             if let TypeKind::Array(t, _) = t.kind() {
//                                 t.size()
//                             } else {
//                                 panic!("The src of get_elem_ptr is not a pointer to an array")
//                             }
//                         }
//                         _ => panic!("The src of get_elem_ptr is not a pointer to an array"),
//                     }
//                 };
//                 asm.push_back(format!("li {}, {}", reg, elem_size));
//                 asm.push_back(format!("mul {}, {}, {}", offset_reg, offset_reg, reg));
//                 asm.push_back(format!("add {}, {}, {}", reg, src_reg, offset_reg));
//                 store_word(asm, reg.as_str(), func_state.get_offset(self.clone()));
                
//                 src.remove_reg(asm, context, func_state);
//                 index.remove_reg(asm, context, func_state);
//                 backend::remove_reg(self);
//             },
//             ValueKind::GetPtr(ins) =>
//             {
//                 let base = ins.src();
//                 let reg = backend::alloc_ins_reg(self);
//                 let base_reg = base.get_load_reg(asm, context, func_state);
//                 let index_reg = ins.index().get_load_reg(asm, context, func_state);
//                 let elem_size = 
//                 {
//                     let ty = if base.is_global()
//                     {
//                         context.program().borrow_value(base).ty().clone()
//                     }
//                     else
//                     {
//                         context.dfg().value(base).ty().clone()
//                     };
//                     match ty.kind() {
//                         TypeKind::Pointer(t) => t.size(),
//                         _ => panic!("The src of get_ptr is not a pointer"),
//                     }
//                 };
//                 asm.push_back(format!("li {}, {}", reg, elem_size));
//                 asm.push_back(format!("mul {}, {}, {}", reg, index_reg, reg));
//                 asm.push_back(format!("add {}, {}, {}", reg, base_reg, reg));
//                 store_word(asm, reg.as_str(), func_state.get_offset(self.clone()));
//                 ins.index().remove_reg(asm, context, func_state);
//                 base.remove_reg(asm, context, func_state);
//                 backend::remove_reg(self);
//             },
//             other => panic!("Not Implemented for value type {:#?}", other)
//         }
//     }
// }
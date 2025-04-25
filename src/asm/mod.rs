mod backend;
use koopa::{back, ir::{dfg::DataFlowGraph, *}};
use std::{collections::{ HashMap, LinkedList }, fmt::format};

pub fn compile(prog: Program) -> String {
    let mut inst_list = LinkedList::<String>::new();
    prog.generate_asm(&mut inst_list);
    inst_list.push_back("\n".to_string());
    inst_list
        .iter()
        .map(|s| s.as_str())
        .collect::<Vec<&str>>()
        .join("\n")
}
trait GenerateAsm {
    fn generate_asm(&self, asm: &mut LinkedList<String>);
}
trait GenerateIns<T> {
    fn generate_ins(&self, asm: &mut LinkedList<String>, data: &T);
}

trait InstReg<T>
{
    fn get_load_reg(&self, asm: &mut LinkedList<String>, data: &T) -> String;
    fn remove_reg(&self, asm: &mut LinkedList<String>, data: &T);
}

impl GenerateAsm for Program {
    fn generate_asm(&self, asm: &mut LinkedList<String>) {
        asm.push_back(String::from(".text"));
        asm.push_back(format!(".globl main"));
        for &func in self.func_layout() {
            let funcdata = self.func(func);
            funcdata.generate_asm(asm);
        }
    }
}


struct FunctionState<'a> {
    sp_offset: HashMap<Value, i32>,
    dfg: &'a DataFlowGraph
}

impl<'a> FunctionState<'a> {
    pub fn get_stackframe_size(&self) -> usize
    {
        self.sp_offset.len() * 4
    }
    pub fn allocate(&mut self ,value: Value)
    {
        let offset = self.get_stackframe_size();
        self.sp_offset.insert(value, offset as i32);
    }
    pub fn get_offset(&self, value: Value) -> i32
    {
        self.sp_offset.get(&value).expect("Get addr of a symbol that is not allocated").clone()
    }
    pub fn get_dfg(&self) -> &'a DataFlowGraph
    {
        self.dfg
    }
}


impl GenerateAsm for FunctionData {
    fn generate_asm(&self, asm: &mut LinkedList<String>) {
        
        let mut func_state = FunctionState {
            sp_offset: HashMap::new(),
            dfg: self.dfg()
        };

        
        let name = if self.name().len() > 1 {
            &self.name()[1..]
        } else {
            panic!("An invalid function name {}", self.name())
        };
        asm.push_back(format!("{}:", name));

        // Preprocess
        for (&bb, node) in self.layout().bbs() {
            backend::alloc_label(&bb);
            node.insts().iter().for_each(|(&value, _)|
            {
                let value_date = self.dfg().value(value);
                if !value_date.ty().is_unit()
                {
                    func_state.allocate(value);
                }
            });
        }

        // Prelogue
        let stack_size = func_state.get_stackframe_size() as i32;
        if stack_size <= 2048
        {
            asm.push_back(format!("addi sp, sp, {}", -stack_size));
        }
        else
        {
            asm.push_back(format!("li t0, {}", -stack_size));
            asm.push_back("add sp, sp, t0".to_string());   
        }

        for (&bb, node) in self.layout().bbs() {
            asm.push_back(format!("{}:", backend::get_label(&bb).expect("The label is not allocated")));
            for &inst in node.insts().keys() {
                inst.generate_ins(asm, &func_state);
            }

            asm.push_back("".to_string());
        }
    }
}

impl<'a> InstReg<FunctionState<'a>> for Value {
    fn get_load_reg(&self, asm: &mut LinkedList<String>, func_state: &FunctionState) -> String {
        let dfg = func_state.dfg;
        let value_data = dfg.value(*self);
        let default_getreg = |asm: &mut LinkedList<String>| {
            let reg = backend::alloc_ins_reg(self);
            let offset = func_state.get_offset(self.clone());
            asm.push_back(format!("lw {}, {}(sp)", reg, offset));
            reg
        };
        match value_data.kind() {
            ValueKind::Binary(_) => default_getreg(asm),
            ValueKind::Integer(ins) =>
            {
                let val = ins.value();
                if val == 0
                {
                    "x0".to_string()
                }
                else {
                    let reg = backend::alloc_ins_reg(self);
                    asm.push_back(format!("li {}, {}", reg, val.to_string()));
                    reg
                }
            },
            ValueKind::Load(_) => default_getreg(asm),
            ValueKind::Alloc(_) => default_getreg(asm),
            other => panic!("Not Implemented for value type {:#?}", other),
        }
    }
    fn remove_reg(&self, _asm: &mut LinkedList<String>, func_state: &FunctionState<'a>) {
        let dfg = func_state.dfg;
        let value_data = dfg.value(*self);
        let default_remove_reg = || {
            backend::remove_reg(self);
        };
        match value_data.kind() {
            ValueKind::Binary(_) => default_remove_reg(),
            ValueKind::Integer(ins) =>
            {
                let val = ins.value();
                if val != 0 {default_remove_reg()}
            },
            ValueKind::Load(_) => default_remove_reg(),
            ValueKind::Alloc(_) => default_remove_reg(),
            other => panic!("Not Implemented for value type {:#?}", other),
        }
    }
}

impl<'a> GenerateIns<FunctionState<'a>> for Value {
    fn generate_ins(&self, asm: &mut LinkedList<String>, func_state: &FunctionState) {
        let dfg = func_state.dfg;
        
        let value_data = dfg.value(*self);
        match value_data.kind() {
            ValueKind::Return(ins) => {
                // Epilogue
                let epilogue = |stack_size: i32| ->LinkedList<String> {
                    let mut linked_list = LinkedList::<String>::new();
                    if stack_size < 2048
                    {
                        linked_list.push_back(format!("addi sp, sp, {}", stack_size));
                    }
                    else
                    {
                        linked_list.push_back(format!("li t0, {}", stack_size));
                        linked_list.push_back("add sp, sp, t0".to_string());
                    }
                    linked_list
                };


                let value = ins.value();
                if let Some(value) = value
                {
                    let reg = value.get_load_reg(asm, func_state);
                    asm.push_back(format!("mv a0, {}", reg));
                    asm.extend(epilogue(func_state.get_stackframe_size() as i32));
                    asm.push_back("ret".to_string());
                    value.remove_reg(asm, func_state);

                }
                else
                {
                    asm.push_back("li a0, 0".to_string());
                    asm.extend(epilogue(func_state.get_stackframe_size() as i32));
                    asm.push_back("ret".to_string());

                }
            },

            ValueKind::Binary(ins) => {
                let lhs = ins.lhs().get_load_reg(asm, func_state);
                let rhs  = ins.rhs().get_load_reg(asm, func_state);
                let reg = backend::alloc_ins_reg(self);
                
                match ins.op() {
                    BinaryOp::Eq =>
                    {
                        asm.push_back(format!("xor {}, {}, {}", reg, lhs, rhs));
                        asm.push_back(format!("seqz {}, {}", reg, reg));
                    },
                    BinaryOp::Add =>
                    {
                        asm.push_back(format!("add {}, {}, {}", reg, lhs, rhs));
                    }
                    BinaryOp::Sub =>
                    {
                        asm.push_back(format!("sub {}, {}, {}", reg, lhs, rhs));
                    },
                    BinaryOp::Mul =>
                    {
                        asm.push_back(format!("mul {}, {}, {}", reg, lhs, rhs));
                    },
                    BinaryOp::Div =>
                    {
                        asm.push_back(format!("div {}, {}, {}", reg, lhs, rhs));
                    },
                    BinaryOp::Mod =>
                    {
                        asm.push_back(format!("rem {}, {}, {}", reg, lhs, rhs));
                    },
                    BinaryOp::Lt =>
                    {
                        asm.push_back(format!("slt {}, {}, {}", reg, lhs, rhs));
                    },
                    BinaryOp::Le =>
                    {
                        asm.push_back(format!("sgt {}, {}, {}", reg, lhs, rhs));
                        asm.push_back(format!("xori {}, {}, 1", reg, reg));
                    },
                    BinaryOp::Gt =>
                    {
                        asm.push_back(format!("sgt {}, {}, {}", reg, lhs, rhs));
                    },
                    BinaryOp::Ge =>
                    {
                        asm.push_back(format!("slt {}, {}, {}", reg, lhs, rhs));
                        asm.push_back(format!("xori {}, {}, 1", reg, reg));
                    }
                    BinaryOp::NotEq =>
                    {
                        if lhs == "x0"
                        {
                            asm.push_back(format!("snez {}, {}", reg, rhs));
                        }
                        else if rhs == "x0"
                        {
                            asm.push_back(format!("snez {}, {}", reg, lhs));
                        }
                        else
                        {
                            asm.push_back(format!("xor {}, {}, {}", reg, lhs, rhs));
                        }

                    },
                    BinaryOp::And =>
                    {
                        asm.push_back(format!("and {}, {}, {}", reg, lhs, rhs));
                    },
                    BinaryOp::Or =>
                    {
                        asm.push_back(format!("or {}, {}, {}", reg, lhs, rhs));
                    },
                    other => panic!("Not implement binary op {:#?}", other)
                }
                
                asm.push_back(format!("sw {}, {}(sp)", reg, func_state.get_offset(self.clone())));

                ins.lhs().remove_reg(asm, func_state);
                ins.rhs().remove_reg(asm, func_state);
                backend::remove_reg(self);
            },

            ValueKind::Store(ins) =>
            {
                let value = ins.value();
                let value_reg = value.get_load_reg(asm, func_state);
                let offset = func_state.get_offset(ins.dest());
                asm.push_back(format!("sw {}, {}(sp)", value_reg, offset));
                value.remove_reg(asm, func_state);
            },

            ValueKind::Load(ins) =>
            {
                let reg = ins.src().get_load_reg(asm, func_state);
                let offset = func_state.get_offset(self.clone());
                asm.push_back(format!("sw {}, {}(sp)", reg, offset));
                ins.src().remove_reg(asm, func_state);
            }

            ValueKind::Alloc(_) => (),

            ValueKind::Jump(ins) =>
            {
                let label = backend::get_label(&ins.target()).expect("The label is not allocated");
                asm.push_back(format!("j {}", label));
            },

            ValueKind::Branch(ins) =>
            {
                let cond = ins.cond().get_load_reg(asm, func_state);
                let true_label = backend::get_label(&ins.true_bb()).expect("The label is not allocated");
                let false_label = backend::get_label(&ins.false_bb()).expect("The label is not allocated");
                asm.push_back(format!("bnez {}, {}", cond, true_label));
                asm.push_back(format!("j {}", false_label));
            },

            other => panic!("Not Implemented for value type {:#?}", other)
        }
    }
}

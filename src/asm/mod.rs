mod backend;
use koopa::{back, ir::{dfg::DataFlowGraph, entities::ValueData, layout::Layout, *}};
use core::{alloc, panic};
use std::{cell::RefCell, clone, collections::{ HashMap, LinkedList }, fmt::format, i32::MAX, ops::{Deref, Index}, os::linux::raw::stat};

static GLOB_MAX_OFFSET: i32 = (1 << 11) - 1;
static GLOB_MIN_OFFSET: i32 = -(1 << 11);

pub fn compile(prog: Program) -> String {
    Type::set_ptr_size(4);

    let mut inst_list: LinkedList<String> = LinkedList::<String>::new();
    generate_dataseg(&mut inst_list, &prog);

    for (func, _) in prog.funcs() {
        let context = Context::new(&prog, func);
        func.generate_asm(&mut inst_list, &context);
    }

    inst_list.push_back("\n".to_string());
    inst_list
        .iter()
        .map(|s| s.as_str())
        .collect::<Vec<&str>>()
        .join("\n")
}
trait GenerateAsm {
    fn generate_asm(&self, asm: &mut LinkedList<String>, context: &Context);
}
trait GenerateIns {
    fn generate_ins(&self, asm: &mut LinkedList<String>, context: &Context, state: &State);
}

trait InstReg
{
    fn get_load_reg(&self, asm: &mut LinkedList<String>, context: &Context, state: &State) -> String;
    fn remove_reg(&self, asm: &mut LinkedList<String>, context: &Context, state: &State);
}

struct State {
    sp_offset: HashMap<Value, i32>,
    save_ra: bool,
    padding_args: i32,
    stack_size: usize,
}

struct Context<'a> {
    program: &'a Program,
    func: &'a Function,
}

impl Context<'_> {
    pub fn new<'a>(program: &'a Program, func: &'a Function) -> Context<'a> {
        Context { program, func }
    }

    pub fn program(&self) -> &Program {
        self.program
    }

    pub fn func(&self) -> &Function {
        self.func
    }

    pub fn func_data(&self) -> &FunctionData {
        self.program.func(self.func.clone())
    }

    pub fn dfg(&self) -> &DataFlowGraph {
        self.func_data().dfg()
    }

    pub fn layout(&self) -> &Layout {
        self.func_data().layout()
    }
}



fn generate_dataseg(asm: &mut LinkedList<String>, prog: &Program) {
    fn generate_initdata(asm: &mut LinkedList<String>, value_data: &ValueData, prog: &Program)
    {
        let type_size = value_data.ty().size();
        match value_data.kind() {
            ValueKind::Integer(ins) => {
                assert!(type_size == 4);
                let val = ins.value();
                asm.push_back(format!(".word {}", val));
            },
            ValueKind::ZeroInit(ins) => {
                asm.push_back(format!(".zero {}", type_size));  
            },
            ValueKind::Aggregate(ins) => {
                for value in ins.elems()
                {
                    let value_data = prog.borrow_value(*value);
                    generate_initdata(asm, &value_data, prog);
                }       
            },
            _ => panic!("Not Implemented for value type {:#?}", value_data.kind())
        }
    }

    asm.push_back(format!(".data"));
    for (value, value_data) in prog.borrow_values().iter() {

        if let ValueKind::GlobalAlloc(ins) = value_data.kind() {
            let name =  &value_data.name().as_ref().expect("The global alloc does not have a name")[1..];
            asm.push_back(format!(".globl {}", name));
            asm.push_back(format!("{}:", name));
            let init_value = prog.borrow_value(ins.init());
            generate_initdata(asm, &init_value, prog,);
            asm.push_back(String::new());
        }
    }
}


impl State {
    fn new(func_data: &FunctionData) -> State {
        let mut state = State {
            sp_offset: HashMap::new(),
            save_ra: false,
            padding_args: 0,
            stack_size: 0,
        };
        for (&bb, node) in func_data.layout().bbs() {
            node.insts().iter().for_each(|(&value, _)|
            {
                let value_data = func_data.dfg().value(value);
                if let ValueKind::Call(ins) = value_data.kind() {
                    state.save_ra = true;
                    state.padding_args = state.padding_args.max(ins.args().len() as i32 - 8);
                }
            })
        }
        state.stack_size = 4 * state.padding_args as usize;
        for (&bb, node) in func_data.layout().bbs() {
            backend::alloc_label(&bb);
            node.insts().iter().for_each(|(&value, _)|
            {
                let value_data = func_data.dfg().value(value);
                if !value_data.ty().is_unit()
                {
                    state.allocate(value);

                    if let ValueKind::Alloc(ins) = value_data.kind() {
                        let alloc_size = match value_data.ty().kind() {
                            TypeKind::Array(t, _) => t.size() as usize,
                            TypeKind::Int32 => 4,
                            TypeKind::Pointer(t) => t.size() as usize,
                            _ => panic!("Not Implemented for value type"),
                        };
                        state.stack_size += alloc_size as usize;
                    }
                    else {
                        state.stack_size += value_data.ty().size() as usize;
                    }
                    
                }
            });
        }
        if state.save_ra
        {
            state.stack_size += 4;
        }
        state.stack_size = (state.stack_size + 15) / 16 * 16;
        state
    }

    fn get_stackframe_size(&self) -> usize
    {
        self.stack_size
    }

    fn allocate(&mut self ,value: Value)
    {
        self.sp_offset.insert(value, self.stack_size as i32);
    }

    fn get_offset(&self, value: Value) -> i32
    {
        self.sp_offset.get(&value).expect("Get addr of a symbol that is not allocated").clone()
    }

    
}

fn store_word(asm: &mut LinkedList<String>, reg: &str, offset: i32) {
    if offset >= GLOB_MIN_OFFSET && offset <= GLOB_MAX_OFFSET
    {
        asm.push_back(format!("sw {}, {}(sp)", reg, offset));
    }
    else
    {

        asm.push_back(format!("li t0, {}", offset));
        asm.push_back(format!("add t0, t0, sp"));
        asm.push_back(format!("sw {}, 0(t0)", reg));
    }
}

fn load_word(asm: &mut LinkedList<String>, reg: &str, offset: i32) {
    if offset >= GLOB_MIN_OFFSET && offset <= GLOB_MAX_OFFSET
    {
        asm.push_back(format!("lw {}, {}(sp)", reg, offset));
    }
    else
    {
        asm.push_back(format!("li t0, {}", offset));
        asm.push_back(format!("add t0, t0, sp"));
        asm.push_back(format!("lw {}, 0(t0)", reg));
    }
}

impl GenerateAsm for Function {
    fn generate_asm(&self, asm: &mut LinkedList<String>, context: &Context) {
        if context.func_data().layout().entry_bb().is_none()
        {
            return; // This is a function declaration
        }

        let func_state = State::new(context.func_data());

        let name = if context.func_data().name().len() > 1 {
            &context.func_data().name()[1..]
        } else {
            panic!("An invalid function name {}", context.func_data().name())
        };
        asm.push_back(format!(".text"));
        asm.push_back(format!(".globl {}", name));
        asm.push_back(format!("{}:", name));

        // Prelogue
        let stack_size = func_state.get_stackframe_size() as i32;
        if -stack_size >= GLOB_MIN_OFFSET
        {
            asm.push_back(format!("addi sp, sp, {}", -stack_size));
        }
        else
        {
            asm.push_back(format!("li t0, {}", -stack_size));
            asm.push_back("add sp, sp, t0".to_string());   
        }

        if func_state.save_ra
        {
            store_word(asm, "ra", stack_size - 4);
        }

        for (&bb, node) in context.layout().bbs() {
            asm.push_back(format!("{}:", backend::get_label(&bb).expect("The label is not allocated")));
            for &inst in node.insts().keys() {
                inst.generate_ins(asm, context, &func_state);
            }

            asm.push_back("".to_string());
        }
    }
}

impl InstReg for Value {
    fn get_load_reg(&self, asm: &mut LinkedList<String>, context: &Context, func_state: &State) -> String {
        let global_value_data;
        let value_kind = if self.is_global()
        {
            global_value_data = context.program().borrow_value(*self);
            global_value_data.kind()
        }
        else
        {
            context.dfg().value(*self).kind()
        };

        let default_getreg = |asm: &mut LinkedList<String>| {

            let reg = backend::alloc_ins_reg(self);
            let offset = func_state.get_offset(self.clone());

            load_word(asm, reg.as_str(), offset);
            reg
        };
        match value_kind {
            ValueKind::Binary(_) => default_getreg(asm),
            ValueKind::Integer(ins) =>
            {
                let val = ins.value();
                let reg = backend::alloc_ins_reg(self);
                asm.push_back(format!("li {}, {}", reg, val.to_string()));
                reg

            },
            ValueKind::Load(_) => default_getreg(asm),
            ValueKind::Alloc(_) => 
            {
                let reg = backend::alloc_ins_reg(self);
                let offset = func_state.get_offset(self.clone());
                asm.push_back(format!("li {}, {}", reg, offset));
                asm.push_back(format!("add {}, {}, sp", reg, reg));
                reg
            },
            ValueKind::FuncArgRef(ins) =>
            {
                if ins.index() < 8
                {
                    format!("a{}", ins.index())
                }
                else
                {
                    let reg = backend::alloc_ins_reg(self);
                    let mut offset:i32 = ((ins.index() - 8) * 4).try_into().unwrap();
                    offset += func_state.get_stackframe_size() as i32;
                    load_word(asm, reg.as_str(), offset);
                    reg
                }
            },
            ValueKind::Call(_) => 
            {
                assert!(!context.dfg().value(*self).ty().is_unit());
                default_getreg(asm)
            },
            ValueKind::GlobalAlloc(ins) =>
            {
                let value_data = context.program().borrow_value(*self);
                let name = &value_data.name().as_ref().expect("The global alloc does not have a name")[1..];
                let reg = backend::alloc_ins_reg(self);
                asm.push_back(format!("la {}, {}", reg, name));
                reg
            },
            ValueKind::GetElemPtr(ins) => default_getreg(asm),
            ValueKind::GetPtr(ins) => default_getreg(asm),
            other => panic!("Not Implemented for value type {:#?}", other),
        }
    }
    fn remove_reg(&self, _asm: &mut LinkedList<String>, context: &Context, func_state: &State) {
        let global_value_data;
        let value_kind = if self.is_global()
        {
            global_value_data = context.program().borrow_value(*self);
            global_value_data.kind()
        }
        else
        {
            context.dfg().value(*self).kind()
        };
        let default_remove_reg = || {
            backend::remove_reg(self);
        };
        match value_kind {
            ValueKind::Binary(_) => default_remove_reg(),
            ValueKind::Integer(ins) =>
            {
                default_remove_reg();
            },
            ValueKind::Load(_) => default_remove_reg(),
            ValueKind::Alloc(_) => default_remove_reg(),
            ValueKind::FuncArgRef(ins) =>
            {
                if ins.index() >= 8
                {
                    default_remove_reg();
                }
            },
            ValueKind::Call(_) => 
            {
                assert!(!context.dfg().value(*self).ty().is_unit());
                default_remove_reg();
            },
            ValueKind::GlobalAlloc(ins) => default_remove_reg(),
            ValueKind::GetElemPtr(ins) => default_remove_reg(),
            ValueKind::GetPtr(ins) => default_remove_reg(),
            other => panic!("Not Implemented for value type {:#?}", other),
        }
    }
}

impl GenerateIns for Value {
    fn generate_ins(&self, asm: &mut LinkedList<String>, context: &Context, func_state: &State) {

        let value_data = context.dfg().value(*self);
        match value_data.kind() {
            ValueKind::Return(ins) => {
                // Epilogue
                let epilogue = |func_state: &State| ->LinkedList<String> {
                    let mut linked_list = LinkedList::<String>::new();
                    let stack_size = func_state.get_stackframe_size() as i32;
                    if func_state.save_ra
                    {
                        load_word(&mut linked_list, "ra", stack_size - 4);
                    }

                    if stack_size <= GLOB_MAX_OFFSET
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
                    let reg = value.get_load_reg(asm, context, func_state);
                    asm.push_back(format!("mv a0, {}", reg));
                    value.remove_reg(asm, context, func_state);
                }
                asm.extend(epilogue(func_state));
                asm.push_back("ret".to_string());
            },

            ValueKind::Binary(ins) => {
                let lhs = ins.lhs().get_load_reg(asm, context, func_state);
                let rhs  = ins.rhs().get_load_reg(asm, context, func_state);
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
                            asm.push_back(format!("snez {}, {}", reg, reg));
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
                
                let offset = func_state.get_offset(self.clone());
                store_word(asm, reg.as_str(), offset);

                ins.lhs().remove_reg(asm, context, func_state);
                ins.rhs().remove_reg(asm, context, func_state);
                backend::remove_reg(self);
            },

            ValueKind::Store(ins) =>
            {
                let value = ins.value();
                let dest = ins.dest();
                let value_reg = value.get_load_reg(asm, context, func_state);
                let dest_reg = dest.get_load_reg(asm, context, func_state);
                asm.push_back(format!("sw {}, 0({})", value_reg, dest_reg));

                value.remove_reg(asm, context, func_state);
                dest.remove_reg(asm, context, func_state);
            },

            ValueKind::Load(ins) =>
            {
                let reg = backend::alloc_ins_reg(self);
                let src_reg = ins.src().get_load_reg(asm, context, func_state);
                asm.push_back(format!("lw {}, 0({})", reg, src_reg));
                let offset = func_state.get_offset(self.clone());
                store_word(asm, reg.as_str(), offset);
                ins.src().remove_reg(asm, context, func_state);
                backend::remove_reg(self);
            }

            ValueKind::Alloc(_) => (),

            ValueKind::Jump(ins) =>
            {
                let label = backend::get_label(&ins.target()).expect("The label is not allocated");
                asm.push_back(format!("j {}", label));
            },

            ValueKind::Branch(ins) =>
            {
                let temp_true_label = backend::alloc_tmp_label();
                let cond = ins.cond().get_load_reg(asm, context, func_state);
                let true_label = backend::get_label(&ins.true_bb()).expect("The label is not allocated");
                let false_label = backend::get_label(&ins.false_bb()).expect("The label is not allocated");
                asm.push_back(format!("bnez {}, {}", cond, temp_true_label));
                ins.cond().remove_reg(asm, context, func_state);
                asm.push_back(format!("j {}", false_label));
                asm.push_back(format!("{}:", temp_true_label));
                asm.push_back(format!("j {}", true_label));
            },

            ValueKind::Call(ins) =>
            {
                let args = ins.args();
                for i in 0..args.len() {
                    let arg = args[i];
                    let reg = arg.get_load_reg(asm, context, func_state);
                    if i < 8
                    {
                        asm.push_back(format!("mv a{}, {}", i, reg));
                    }
                    else
                    {
                        let offset:i32 = ((i - 8) * 4).try_into().unwrap();
                        store_word(asm, reg.as_str(), offset);
                    }
                    arg.remove_reg(asm, context, func_state);
                }

                asm.push_back(format!("call {}", &context.program.func(ins.callee()).name()[1..]));
                if !value_data.ty().is_unit()
                {
                    store_word(asm, "a0", func_state.get_offset(self.clone()));
                }
                    
            },

            ValueKind::GetElemPtr(ins) =>
            {
                let src = ins.src();
                let src_reg = src.get_load_reg(asm, context, func_state);
                let reg = backend::alloc_ins_reg(self);
                let index = ins.index();
                let offset_reg = index.get_load_reg(asm, context, func_state);
                let elem_size = 
                {
                    let ty = if src.is_global()
                    {
                        context.program().borrow_value(src).ty().clone()
                    }
                    else
                    {
                        context.dfg().value(src).ty().clone()
                    };
                    match ty.kind() {
                        TypeKind::Pointer(t) =>
                        {
                            if let TypeKind::Array(t, _) = t.kind() {
                                t.size()
                            } else {
                                panic!("The src of get_elem_ptr is not a pointer to an array")
                            }
                        }
                        _ => panic!("The src of get_elem_ptr is not a pointer to an array"),
                    }
                };
                asm.push_back(format!("li {}, {}", reg, elem_size));
                asm.push_back(format!("mul {}, {}, {}", offset_reg, offset_reg, reg));
                asm.push_back(format!("add {}, {}, {}", reg, src_reg, offset_reg));
                store_word(asm, reg.as_str(), func_state.get_offset(self.clone()));
                
                src.remove_reg(asm, context, func_state);
                index.remove_reg(asm, context, func_state);
                backend::remove_reg(self);
            },
            ValueKind::GetPtr(ins) =>
            {
                let base = ins.src();
                let reg = backend::alloc_ins_reg(self);
                let base_reg = base.get_load_reg(asm, context, func_state);
                let index_reg = ins.index().get_load_reg(asm, context, func_state);
                let elem_size = 
                {
                    let ty = if base.is_global()
                    {
                        context.program().borrow_value(base).ty().clone()
                    }
                    else
                    {
                        context.dfg().value(base).ty().clone()
                    };
                    match ty.kind() {
                        TypeKind::Pointer(t) => t.size(),
                        _ => panic!("The src of get_ptr is not a pointer"),
                    }
                };
                asm.push_back(format!("li {}, {}", reg, elem_size));
                asm.push_back(format!("mul {}, {}, {}", reg, index_reg, reg));
                asm.push_back(format!("add {}, {}, {}", reg, base_reg, reg));
                store_word(asm, reg.as_str(), func_state.get_offset(self.clone()));
                ins.index().remove_reg(asm, context, func_state);
                base.remove_reg(asm, context, func_state);
                backend::remove_reg(self);
            },
            other => panic!("Not Implemented for value type {:#?}", other)
        }
    }
}

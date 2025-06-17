//! In phase 1, we will convert koopa IR form into a basic SSA form (not exactly SSA) using infinite registers.
//! We use some pesudo instructions, these pesudo instructions will begin with Capital letters.

use koopa::ir::{BinaryOp, FunctionData, TypeKind, Value, ValueKind};
use std::collections::{HashMap, HashSet, LinkedList};

use crate::perf::riscv::Instr;
use super::riscv;

#[derive(Debug, Clone)]
pub struct SSAPass1Func {
    pub blocks: HashMap<String, SSAPass1Block>,
    pub name: String,
    pub entry: Option<String>,
    pub args: Vec<String>,
}
impl SSAPass1Func {
    #[allow(dead_code)]
    pub fn dump(&self) -> LinkedList<String> {
        let mut inst_list: LinkedList<String> = LinkedList::new();
        inst_list.push_back(format!("{}({}):", self.name, self.args.join(", ")));
        if let Some(entry) = &self.entry {
            let engtry_block = self
                .blocks
                .get(entry)
                .expect("Entry block should be set for a function");
            inst_list.extend(engtry_block.block.dump());
            inst_list.push_back("".to_string());

            self.blocks.iter().for_each(|(name, block)| {
                if name == entry {
                    return; // Skip the entry block, it has been added already
                }
                inst_list.extend(block.block.dump());
                inst_list.push_back("".to_string());
            });
        }
        inst_list
    }
}

#[derive(Debug, Clone)]
pub struct SSAPass1Block {
    pub block: riscv::Block,
    pub next: Vec<String>,
}

impl SSAPass1Block {
    fn push_instr(&mut self, instr: &str) {
        self.block.instrs.push(Instr::new(instr));
    }
}

#[derive(Debug, Clone)]
struct TempAllocator {
    nextvar: usize,
    _nextlabel: usize,
}

impl TempAllocator {
    pub fn new() -> Self {
        TempAllocator {
            nextvar: 0,
            _nextlabel: 0,
        }
    }

    pub fn get_temp_var(&mut self) -> String {
        let temp_var = format!("t{}", self.nextvar);
        self.nextvar += 1;
        temp_var
    }
}

#[derive(Debug, Clone)]
struct Pass1State {
    pub temp_allocator: TempAllocator,
    pub ssa_mem_reg: HashMap<String, usize>,
    pub global_vars: HashSet<String>,
}

impl Pass1State {
    pub fn get_var(&mut self, var: &str, is_write: bool) -> String {
        if let Some(idx) = self.ssa_mem_reg.get_mut(var) {
            if is_write {
                *idx += 1;
            }
            format!("k{}", &var[1..])
        } else {
            format!("k{}", &var[1..])
        }
    }
    pub fn enter_new_block(&mut self) {
        self.ssa_mem_reg.iter_mut().for_each(|(_, idx)| {
            *idx += 1; // Increment the index for all variables
        });
    }
}

trait Pass1Append {
    fn append(&self, context: &Pass1Context, block: &mut SSAPass1Block, state: &mut Pass1State);
}

struct Pass1Context<'a> {
    program: &'a koopa::ir::Program,
    func: &'a koopa::ir::Function,
}

impl Pass1Context<'_> {
    pub fn new<'a>(
        program: &'a koopa::ir::Program,
        func: &'a koopa::ir::Function,
    ) -> Pass1Context<'a> {
        Pass1Context { program, func }
    }

    pub fn program(&self) -> &koopa::ir::Program {
        self.program
    }

    #[allow(dead_code)]
    pub fn func(&self) -> &koopa::ir::Function {
        self.func
    }

    pub fn func_data(&self) -> &FunctionData {
        self.program.func(self.func.clone())
    }

    pub fn dfg(&self) -> &koopa::ir::dfg::DataFlowGraph {
        self.func_data().dfg()
    }

    pub fn layout(&self) -> &koopa::ir::layout::Layout {
        self.func_data().layout()
    }
}

pub fn pass(prog: &koopa::ir::Program, func: koopa::ir::Function) -> SSAPass1Func {
    let context = Pass1Context::new(prog, &func);
    let mut global_state = Pass1State {
        temp_allocator: TempAllocator::new(),
        ssa_mem_reg: HashMap::new(),
        global_vars: HashSet::new(),
    };

    let mut pass1_func = SSAPass1Func {
        blocks: HashMap::new(),
        name: context.func_data().name().to_string(),
        entry: None,
        args: Vec::new(),
    };

    if let Some(entry) = context.layout().entry_bb() {
        pass1_func.entry = Some(format!(
            "L{}",
            &context.dfg().bb(entry).name().as_ref().unwrap()[1..]
        ));
    } else {
        assert!(context.layout().bbs().len() == 0); // Declaration
        return pass1_func;
    }

    pass1_func.args = context
        .func_data()
        .params()
        .iter()
        .map(|arg| {
            let arg_name = context
                .dfg()
                .value(*arg)
                .name()
                .as_ref()
                .expect("Argument should have name");
            format!("k{}", &arg_name[1..])
        })
        .collect();

    for (bb, bb_node) in context.layout().bbs() {
        let block_name = &context
            .dfg()
            .bb(*bb)
            .name()
            .as_ref()
            .expect("Block name should be set")[1..];
        let mut pass1_block = SSAPass1Block {
            block: riscv::Block::new(format!("L{}", block_name.to_string())),
            next: Vec::new(),
        };
        bb_node.insts().keys().for_each(|instr| {
            instr.append(&context, &mut pass1_block, &mut global_state);
        });
        pass1_func
            .blocks
            .insert(pass1_block.block.name.clone(), pass1_block);
        global_state.enter_new_block();
    }

    let entry_block = pass1_func
        .blocks
        .get_mut(
            &pass1_func
                .entry
                .as_ref()
                .expect("Entry block should be set")
                .clone(),
        )
        .expect("Entry block should exist");

    let global_inits = global_state
        .global_vars
        .iter()
        .map(|var| format!("la g_{}, {}", &var[1..], var))
        .collect::<Vec<String>>();

    entry_block.block.instrs = global_inits
        .into_iter()
        .map(|instr| Instr::new(&instr))
        .chain(entry_block.block.instrs.clone())
        .collect();

    pass1_func
}

fn load_integer(
    value: &Value,
    block: &mut SSAPass1Block,
    context: &Pass1Context,
    state: &mut Pass1State,
) -> String {
    let value_data = context.dfg().value(*value);
    if let ValueKind::Integer(val) = value_data.kind() {
        let temp_var = state.temp_allocator.get_temp_var();
        block.push_instr(&format!("li {}, {}", temp_var, val.value()));
        temp_var
    } else {
        panic!("Expected an integer value, found: {:?}", value_data.kind());
    }
}

impl Pass1Append for Value {
    fn append(&self, context: &Pass1Context, block: &mut SSAPass1Block, state: &mut Pass1State) {
        let mut get_value_reg = |value: Value, is_write: bool| -> String {
            if value.is_global() {
                let value_data = context.program().borrow_value(value);
                assert!(value_data.kind().is_global_alloc());
                let name = value_data
                    .name()
                    .as_ref()
                    .expect("Global value should have a name");
                assert!(!is_write, "Global ptr should not be written to");
                state.global_vars.insert(name.clone());
                format!("g_{}", &name[1..])
            } else if let Some(name) = context.dfg().value(value).name().as_ref() {
                state.get_var(name, is_write)
            } else {
                assert!(!is_write);
                load_integer(&value, block, context, state)
            }
        };

        let value_data = context.dfg().value(*self);
        match value_data.kind() {
            ValueKind::Return(ins) => {
                let value = ins.value();
                if let Some(value) = value {
                    let ret_reg = get_value_reg(value, false);
                    block.push_instr(&format!("Ret {}", ret_reg));
                } else {
                    block.push_instr("Ret");
                }
            }

            ValueKind::Binary(ins) => {
                let lhs = get_value_reg(ins.lhs(), false);
                let rhs = get_value_reg(ins.rhs(), false);
                let reg = get_value_reg(*self, true);
                match ins.op() {
                    BinaryOp::Eq => {
                        let tmp_reg = state.temp_allocator.get_temp_var();
                        block.push_instr(&format!("xor {}, {}, {}", tmp_reg, lhs, rhs));
                        block.push_instr(&format!("seqz {}, {}", reg, tmp_reg));
                    }
                    BinaryOp::Add => {
                        block.push_instr(&format!("add {}, {}, {}", reg, lhs, rhs));
                    }
                    BinaryOp::Sub => {
                        block.push_instr(&format!("sub {}, {}, {}", reg, lhs, rhs));
                    }
                    BinaryOp::Mul => {
                        block.push_instr(&format!("mul {}, {}, {}", reg, lhs, rhs));
                    }
                    BinaryOp::Div => {
                        block.push_instr(&format!("div {}, {}, {}", reg, lhs, rhs));
                    }
                    BinaryOp::Mod => {
                        block.push_instr(&format!("rem {}, {}, {}", reg, lhs, rhs));
                    }
                    BinaryOp::Lt => {
                        block.push_instr(&format!("slt {}, {}, {}", reg, lhs, rhs));
                    }
                    BinaryOp::Le => {
                        let tmp_reg = state.temp_allocator.get_temp_var();
                        block.push_instr(&format!("sgt {}, {}, {}", tmp_reg, lhs, rhs));
                        block.push_instr(&format!("xori {}, {}, 1", reg, tmp_reg));
                    }
                    BinaryOp::Gt => {
                        block.push_instr(&format!("sgt {}, {}, {}", reg, lhs, rhs));
                    }
                    BinaryOp::Ge => {
                        let tmp_reg = state.temp_allocator.get_temp_var();
                        block.push_instr(&format!("slt {}, {}, {}", tmp_reg, lhs, rhs));
                        block.push_instr(&format!("xori {}, {}, 1", reg, tmp_reg));
                    }
                    BinaryOp::NotEq => {
                        let tmp_reg = state.temp_allocator.get_temp_var();
                        block.push_instr(&format!("xor {}, {}, {}", tmp_reg, lhs, rhs));
                        block.push_instr(&format!("snez {}, {}", reg, tmp_reg));
                    }
                    BinaryOp::And => {
                        block.push_instr(&format!("and {}, {}, {}", reg, lhs, rhs));
                    }
                    BinaryOp::Or => {
                        block.push_instr(&format!("or {}, {}, {}", reg, lhs, rhs));
                    }
                    other => panic!("Not implement binary op {:#?}", other),
                }
            }

            ValueKind::Store(ins) => {
                let value_reg = get_value_reg(ins.value(), false);
                if ins.dest().is_global() {
                    if let TypeKind::Pointer(t) =
                        context.program().borrow_value(ins.dest()).ty().kind()
                    {
                        assert!(
                            t.size() == 4,
                            "Store to a pointer that is not 4 bytes is not supported in this pass"
                        );
                    } else {
                        panic!("Error! Store to a non-pointer type");
                    }
                } else if let TypeKind::Pointer(t) = context.dfg().value(ins.dest()).ty().kind() {
                    assert!(
                        t.size() == 4,
                        "Store to a pointer that is not 4 bytes is not supported in this pass"
                    );
                } else {
                    panic!("Error! Store to a non-pointer type");
                }

                if ins.dest().is_global() {
                    let global_ptr_reg = get_value_reg(ins.dest(), false);
                    block.push_instr(&format!("sw {}, 0({})", value_reg, global_ptr_reg));
                } else if let ValueKind::Alloc(_) = context.dfg().value(ins.dest()).kind() {
                    let dest_reg = get_value_reg(ins.dest(), true);
                    block.push_instr(&format!("mv {}, {}", dest_reg, value_reg));
                } else {
                    let dest_reg = get_value_reg(ins.dest(), false);
                    block.push_instr(&format!("sw {}, 0({})", value_reg, dest_reg));
                }
            }

            ValueKind::Load(ins) => {
                let reg = get_value_reg(*self, true);
                if ins.src().is_global() {
                    if let TypeKind::Pointer(t) =
                        context.program().borrow_value(ins.src()).ty().kind()
                    {
                        assert!(
                            t.size() == 4,
                            "Store to a pointer that is not 4 bytes is not supported in this pass"
                        );
                    } else {
                        panic!("Error! Store to a non-pointer type");
                    }
                } else if let TypeKind::Pointer(t) = context.dfg().value(ins.src()).ty().kind() {
                    assert!(
                        t.size() == 4,
                        "Store to a pointer that is not 4 bytes is not supported in this pass"
                    );
                } else {
                    panic!("Error! Store to a non-pointer type");
                }

                if ins.src().is_global() {
                    let global_ptr_reg = get_value_reg(ins.src(), false);
                    block.push_instr(&format!("lw {}, 0({})", reg, global_ptr_reg));
                } else if let ValueKind::Alloc(_) = context.dfg().value(ins.src()).kind() {
                    let src_reg = get_value_reg(ins.src(), false);
                    block.push_instr(&format!("mv {}, {}", reg, src_reg));
                } else {
                    let src_reg = get_value_reg(ins.src(), false);
                    block.push_instr(&format!("lw {}, 0({})", reg, src_reg));
                }
            }

            ValueKind::Alloc(_ins) => {
                if let TypeKind::Pointer(t) = value_data.ty().kind() {
                    if t.size() == 4 {
                        let value_name = value_data
                            .name()
                            .as_ref()
                            .expect("Value should have a name");
                        state.ssa_mem_reg.insert(value_name.clone(), 0);
                    } else {
                        let reg = get_value_reg(*self, true);
                        block.push_instr(&format!("Alloc {}, {}", reg, t.size()));
                    }
                } else {
                    panic!("Error!");
                }
            }

            ValueKind::Jump(ins) => {
                let target = ins.target();
                let target_name = &context.dfg().bb(target).name().as_ref().unwrap()[1..];
                block.push_instr(&format!("j L{}", target_name));
                assert!(
                    block.next.is_empty(),
                    "Jump instruction should not have next blocks"
                );
                block.next.push(format!("L{}", target_name).to_string());
                assert!(
                    format!("L{}", target_name) != block.block.name,
                    "Jump instruction should not jump to itself"
                );
            }

            ValueKind::Branch(ins) => {
                let cond_reg = get_value_reg(ins.cond(), false);
                let true_target = ins.true_bb();
                let false_target = ins.false_bb();
                let true_target_name = &context.dfg().bb(true_target).name().as_ref().unwrap()[1..];
                let false_target_name =
                    &context.dfg().bb(false_target).name().as_ref().unwrap()[1..];
                block.push_instr(&format!(
                    "Br {}, L{}, L{}",
                    cond_reg, true_target_name, false_target_name
                ));
                assert!(
                    block.next.is_empty(),
                    "Branch instruction should not have next blocks"
                );
                block
                    .next
                    .push(format!("L{}", true_target_name).to_string());
                block
                    .next
                    .push(format!("L{}", false_target_name).to_string());
                assert!(
                    format!("L{}", true_target_name) != block.block.name,
                    "Branch instruction should not branch to itself"
                );
                assert!(
                    format!("L{}", false_target_name) != block.block.name,
                    "Branch instruction should not branch to itself"
                );
            }

            ValueKind::Call(ins) => {
                let args = ins.args();
                let arg_regs: Vec<String> =
                    args.iter().map(|arg| get_value_reg(*arg, false)).collect();

                let func = ins.callee();

                let func_name = context.program.func(func).name();
                if value_data.ty().is_unit() {
                    if arg_regs.is_empty() {
                        block.push_instr(&format!("Call_1 {}", func_name));
                    } else {
                        block.push_instr(&format!("Call_1 {}, {}", func_name, arg_regs.join(", ")));
                    }
                } else {
                    let ret_reg = get_value_reg(*self, true);
                    if arg_regs.is_empty() {
                        block.push_instr(&format!("Call_2 {}, {}", ret_reg, func_name));
                    } else {
                        block.push_instr(&format!(
                            "Call_2 {}, {}, {}",
                            ret_reg,
                            func_name,
                            arg_regs.join(", ")
                        ));
                    }
                }
            }
            ValueKind::GetElemPtr(ins) => {
                let is_integer_zero = |value: &Value| {
                    if value.is_global() {
                        if let ValueKind::Integer(int_val) =
                            context.program().borrow_value(*value).kind()
                        {
                            int_val.value() == 0
                        } else {
                            false
                        }
                    } else {
                        if let ValueKind::Integer(int_val) = context.dfg().value(*value).kind() {
                            int_val.value() == 0
                        } else {
                            false
                        }
                    }
                };

                if is_integer_zero(&ins.index()) {
                    // If the index is zero, we can just return the src pointer
                    let src_reg = get_value_reg(ins.src(), false);
                    let reg = get_value_reg(*self, true);
                    block.push_instr(&format!("mv {}, {}", reg, src_reg));
                    return;
                }

                let src = ins.src();
                let src_reg = get_value_reg(src, false);
                let index_reg = get_value_reg(ins.index(), false);
                let reg = get_value_reg(*self, true);
                let elem_size = {
                    let ty = if src.is_global() {
                        context.program().borrow_value(src).ty().clone()
                    } else {
                        context.dfg().value(src).ty().clone()
                    };
                    match ty.kind() {
                        TypeKind::Pointer(t) => {
                            if let TypeKind::Array(t, _) = t.kind() {
                                t.size()
                            } else {
                                panic!("The src of get_elem_ptr is not a pointer to an array")
                            }
                        }
                        _ => panic!("The src of get_elem_ptr is not a pointer to an array"),
                    }
                };
                let tmp_reg1 = state.temp_allocator.get_temp_var();
                let tmp_reg2 = state.temp_allocator.get_temp_var();
                block.push_instr(&format!("li {}, {}", tmp_reg1, elem_size));
                block.push_instr(&format!("mul {}, {}, {}", tmp_reg2, index_reg, tmp_reg1));
                block.push_instr(&format!("add {}, {}, {}", reg, src_reg, tmp_reg2));
            }
            ValueKind::GetPtr(ins) => {
                let is_integer_zero = |value: &Value| {
                    if value.is_global() {
                        if let ValueKind::Integer(int_val) =
                            context.program().borrow_value(*value).kind()
                        {
                            int_val.value() == 0
                        } else {
                            false
                        }
                    } else {
                        if let ValueKind::Integer(int_val) = context.dfg().value(*value).kind() {
                            int_val.value() == 0
                        } else {
                            false
                        }
                    }
                };

                if is_integer_zero(&ins.index()) {
                    // If the index is zero, we can just return the src pointer
                    let src_reg = get_value_reg(ins.src(), false);
                    let reg = get_value_reg(*self, true);
                    block.push_instr(&format!("mv {}, {}", reg, src_reg));
                    return;
                }

                let base = ins.src();
                let base_reg = get_value_reg(base, false);
                let index_reg = get_value_reg(ins.index(), false);
                let reg = get_value_reg(*self, true);
                let elem_size = {
                    let ty = if base.is_global() {
                        context.program().borrow_value(base).ty().clone()
                    } else {
                        context.dfg().value(base).ty().clone()
                    };
                    match ty.kind() {
                        TypeKind::Pointer(t) => t.size(),
                        _ => panic!("The src of get_ptr is not a pointer"),
                    }
                };
                let tmp_reg1 = state.temp_allocator.get_temp_var();
                let tmp_reg2 = state.temp_allocator.get_temp_var();
                block.push_instr(&format!("li {}, {}", tmp_reg1, elem_size));
                block.push_instr(&format!("mul {}, {}, {}", tmp_reg2, index_reg, tmp_reg1));
                block.push_instr(&format!("add {}, {}, {}", reg, base_reg, tmp_reg2));
            }
            other => panic!("Not Implemented for value type {:#?}", other),
        }
    }
}

mod symtable;
use koopa::back::koopa;
use ::koopa::ir::builder::{*};
use ::koopa::ir::values::Aggregate;
use ::koopa::ir::{BasicBlock, TypeKind, Value, ValueKind};
use symtable::{SymValue, VarSymbol};
use super::ast::{*};

use std::collections::LinkedList;
use std::mem;
use std::ops::Deref;
use std::os::linux::raw::stat;
use ::koopa::ir as koopa_ir;

type ValueList = LinkedList<koopa_ir::Value>;

#[derive(Debug, Clone, Copy)]
struct IRContext
{
    pub next_bb: Option<BasicBlock>,
    //The basic block outside the loop(if any)
    pub out_bb: Option<koopa_ir::BasicBlock>,
    //The cond basic block for loop(if any)
    pub cond_bb: Option<koopa_ir::BasicBlock>
}

struct IRState {
    current_bb: Option<BasicBlock>,
    pub instrs_list: ValueList,
    next_idx: usize,
}

impl Default for IRContext {
    fn default() -> Self {
        IRContext { next_bb: None, out_bb: None, cond_bb: None }
    }
}

impl Default for IRState {
    fn default() -> Self {
        IRState {
            current_bb: None,
            instrs_list: ValueList::new(),
            next_idx: 0,
        }
    }
}

impl IRState
{
    fn set_current_bb(&mut self, bb: BasicBlock, func_data: &mut koopa_ir::FunctionData) {
        self.finalize(func_data);
        self.current_bb = Some(bb);
    }

    fn finalize(&mut self, func_data: &mut koopa_ir::FunctionData) 
    {
        if self.instrs_list.is_empty() {
            return;
        }
        let cloned_lists = self.instrs_list.clone();
        cloned_lists.into_iter().for_each(|instr| {
            func_data.dfg_mut().set_value_name(instr, self.get_idx());
        });
        func_data.layout_mut().bb_mut(self.current_bb.expect("Current_bb must be set")).
        insts_mut().extend(mem::take(&mut self.instrs_list));
    }

    fn get_idx(&mut self) -> Option<String> {
        let idx = self.next_idx;
        self.next_idx += 1;
        Some(format!("%{}", idx))
    }
}

fn add_libfunc_declarations(program: &mut koopa_ir::Program) {
    // decl @getint(): i32
    let func = program.new_func(koopa_ir::FunctionData::new(
        "@getint".to_string(),
        vec![],
        koopa_ir::Type::get_i32(),
    ));
    symtable::insert("getint", SymValue::Function(func));
    // decl @getch(): i32
    let func = program.new_func(koopa_ir::FunctionData::new(
        "@getch".to_string(),
        vec![],
        koopa_ir::Type::get_i32(),
    ));
    symtable::insert("getch", SymValue::Function(func));
    // decl @getarray(*i32): i32
    let func = program.new_func(koopa_ir::FunctionData::new(
        "@getarray".to_string(),
        vec![koopa_ir::Type::get_pointer(
            koopa_ir::Type::get_i32(),
        )],
        koopa_ir::Type::get_i32(),
    ));
    symtable::insert("getarray", SymValue::Function(func));
    // decl @putint(i32)
    let func = program.new_func(koopa_ir::FunctionData::new(
        "@putint".to_string(),
        vec![koopa_ir::Type::get_i32()],
        koopa_ir::Type::get_unit(),
    ));
    symtable::insert("putint", SymValue::Function(func));
    // decl @putch(i32)
    let func = program.new_func(koopa_ir::FunctionData::new(
        "@putch".to_string(),
        vec![koopa_ir::Type::get_i32()],
        koopa_ir::Type::get_unit(),
    ));
    symtable::insert("putch", SymValue::Function(func));
    // decl @putarray(i32, *i32)
    let func = program.new_func(koopa_ir::FunctionData::new(
        "@putarray".to_string(),
        vec![
            koopa_ir::Type::get_i32(),
            koopa_ir::Type::get_pointer(koopa_ir::Type::get_i32()),
        ],
        koopa_ir::Type::get_unit(),
    ));
    symtable::insert("putarray", SymValue::Function(func));
    // decl @starttime()
    let func = program.new_func(koopa_ir::FunctionData::new(
        "@starttime".to_string(),
        vec![],
        koopa_ir::Type::get_unit(),
    ));
    symtable::insert("starttime", SymValue::Function(func));
    // decl @stoptime()
    let func = program.new_func(koopa_ir::FunctionData::new(
        "@stoptime".to_string(),
        vec![],
        koopa_ir::Type::get_unit(),
    ));
    symtable::insert("stoptime", SymValue::Function(func));
}


pub fn build_koopa(ast: CompUnit) -> koopa_ir::Program {
    let mut koopa_program = koopa_ir::Program::new();
    let mut state = IRState::default();

    add_libfunc_declarations(&mut koopa_program);
    ast.global_init(&mut koopa_program);

    ast.koopa_append(&mut koopa_program, IRContext::default(), &mut state);
    koopa_program
}

impl From<&UnaryOp> for koopa_ir::BinaryOp {
    fn from(value: &UnaryOp) -> Self {
        match value {
            UnaryOp::Plus => koopa_ir::BinaryOp::Add,
            UnaryOp::Minus => koopa_ir::BinaryOp::Sub,
            UnaryOp::Not => koopa_ir::BinaryOp::Eq,
        }
    }
}

impl TryFrom<&BinaryOp> for koopa_ir::BinaryOp {
    type Error = ();

    fn try_from(value: &BinaryOp) -> Result<Self, Self::Error> {
        match value {
            BinaryOp::Add => Ok(koopa_ir::BinaryOp::Add),
            BinaryOp::Sub => Ok(koopa_ir::BinaryOp::Sub),
            BinaryOp::Mul => Ok(koopa_ir::BinaryOp::Mul),
            BinaryOp::Div => Ok(koopa_ir::BinaryOp::Div),
            BinaryOp::Mod => Ok(koopa_ir::BinaryOp::Mod),
            BinaryOp::Less => Ok(koopa_ir::BinaryOp::Lt),
            BinaryOp::Greater => Ok(koopa_ir::BinaryOp::Gt),
            BinaryOp::LessEqual => Ok(koopa_ir::BinaryOp::Le),
            BinaryOp::GreaterEqual => Ok(koopa_ir::BinaryOp::Ge),
            BinaryOp::Equal => Ok(koopa_ir::BinaryOp::Eq),
            BinaryOp::NotEqual => Ok(koopa_ir::BinaryOp::NotEq),
            BinaryOp::LogicAnd => Err(()),
            BinaryOp::LogicOr => Err(()),
            _ => Err(()),
        }
    }
}

impl From<&ValType> for koopa_ir::Type {
    fn from(val_type: &ValType) -> Self {
        match val_type {
            ValType::Int => koopa_ir::Type::get_i32(),
            ValType::Void => koopa_ir::Type::get_unit(),
        }
    }
}

trait KoopaAppend<T, U> {
    fn koopa_append(&self, koopa_entity: &mut T, context: IRContext, state: &mut IRState) -> U;
}

impl KoopaAppend<koopa_ir::FunctionData, koopa_ir::Value> for Number {
    fn koopa_append(&self, func_data: &mut koopa_ir::FunctionData, context: IRContext, state: &mut IRState)
     -> koopa_ir::Value
    {
        let value = func_data.dfg_mut().new_value().integer(self.value);
        value
    }
}

impl FuncParam {
    fn get_koopa_type(&self) -> koopa_ir::Type {
        match self {
            FuncParam::Scalar { btype , ..} => (&btype.clone()).into(),
            FuncParam::Array { btype, shape, .. } => 
            {
                let shape = shape.iter().map(|exp| {
                    let constant: i32 = exp.try_into().expect("ConstDecl expect a const value at compile time");
                    constant as usize
                }).collect::<Vec<_>>();
                let elme_type = get_type_of_array(&shape, btype.into());
                koopa_ir::Type::get_pointer(elme_type)
            }
        }
    }
    fn get_ident(&self) -> Ident {
        match self {
            FuncParam::Scalar { ident, .. } => ident.clone(),
            FuncParam::Array { ident, .. } => ident.clone(),
        }
    }
}

impl Ident {
    fn get_dim(&self) -> usize {
        let entry = symtable::get(&self.name).expect("Use a symbol that is not declared");
        if let SymValue::VarSymbol(value) = entry
        {
            match value {
                VarSymbol::Array(_, dim) => dim,
                VarSymbol::Ptr(_, dim) => dim,
                VarSymbol::Variable(_) => 0,
                _ => panic!("Use other ident as a variable"),
            }
        }
        else {
            panic!("Use other ident as a variable");
        }
    }
    
}

impl CompUnit {
    fn global_init(&self, program: &mut koopa_ir::Program) {
        for var_decl in &self.decls {
            match var_decl {
                Decl::ConstDecl { btype, const_defs } =>
                {
                    assert!(btype.clone() == ValType::Int);
                    for const_def in const_defs {
                        match const_def {
                            DeclEntry::Scalar { ident, init_val } => {
                                let constant = if let Some(InitVal::Scalar { exp }) = init_val {
                                    exp.try_into().expect("ConstDecl expect a const value at compile time")
                                } else {
                                    panic!("ConstDecl expect a const value at compile time")
                                };
                                symtable::insert(ident.name.as_str(), SymValue::VarSymbol(VarSymbol::Constant(constant)));
                            },
                            DeclEntry::Array { ident, shape, init_val } => {
                                let shape = shape.iter().map(|exp| {
                                    let constant: i32 = exp.try_into().expect("ConstDecl expect a const value at compile time");
                                    constant as usize
                                }).collect::<Vec<_>>();
                                let init_val = if let Some(init_val) = init_val {
                                    init_val
                                } else {
                                    panic!("ConstDecl expect a const value at compile time")
                                };
                                let init_val = init_val.adapt_to_shape(&shape);
                                let init_val = init_val.get_const_vals();
                                let init_val = init_val.into_iter().map(|x| {
                                    let value = program.new_value().integer(x);
                                    program.new_value().integer(x);
                                    value
                                }).collect::<Vec<_>>();
                                let value = aggregate_to_shape(init_val, &shape, program);
                                let array_value = program.new_value().global_alloc(value);
                                program.set_value_name(array_value, Some(format!("@{}", ident.name)));
                                symtable::insert(ident.name.as_str(), SymValue::VarSymbol(VarSymbol::Array(array_value, shape.len())));
                            }
                        }
                    }
                    
                },
                Decl::VarDecl { btype, var_defs } =>
                {
                    assert!(btype.clone() == ValType::Int);
                    for var_def in var_defs {
                        match var_def {
                            DeclEntry::Scalar { ident, init_val } => {
                                let init_val = if let None = init_val {
                                    program.new_value().zero_init(btype.into())
                                }
                                else if let Some(InitVal::Scalar { exp }) = init_val {
                                    let value = exp.try_into().expect("Decl expect a const value at compile time");
                                    program.new_value().integer(value)
                                }
                                else {
                                    panic!("VarDecl expect a scalar value");
                                };
                                let var = program.new_value().global_alloc(init_val);
                                program.set_value_name(var, Some(format!("@{}", ident.name)));
                                symtable::insert(ident.name.as_str(), SymValue::VarSymbol(VarSymbol::Variable(var)));
                            },
                            DeclEntry::Array { ident, shape, init_val } => {
                                let shape = shape.iter().map(|exp| {
                                    let constant: i32 = exp.try_into().expect("ConstDecl expect a const value at compile time");
                                    constant as usize
                                }).collect::<Vec<_>>();
                                let init_val = if let Some(init_val) = init_val {
                                    let init_val = init_val.adapt_to_shape(&shape);
                                    let init_val = init_val.get_const_vals();
                                    let init_val = init_val.into_iter().map(|x| {
                                        let value = program.new_value().integer(x);
                                        program.new_value().integer(x);
                                        value
                                    }).collect::<Vec<_>>();
                                    aggregate_to_shape(init_val, &shape, program)
                                } else {
                                    let type_ = get_type_of_array(&shape, btype.into());
                                    program.new_value().zero_init(type_)
                                };
                                let array_value = program.new_value().global_alloc(init_val);
                                program.set_value_name(array_value, Some(format!("@{}", ident.name)));
                                symtable::insert(ident.name.as_str(), SymValue::VarSymbol(VarSymbol::Array(array_value, shape.len())));   
                            }
                        }
                    }
                }
            }
        }
        
        for func_def in &self.func_defs {
            let params = func_def.params.iter().map(|param| param.get_koopa_type()).collect::<Vec<_>>();
            let func = program.new_func(koopa_ir::FunctionData::new(
                "@".to_string() + &func_def.ident.name,
                params,
                (&func_def.func_type).into(),
            ));
            symtable::insert(func_def.ident.name.as_str(), SymValue::Function(func));
        }
    }
}

impl KoopaAppend<koopa_ir::FunctionData, koopa_ir::Value> for Ident {
    fn koopa_append(&self, func_data: &mut koopa_ir::FunctionData, context: IRContext, state: &mut IRState)
    -> koopa_ir::Value {
        let entry = symtable::get(&self.name).expect("Use a symbol that is not declared");
        if let SymValue::VarSymbol(value) = entry
        {
            match value {
                VarSymbol::Constant(constant) => {
                    let value = func_data.dfg_mut().new_value().integer(constant);
                    value
                },
                VarSymbol::Variable(value) => {
                    let load_inst = func_data.dfg_mut().new_value().load(value);
                    state.instrs_list.push_back(load_inst);
                    load_inst
                },
                VarSymbol::Array(value, ..) => {
                    let zero = func_data.dfg_mut().new_value().integer(0);
                    let ptr = func_data.dfg_mut().new_value().get_elem_ptr(value, zero);
                    state.instrs_list.push_back(ptr);
                    ptr
                },
                VarSymbol::Ptr(value, ..) => {
                    let load_inst = func_data.dfg_mut().new_value().load(value);
                    state.instrs_list.push_back(load_inst);
                    load_inst
                }
            }
        }
        else {
            panic!("Use other ident as a variable");
        }
    }
}

impl KoopaAppend<koopa_ir::FunctionData, koopa_ir::Value> for Lval {
    fn koopa_append(&self, func_data: &mut koopa_ir::FunctionData, context: IRContext, state: &mut IRState)
    -> koopa_ir::Value {
        match self {
            Lval::Ident { ident } => ident.koopa_append(func_data, context, state),
            Lval::Array { ident, indices } => {
                let mut value = ident.koopa_append(func_data, context, state);
                assert!(indices.len() <= ident.get_dim(), "Array index out of bound");
                let is_load_scaler = (indices.len() == ident.get_dim());
                let mut is_first = true;
                for idx in indices.iter()
                {
                    let idx_value = idx.koopa_append(func_data, context, state);
                    value = if is_first
                    {
                        is_first = false;
                        func_data.dfg_mut().new_value().get_ptr(value, idx_value)
                    }
                    else
                    {
                        func_data.dfg_mut().new_value().get_elem_ptr(value, idx_value)
                    };
                    state.instrs_list.push_back(value);
                }
                let ret_value = if is_load_scaler
                {
                    func_data.dfg_mut().new_value().load(value)
                }
                else
                {
                    let zero = func_data.dfg_mut().new_value().integer(0);
                    func_data.dfg_mut().new_value().get_elem_ptr(value, zero)
                };
                state.instrs_list.push_back(ret_value);
                ret_value
            }
        }
    }
}

impl Lval {
    fn addr_koopa_append(&self, func_data: &mut koopa_ir::FunctionData, context: IRContext, state: &mut IRState) -> koopa_ir::Value
    {
        match self {
            Lval::Ident { ident } => {
                let entry = symtable::get(&ident.name).expect("Use a symbol that is not declared");
                if let SymValue::VarSymbol(value) = entry
                {
                    match value {
                        VarSymbol::Variable(value) => value,
                        _ => panic!("Use other ident as a variable"),
                    }
                }
                else {
                    panic!("Use other ident as a variable");
                }
            },
            Lval::Array { ident, indices: index } => {
                let entry = symtable::get(&ident.name).expect("Use a symbol that is not declared");
                let mut value = if let SymValue::VarSymbol(value) = entry
                {
                    match value {
                        VarSymbol::Array(value, ..) => 
                        {
                            let zero = func_data.dfg_mut().new_value().integer(0);
                            let ptr = func_data.dfg_mut().new_value().get_elem_ptr(value, zero);
                            state.instrs_list.push_back(ptr);
                            ptr
                        },
                        VarSymbol::Ptr(value, ..) => 
                        {
                            let load_inst = func_data.dfg_mut().new_value().load(value);
                            state.instrs_list.push_back(load_inst);
                            load_inst
                        },
                        _ => panic!("Use other ident as a variable"),
                    }
                }
                else {
                    panic!("Use other ident as a variable");
                };
                let mut is_first = true;
                for idx in index.iter()
                {
                    let idx_value = idx.koopa_append(func_data, context, state);
                    value = if is_first
                    {
                        is_first = false;
                        func_data.dfg_mut().new_value().get_ptr(value, idx_value)
                    }
                    else
                    {
                        func_data.dfg_mut().new_value().get_elem_ptr(value, idx_value)
                    };
                    state.instrs_list.push_back(value);
                }
                value
            }
        }
    }
}


impl KoopaAppend<koopa_ir::FunctionData, koopa_ir::Value> for Exp {

    /// # Returns
    /// (Value, ValueList)
    /// - `Value` is the value for Exp itself
    /// - `ValueList` contains values to be added to insts
    fn koopa_append(&self, func_data: &mut koopa_ir::FunctionData, context: IRContext, state: &mut IRState)
     -> koopa_ir::Value {
        match self {
            Exp::Number{value} => value.koopa_append(func_data, context, state),
            Exp::Lval { lval } => lval.koopa_append(func_data, context, state),
            Exp::UnaryExp{unary_op : UnaryOp::Plus, exp} => exp.koopa_append(func_data, context, state),
            Exp::UnaryExp{unary_op, exp} => {
                let value = exp.koopa_append(func_data, context, state);
                let zero = func_data.dfg_mut().new_value().integer(0);
                let value = func_data.dfg_mut().new_value().binary(unary_op.into(), zero, value);
                state.instrs_list.push_back(value);
                value
            }
            Exp::BinaryExp { binary_op, lhs, rhs } =>
            {

                let koopa_binary_op: Result<koopa_ir::BinaryOp, _> = binary_op.try_into();

                // Process binary operations that can be mapped to koopa IR directly
                if let Ok(binary_op) = koopa_binary_op
                {
                    let lhs_value = lhs.koopa_append(func_data, context, state);
                    let rhs_value = rhs.koopa_append(func_data, context, state);
                    let value = func_data.dfg_mut().new_value().binary(binary_op, lhs_value, rhs_value);
                    state.instrs_list.push_back(value);
                    return value;
                }

                // Process short-circuit evaluation binary operations
                let value;
                let result = func_data.dfg_mut().new_value().alloc(koopa_ir::Type::get_i32());
                state.instrs_list.push_back(result);
                let lhs_value = lhs.koopa_append(func_data, context, state);
                
                match &binary_op {
                    BinaryOp::LogicAnd =>
                    {
                        let zero = func_data.dfg_mut().new_value().integer(0);
                        let store_zero = func_data.dfg_mut().new_value().store(zero, result);
                        state.instrs_list.push_back(store_zero);
                        let then_bb = func_data.dfg_mut().new_bb().basic_block(state.get_idx());
                        let next_bb = func_data.dfg_mut().new_bb().basic_block(state.get_idx());
                        func_data.layout_mut().bbs_mut().extend([then_bb, next_bb]);
                        let if_value = func_data.dfg_mut().new_value().branch(lhs_value, then_bb, next_bb);
                        state.instrs_list.push_back(if_value);
                        state.set_current_bb(then_bb, func_data);
                        let rhs_value = rhs.koopa_append(func_data, IRContext { next_bb: Some(next_bb), ..context }, state);
                        let bool_rhs = func_data.dfg_mut().new_value().binary(koopa_ir::BinaryOp::NotEq, rhs_value, zero);
                        state.instrs_list.push_back(bool_rhs);
                        let store_value = func_data.dfg_mut().new_value().store(bool_rhs, result);
                        let jump_value = func_data.dfg_mut().new_value().jump(next_bb);
                        state.instrs_list.extend([store_value, jump_value]);
                        state.set_current_bb(next_bb, func_data);
                    },
                    BinaryOp::LogicOr =>
                    {
                        let one = func_data.dfg_mut().new_value().integer(1);
                        let zero = func_data.dfg_mut().new_value().integer(0);
                        let store_one = func_data.dfg_mut().new_value().store(one, result);
                        state.instrs_list.push_back(store_one);
                        let then_bb = func_data.dfg_mut().new_bb().basic_block(state.get_idx());
                        let next_bb = func_data.dfg_mut().new_bb().basic_block(state.get_idx());
                        func_data.layout_mut().bbs_mut().extend([then_bb, next_bb]);
                        let cond = func_data.dfg_mut().new_value().binary(koopa_ir::BinaryOp::Eq, lhs_value, zero);
                        state.instrs_list.push_back(cond);
                        let if_value = func_data.dfg_mut().new_value().branch(cond, then_bb, next_bb);
                        state.instrs_list.push_back(if_value);
                        state.set_current_bb(then_bb, func_data);
                        let rhs_value = rhs.koopa_append(func_data, IRContext { next_bb: Some(next_bb), ..context}, state);
                        let bool_rhs = func_data.dfg_mut().new_value().binary(koopa_ir::BinaryOp::NotEq, rhs_value, zero);
                        state.instrs_list.push_back(bool_rhs);
                        let store_value = func_data.dfg_mut().new_value().store(bool_rhs, result);
                        let jump_value = func_data.dfg_mut().new_value().jump(next_bb);
                        state.instrs_list.extend([store_value, jump_value]);
                        state.set_current_bb(next_bb, func_data);
                    },
                    _ => panic!("Binary Op Not expected")
                };
                
                value = func_data.dfg_mut().new_value().load(result);
                state.instrs_list.push_back(value);
                value
            },
            Exp::Call { ident, args } =>
            {
                if let SymValue::Function(func) = symtable::get(ident.name.as_str()).expect("Call a function that is not declared")
                {
                    let args = args.iter().map(|arg| arg.koopa_append(func_data, context, state)).collect::<Vec<_>>();
                    let value = func_data.dfg_mut().new_value().call(func, args);
                    state.instrs_list.push_back(value);
                    value
                }
                else {
                    panic!("Call a non-function");
                }
            }
        }
    }
}

fn get_type_of_array(shape: &Vec<usize>, base_type: koopa_ir::Type) -> koopa_ir::Type
{
    if shape.len() == 0
    {
        return base_type;
    }
    let mut type_ = base_type;
    for dim in shape.iter().rev()
    {
        type_ = koopa_ir::Type::get_array(type_, *dim)
    }
    type_
}

impl InitVal {
    fn get_const_vals(&self) -> Vec<i32>
    {
        let mut out: Vec<i32> = vec![];
        self.__get_const_vals(&mut out);
        out
    }

    fn adapt_to_shape(&self, shape: &Vec<usize>) -> InitVal
    {
        let size = shape.iter().fold(1, |acc, x| acc * x);
        let mut v = vec![InitVal::Scalar { exp: Exp::Number { value: Number { value: 0 } } }; size];
        let index = vec![0; shape.len()];
        let mut agg_shape = shape.clone();
        agg_shape.push(1);
        for i in 0..agg_shape.len()-1
        {
            agg_shape[i] = agg_shape[i] * agg_shape[i+1];
        }
        self.__adapt_to_shape(&mut v[..], index, &agg_shape[..], 1, shape, 0);
        InitVal::Array { init_vals: v.into_iter().map(|x| Box::new(x)).collect() }
    }

    fn __adapt_to_shape(&self, v: &mut [InitVal], mut index: Vec<usize>, 
        agg_shape: &[usize], mut aligned_idx: usize, shape: &[usize],
        mut i: usize)
    {
        let carry = |aligned_idx: &mut usize, indices: &mut Vec<usize>,
        shape: &[usize]|
        {
            for j in (0..shape.len()).rev()
            {
                if indices[j] == shape[j]
                {
                    indices[j] = 0;
                    if j > 0
                    {
                        indices[j-1] += 1;
                    }
                }
            }
            *aligned_idx = 1;
            for j in (0..shape.len()).rev()
            {
                if indices[j] != 0
                {
                    *aligned_idx = j + 1;
                    break;
                }
            }
        };
        if let InitVal::Array { init_vals } = self
        {
            for init_val in init_vals.iter()
            {
                match init_val.deref()
                {
                    InitVal::Scalar {..} => {
                        v[i] = init_val.deref().clone();
                        let back_pos = index.len() - 1;
                        i += 1; index[back_pos] += 1;
                        carry(&mut aligned_idx, &mut index, shape);
                    },
                    InitVal::Array { .. } => {
                        assert!(aligned_idx != agg_shape.len()-1, "Initialization of array is not aligned");
                        let new_size = agg_shape[aligned_idx];
                        let new_index = vec![0; shape.len() - aligned_idx];
                        init_val.__adapt_to_shape(&mut v[i..i+new_size], new_index, &agg_shape[aligned_idx..], 1,
                        &shape[aligned_idx..], 0);
                        i += new_size; index[aligned_idx - 1] += 1;
                        carry(&mut aligned_idx, &mut index, shape);
                    }
                }
            }
        }
        else {
            panic!("This will never happen");
        }
    }

    fn koopa_append(&self, func_data: &mut koopa_ir::FunctionData, context: IRContext, state: &mut IRState) -> Vec<Value>
    {
        let mut out: Vec<Value> = vec![];
        self.__koopa_append(func_data, context, state, &mut out);
        out
    }

    fn __get_const_vals(&self, out: &mut Vec<i32>)
    {
        match self {
            InitVal::Scalar { exp } => {
                let constant: i32 = exp.try_into().expect("ConstDecl expect a const value at compile time");
                out.push(constant);
            },
            InitVal::Array { init_vals } => {
                init_vals.iter().for_each(|init_val| {
                    init_val.__get_const_vals(out);
                });
            }
        }
    }

    fn __koopa_append(&self, func_data: &mut koopa_ir::FunctionData, context: IRContext
    , state: &mut IRState, out: &mut Vec<Value>)
    {
        match self {
            InitVal::Scalar { exp } => {
                let value = exp.koopa_append(func_data, context, state);
                out.push(value); 
            },
            InitVal::Array { init_vals } => {
                init_vals.iter().for_each(|init_val| {
                    init_val.__koopa_append(func_data, context, state, out);
                });
            }
        }
    }
}

/// # Returns
/// - `Value` is the value for the aggregate
fn aggregate_to_shape(init_values: Vec<Value>, shape: &Vec<usize>, program: &mut koopa_ir::Program) -> Value
{
    fn __aggregate_to_shape(init_values: &[Value], shape: &[usize], program: &mut koopa_ir::Program) -> Value {
        if shape.len() == 1 {
            let init_values: Vec<Value> = init_values.to_vec();
            program.new_value().aggregate(init_values)
        }
        else {
            assert!(shape.len() > 1);
            let slice_size = shape[1..].iter().fold(1, |acc, x| acc * x);
            let sub_shape = &shape[1..];
            let mut new_init_values: Vec<Value> = Vec::new();
            for i in 0..shape[0]
            {
                let start = i * slice_size;
                let end = start + slice_size;
                let sub_init_values = &init_values[start..end];
                let sub_agg = __aggregate_to_shape(sub_init_values, sub_shape, program);
                new_init_values.push(sub_agg);
            }
            program.new_value().aggregate(new_init_values)
        }
    }
    __aggregate_to_shape(&init_values[..], &shape[..], program)
}

impl KoopaAppend<koopa_ir::FunctionData, ()> for Decl {
    fn koopa_append(&self, func_data: &mut koopa_ir::FunctionData, context: IRContext, state: &mut IRState)
    -> () {
        match self {
            Decl::ConstDecl { btype, const_defs } => {
                assert!(btype.clone() == ValType::Int);
                for const_def in const_defs {
                    match const_def {
                        DeclEntry::Scalar { ident, init_val } => {
                            let constant = if let Some(InitVal::Scalar { exp }) = init_val {
                                exp.try_into().expect("ConstDecl expect a const value at compile time")
                            } else {
                                panic!("ConstDecl expect a const value at compile time")
                            };
                            symtable::insert(ident.name.as_str(), SymValue::VarSymbol(VarSymbol::Constant(constant)));
                        },
                        DeclEntry::Array { ident, shape, init_val } => {
                            let shape = shape.iter().map(|exp| {
                                let constant: i32 = exp.try_into().expect("ConstDecl expect a const value at compile time");
                                constant as usize
                            }).collect::<Vec<_>>();
                            let init_val = if let Some(init_val) = init_val {
                                init_val
                            } else {
                                panic!("ConstDecl expect a const value at compile time")
                            };
                            let init_val = init_val.adapt_to_shape(&shape);
                            let array_type = get_type_of_array(&shape, btype.into());
                            let array_value = func_data.dfg_mut().new_value().alloc(array_type);
                            symtable::insert(ident.name.as_str(), SymValue::VarSymbol(VarSymbol::Array(array_value, shape.len())));
                            state.instrs_list.push_back(array_value);
                            let size = shape.iter().fold(1, |acc, x| acc * x);
                            let init_vals = init_val.get_const_vals();
                            assert!(init_vals.len() == size, "Array size mismatch");
                            let lval = Lval::Array{
                                ident: ident.clone(),
                                indices: vec![Exp::Number { value: Number { value: 0 } }; shape.len()]
                            };
                            let addr_value = lval.addr_koopa_append(func_data, context, state);
                            
                            for i in 0..size
                            {
                                let value = func_data.dfg_mut().new_value().integer(init_vals[i]);
                                let index_value = func_data.dfg_mut().new_value().integer(i as i32);
                                let pos_value = func_data.dfg_mut().new_value().get_ptr(addr_value, index_value);
                                let store_value = func_data.dfg_mut().new_value().store(value, pos_value);
                                state.instrs_list.extend([pos_value, store_value]);
                            }
                        }
                    }
                }
            },
            Decl::VarDecl { btype, var_defs } => {
                assert!(btype.clone() == ValType::Int);
                for var_def in var_defs {
                    match var_def {
                        DeclEntry::Scalar { ident, init_val } => {
                            let var = func_data.dfg_mut().new_value().alloc(btype.into());
                            symtable::insert(ident.name.as_str(), SymValue::VarSymbol(VarSymbol::Variable(var)));
                            state.instrs_list.push_back(var);
                            if let Some(init_val) = init_val {
                                if let InitVal::Scalar { exp } = init_val {
                                    let value = exp.koopa_append(func_data, context, state);
                                    let store_value = func_data.dfg_mut().new_value().store(value, var);
                                    state.instrs_list.push_back(store_value);
                                }
                                else {
                                    panic!("VarDecl expect a scalar value");
                                }

                            }
                        },
                        DeclEntry::Array { ident, shape, init_val } => {
                            let shape = shape.iter().map(|exp| {
                                let constant: i32 = exp.try_into().expect("ConstDecl expect a const value at compile time");
                                constant as usize
                            }).collect::<Vec<_>>();

                            let array_type = get_type_of_array(&shape, btype.into());
                            let array_value = func_data.dfg_mut().new_value().alloc(array_type);
                            symtable::insert(ident.name.as_str(), SymValue::VarSymbol(VarSymbol::Array(array_value, shape.len())));
                            state.instrs_list.push_back(array_value);
                            if let Some(init_val) = init_val {
                                let init_val = init_val.adapt_to_shape(&shape);
                                let size = shape.iter().fold(1, |acc, x| acc * x);
                                let init_vals = init_val.koopa_append(func_data, context, state);
                                assert!(init_vals.len() == size, "Array size mismatch");
                                let lval = Lval::Array{
                                    ident: ident.clone(),
                                    indices: vec![Exp::Number { value: Number { value: 0 } }; shape.len()]
                                };
                                let addr_value = lval.addr_koopa_append(func_data, context, state);
                            
                                for i in 0..size
                                {
                                    let value = init_vals[i];
                                    let index_value = func_data.dfg_mut().new_value().integer(i as i32);
                                    let pos_value = func_data.dfg_mut().new_value().get_ptr(addr_value, index_value);
                                    let store_value = func_data.dfg_mut().new_value().store(value, pos_value);
                                    state.instrs_list.extend([pos_value, store_value]);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

impl KoopaAppend<koopa_ir::FunctionData, ()> for Stmt
{
    fn koopa_append(&self, func_data: &mut koopa_ir::FunctionData, context: IRContext, state: &mut IRState)
    -> () {
        match self {
            Stmt::Return { exp } => {
                match exp {
                    Some(exp) => {
                        let value = exp.koopa_append(func_data, context, state);
                        let dfg = func_data.dfg_mut();
                        state.instrs_list.push_back(dfg.new_value().ret(Some(value)));
                    }
                    None => {
                        state.instrs_list.push_back(func_data.dfg_mut().new_value().ret(None));
                    }   
                }
            },
            Stmt::Assign { lval, exp } =>
            {
                let addr_value = lval.addr_koopa_append(func_data, context, state);
                let val  = exp.koopa_append(func_data, context, state);
                let dfg = func_data.dfg_mut();
                state.instrs_list.push_back(
                    dfg.new_value().store(val, addr_value)
                );

            },
            Stmt::Exp {exp} => 
            {
                if let Some(exp) = exp
                {
                    let value = exp.koopa_append(func_data, context, state);
                    state.instrs_list.push_back(value);
                }
            }
            Stmt::If { cond, then_block: then_stmt, else_block: else_stmt } =>
            {
                let cond_value= cond.koopa_append(func_data, context, state);
                let then_bb = func_data.dfg_mut().new_bb().basic_block(state.get_idx());
                func_data.layout_mut().bbs_mut().extend([then_bb]);
                if let Some(else_stmt) = else_stmt
                {
                    let else_bb = func_data.dfg_mut().new_bb().basic_block(state.get_idx());
                    func_data.layout_mut().bbs_mut().extend([else_bb]);

                    let if_value = func_data.dfg_mut().new_value().branch(cond_value, then_bb, else_bb);
                    state.instrs_list.push_back(if_value);

                    state.set_current_bb(else_bb, func_data);
                    else_stmt.koopa_append(func_data, context, state);
                }
                else {
                    let if_value = func_data.dfg_mut().new_value().branch(cond_value, then_bb, context.next_bb.unwrap());
                    state.instrs_list.push_back(if_value);
                }

                state.set_current_bb(then_bb, func_data);
                then_stmt.koopa_append(func_data, context, state);
            },
            Stmt::While { cond, block } =>
            {
                let cond_bb = func_data.dfg_mut().new_bb().basic_block(state.get_idx());
                let body_bb = func_data.dfg_mut().new_bb().basic_block(state.get_idx());
                let next_bb = func_data.dfg_mut().new_bb().basic_block(state.get_idx());
                func_data.layout_mut().bbs_mut().extend([cond_bb, body_bb, next_bb]);
                state.instrs_list.push_back(
                    func_data.dfg_mut().new_value().jump(cond_bb)
                );
                state.set_current_bb(cond_bb, func_data);
                let cond_value = cond.koopa_append(func_data, IRContext { next_bb: Some(next_bb), ..context }, state);
                state.instrs_list.push_back(
                func_data.dfg_mut().new_value().branch(cond_value, body_bb, next_bb)
                );
                state.set_current_bb(body_bb, func_data);
                block.koopa_append(func_data, IRContext { next_bb: Some(cond_bb), out_bb: Some(next_bb), cond_bb: Some(cond_bb) }, state);
                state.set_current_bb(next_bb, func_data);
            },
            Stmt::Break =>
            {
                let out_bb = context.out_bb.expect("Break out of loop");
                state.instrs_list.push_back(
                    func_data.dfg_mut().new_value().jump(out_bb)
                );
            }
            Stmt::Continue =>
            {
                let cond_bb = context.cond_bb.expect("Continue out of loop");
                state.instrs_list.push_back(
                    func_data.dfg_mut().new_value().jump(cond_bb)
                );
            }
            Stmt::Block { .. } => panic!("This never happens.")
        }
    }
}

impl KoopaAppend<koopa_ir::FunctionData, ()> for BlockItem {
    fn koopa_append(&self, func_data: &mut koopa_ir::FunctionData, context: IRContext, state: &mut IRState)
     -> () {
        match self {
            BlockItem::Decl { decl } => decl.koopa_append(func_data, context, state),
            BlockItem::Stmt { stmt } => stmt.koopa_append(func_data, context, state)
        }
    }
}

// We assume the block initalization is outside the function
impl KoopaAppend<koopa_ir::FunctionData, ()> for Block {
    fn koopa_append(&self, func_data: &mut koopa_ir::FunctionData, context: IRContext, state: &mut IRState)
    -> () {
        symtable::push_scope();
        let mut jump_end: bool = false;
        for item in &self.block_items
        {
            match item {
                BlockItem::Stmt {stmt: Stmt::Return {..}} =>
                {
                    item.koopa_append(func_data, context, state);
                    jump_end = true;
                    break;
                },
                BlockItem::Stmt {stmt: Stmt::If {..}} =>
                {
                    let next_bb = func_data.dfg_mut().new_bb().basic_block(state.get_idx());
                    func_data.layout_mut().bbs_mut().extend([next_bb]);
                    item.koopa_append(func_data,IRContext {  next_bb: Some(next_bb), ..context }, state);
                    state.set_current_bb(next_bb, func_data);
                    continue;
                },
                BlockItem::Stmt {stmt: Stmt::Block {block}} =>
                {
                    let block_bb= func_data.dfg_mut().new_bb().basic_block(state.get_idx());
                    let next_bb = func_data.dfg_mut().new_bb().basic_block(state.get_idx());
                    func_data.layout_mut().bbs_mut().extend([block_bb, next_bb]);
                    let jmp_value = func_data.dfg_mut().new_value().jump(block_bb);
                    state.instrs_list.push_back(jmp_value);
                    state.set_current_bb(block_bb, func_data);
                    block.koopa_append(func_data, 
                    IRContext { next_bb: Some(next_bb), ..context }, state);
                    state.set_current_bb(next_bb, func_data);
                    continue;
                },
                BlockItem::Stmt { stmt: Stmt::Break } | BlockItem::Stmt { stmt: Stmt::Continue } => {
                    item.koopa_append(func_data, context, state);
                    jump_end = true;
                    break;  // Stop processing further items after break/continue
                },
                _ =>
                {
                    item.koopa_append(func_data, 
                        context, state);
                }
            }
        }
        
       
        if !jump_end
        {
            if let Some(next_bb) = context.next_bb
            {
                let jmp_value = func_data.dfg_mut().new_value().jump(next_bb);
                state.instrs_list.push_back(jmp_value);
            }
            else 
            {
                let ret_exp = if let TypeKind::Function(_, ret_type) = func_data.ty().kind() {
                    match ret_type.kind() {
                        koopa_ir::TypeKind::Unit => None,
                        koopa_ir::TypeKind::Int32 => Some(Exp::Number { value: Number { value: 0 } }),
                        _ => panic!("Return type is not int or void")
                    }
                } else {
                    panic!("Function type is not a function");
                };
                let defalut_return: Stmt = Stmt::Return { exp: ret_exp };
                defalut_return.koopa_append(func_data, context, state);
            }
        }

        symtable::pop_scope();
        state.finalize(func_data);
    }
}

impl KoopaAppend<koopa_ir::Program, koopa_ir::Function> for FuncDef {
    fn koopa_append(&self, program: &mut koopa_ir::Program, context: IRContext, state: &mut IRState)
    -> koopa_ir::Function {
        let func = symtable::get(self.ident.name.as_str()).expect("Use a function that is not declared");
        let func = if let SymValue::Function(func) = func {
            func
        } else {
            panic!("Use a non-function");
        };
        symtable::push_scope();
        let func_data = program.func_mut(func);
        let param_values = func_data.params().iter().cloned().collect::<Vec<_>>();
        param_values.into_iter().for_each(|param| {
            func_data.dfg_mut().set_value_name(param, state.get_idx());
        });
        let entry_bb = func_data.dfg_mut().new_bb().basic_block(state.get_idx());
        func_data.layout_mut().bbs_mut().extend([entry_bb]);
        state.set_current_bb(entry_bb, func_data);
        self.params.iter().enumerate().for_each(|(i, param)| {
            let alloc_value = func_data.dfg_mut().new_value().
            alloc(param.get_koopa_type());
            let ith_param = func_data.params()[i];
            let store_value = func_data.dfg_mut().new_value().store(
                ith_param, alloc_value);
            let symvalue = if let FuncParam::Array { btype, ident, shape } = param
            {
                SymValue::VarSymbol(VarSymbol::Ptr(alloc_value, shape.len() + 1))
            }
            else {
                SymValue::VarSymbol(VarSymbol::Variable(alloc_value))
            };
            symtable::insert(self.params[i].get_ident().name.as_str(), symvalue);
            state.instrs_list.extend([alloc_value, store_value]);
        });
        self.block.koopa_append(func_data, 
        IRContext::default(), state);
        state.finalize(func_data);
        symtable::pop_scope();
        func
    }
}

impl KoopaAppend<koopa_ir::Program, ()> for CompUnit {
    fn koopa_append(&self, program: &mut koopa_ir::Program, context: IRContext, state: &mut IRState)
    -> () {
        
        self.func_defs.iter().for_each(|func_def| {
            func_def.koopa_append(program, context, state);
        });
    }
}

/// # Brief
/// Try to get the i32 value of an exp.
/// # Returns
/// None if it cannot be calculated at compiling time.
impl TryFrom<&Exp> for i32{
    type Error = String;
    fn try_from(exp: &Exp) -> Result<Self, Self::Error> {
        match exp {
            Exp::Number { value } => Ok(value.value),
            Exp::Lval { lval } => {
                match lval {
                    Lval::Ident { ident } => {
                        let value = symtable::get(ident.name.as_str())
                        .expect("Use unrecognized symbol in const Exp");
                        if let SymValue::VarSymbol(VarSymbol::Constant(constant)) = value
                        {
                            return Ok(constant);
                        }
                        Err(format!("Use a function {} in const Exp", ident.name))
                    },
                    Lval::Array { .. } => Err(format!("Use an array as a const value"))
                }
            },
            Exp::BinaryExp { binary_op, lhs, rhs } =>
            {
                let lhs: i32 = lhs.as_ref().try_into()?;
                let rhs: i32 = rhs.as_ref().try_into()?;
                match binary_op {
                    BinaryOp::Add => Ok(lhs + rhs),
                    BinaryOp::Div => Ok(lhs / rhs),
                    BinaryOp::LogicAnd => Ok(((lhs != 0) && (rhs != 0)) as i32),
                    BinaryOp::Equal => Ok((lhs == rhs) as i32),
                    BinaryOp::Greater => Ok((lhs > rhs) as i32),
                    BinaryOp::GreaterEqual => Ok((lhs >= rhs) as i32),
                    BinaryOp::Less => Ok((lhs < rhs) as i32),
                    BinaryOp::LessEqual => Ok((lhs <= rhs) as i32),
                    BinaryOp::Mod => Ok(lhs % rhs),
                    BinaryOp::Mul => Ok(lhs * rhs),
                    BinaryOp::NotEqual => Ok((lhs != rhs) as i32),
                    BinaryOp::LogicOr => Ok(((lhs != 0) || (rhs != 0)) as i32),
                    BinaryOp::Sub => Ok(lhs - rhs)
                }
            },
            Exp::UnaryExp { unary_op, exp } =>
            {
                let val: i32 = exp.as_ref().try_into()?;
                match unary_op {
                    UnaryOp::Minus => Ok(-val),
                    UnaryOp::Not => Ok((val == 0) as i32),
                    UnaryOp::Plus => Ok(val)
                }
            },
            Exp::Call { ident, args } => Err(format!("Call function {} in const Exp", ident.name)),
        }
    }
}
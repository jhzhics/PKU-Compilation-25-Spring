mod symtable;
use symtable::SymValue;
use super::ast::{*};

use std::collections::LinkedList;
use std::f32::consts::E;
use std::mem;
use std::os::unix::process;
use std::task::Context;
use koopa::ir::{builder_traits::*, dfg, BasicBlock, Type, Value, ValueKind};
use koopa::ir as koopa_ir;


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
    pub ints_list: ValueList,
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
            ints_list: ValueList::new(),
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
        if self.ints_list.is_empty() {
            return;
        }
        func_data.layout_mut().bb_mut(self.current_bb.expect("Current_bb must be set")).
        insts_mut().extend(mem::take(&mut self.ints_list));
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
    fn from(func_type: &ValType) -> Self {
        match func_type {
            ValType::Int => koopa_ir::Type::get_i32(),
        }
    }
}

impl From<&FuncType> for koopa_ir::Type {
    fn from(func_type: &FuncType) -> Self {
        match func_type {
            FuncType::Int => koopa_ir::Type::get_i32(),
            FuncType::Void => koopa_ir::Type::get_unit(),
        }
    }
}

trait KoopaAppend<T, U> {
    fn koopa_append(&self, koopa_entity: &mut T, context: IRContext, state: &mut IRState) -> U;
}

impl KoopaAppend<koopa_ir::dfg::DataFlowGraph, koopa_ir::Value> for Number {
    fn koopa_append(&self, dfg: &mut koopa_ir::dfg::DataFlowGraph, context: IRContext, state: &mut IRState)
     -> koopa_ir::Value
    {
        dfg.new_value().integer(self.value)
    }
}

impl KoopaAppend<koopa_ir::dfg::DataFlowGraph, koopa_ir::Value> for Ident {
    fn koopa_append(&self, dfg: &mut koopa_ir::dfg::DataFlowGraph, context: IRContext, state: &mut IRState)
    -> koopa_ir::Value {
        let entry = symtable::get(&self.name).expect("Use a symbol that is not declared");
        if let SymValue::Value(value, constant) = entry
        {
            match constant {
                Some(_) => value,
                None =>
                {
                    let load_inst = dfg.new_value().load(value);
                    state.ints_list.push_back(load_inst);
                    load_inst
                }
            }
        }
        else {
            panic!("Use other ident as a variable");
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
            Exp::Number{value} => value.koopa_append(func_data.dfg_mut(), context, state),
            Exp::Ident { ident} => ident.koopa_append(func_data.dfg_mut(), context, state),
            Exp::UnaryExp{unary_op : UnaryOp::Plus, exp} => exp.koopa_append(func_data, context, state),
            Exp::UnaryExp{unary_op, exp} => {
                let value = exp.koopa_append(func_data, context, state);
                let zero = func_data.dfg_mut().new_value().integer(0);
                let value = func_data.dfg_mut().new_value().binary(unary_op.into(), zero, value);
                state.ints_list.push_back(value);
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
                    state.ints_list.push_back(value);
                    return value;
                }

                // Process short-circuit evaluation binary operations
                let value;
                let result = func_data.dfg_mut().new_value().alloc(Type::get_i32());
                state.ints_list.push_back(result);
                let lhs_value = lhs.koopa_append(func_data, context, state);
                
                match &binary_op {
                    BinaryOp::LogicAnd =>
                    {
                        let zero = func_data.dfg_mut().new_value().integer(0);
                        let store_zero = func_data.dfg_mut().new_value().store(zero, result);
                        state.ints_list.push_back(store_zero);
                        let then_bb = func_data.dfg_mut().new_bb().basic_block(None);
                        let next_bb = func_data.dfg_mut().new_bb().basic_block(None);
                        func_data.layout_mut().bbs_mut().extend([then_bb, next_bb]);
                        let if_value = func_data.dfg_mut().new_value().branch(lhs_value, then_bb, next_bb);
                        state.ints_list.push_back(if_value);
                        state.set_current_bb(then_bb, func_data);
                        let rhs_value = rhs.koopa_append(func_data, IRContext { next_bb: Some(next_bb), ..context }, state);
                        let bool_rhs = func_data.dfg_mut().new_value().binary(koopa_ir::BinaryOp::NotEq, rhs_value, zero);
                        state.ints_list.push_back(bool_rhs);
                        let store_value = func_data.dfg_mut().new_value().store(bool_rhs, result);
                        let jump_value = func_data.dfg_mut().new_value().jump(next_bb);
                        state.ints_list.extend([store_value, jump_value]);
                        state.set_current_bb(next_bb, func_data);
                    },
                    BinaryOp::LogicOr =>
                    {
                        let one = func_data.dfg_mut().new_value().integer(1);
                        let zero = func_data.dfg_mut().new_value().integer(0);
                        let store_one = func_data.dfg_mut().new_value().store(one, result);
                        state.ints_list.push_back(store_one);
                        let then_bb = func_data.dfg_mut().new_bb().basic_block(None);
                        let next_bb = func_data.dfg_mut().new_bb().basic_block(None);
                        func_data.layout_mut().bbs_mut().extend([then_bb, next_bb]);
                        let cond = func_data.dfg_mut().new_value().binary(koopa_ir::BinaryOp::Eq, lhs_value, zero);
                        state.ints_list.push_back(cond);
                        let if_value = func_data.dfg_mut().new_value().branch(cond, then_bb, next_bb);
                        state.ints_list.push_back(if_value);
                        state.set_current_bb(then_bb, func_data);
                        let rhs_value = rhs.koopa_append(func_data, IRContext { next_bb: Some(next_bb), ..context}, state);
                        let bool_rhs = func_data.dfg_mut().new_value().binary(koopa_ir::BinaryOp::NotEq, rhs_value, zero);
                        state.ints_list.push_back(bool_rhs);
                        let store_value = func_data.dfg_mut().new_value().store(bool_rhs, result);
                        let jump_value = func_data.dfg_mut().new_value().jump(next_bb);
                        state.ints_list.extend([store_value, jump_value]);
                        state.set_current_bb(next_bb, func_data);
                    },
                    _ => panic!("Binary Op Not expected")
                };
                
                value = func_data.dfg_mut().new_value().load(result);
                state.ints_list.push_back(value);
                value
            },
            Exp::Call { ident, args } =>
            {
                if let SymValue::Function(func) = symtable::get(ident.name.as_str()).expect("Call a function that is not declared")
                {
                    let args = args.iter().map(|arg| arg.koopa_append(func_data, context, state)).collect::<Vec<_>>();
                    let value = func_data.dfg_mut().new_value().call(func, args);
                    state.ints_list.push_back(value);
                    value
                }
                else {
                    panic!("Call a non-function");
                }
            }
        }
    }
}

impl KoopaAppend<koopa_ir::FunctionData, ()> for Decl {
    fn koopa_append(&self, func_data: &mut koopa_ir::FunctionData, context: IRContext, state: &mut IRState)
    -> () {
        match self {
            Decl::ConstDecl { btype, const_defs } =>
            {
                let dfg = func_data.dfg_mut();
                assert!(btype.clone() == ValType::Int);
                const_defs.iter().for_each(|(ident, exp)|
                {
                    let constant : i32 = exp.try_into().expect("ConstDecl expect a const value at compile time");
                    let value = dfg.new_value().integer(constant);
                    symtable::insert(ident.name.as_str(), SymValue::Value(value, Some(constant)));
                });
            },
            Decl::VarDecl { btype, var_defs } =>
            {
                assert!(btype.clone() == ValType::Int);
                var_defs.iter().for_each(|(ident, exp)|
                {
                    let var = func_data.dfg_mut().new_value().alloc(btype.into());
                    symtable::insert(ident.name.as_str(), SymValue::Value(var, None));
                    state.ints_list.push_back(var);
                    if let Some(exp) = exp
                    {
                        let val  = exp.koopa_append(func_data, context, state);
                        state.ints_list.push_back(
                            func_data.dfg_mut().new_value().store(val, var)
                        );
                    }
                });
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
                        state.ints_list.push_back(dfg.new_value().ret(Some(value)));
                    }
                    None => {
                        state.ints_list.push_back(func_data.dfg_mut().new_value().ret(None));
                    }   
                }
            },
            Stmt::Assign { ident, exp } =>
            {
                if let SymValue::Value(var, constant ) = symtable::get(&ident.name).expect("Assign a variable before declared")
                {
                assert!(constant.is_none(), "Try to assign a constant");
                let val  = exp.koopa_append(func_data, context, state);
                let dfg = func_data.dfg_mut();
                state.ints_list.push_back(
                    dfg.new_value().store(val, var)
                );
                }
                else {
                    panic!("Assign a non-variable");
                }
            },
            Stmt::Exp {exp} => 
            {
                if let Some(exp) = exp
                {
                    let value = exp.koopa_append(func_data, context, state);
                    state.ints_list.push_back(value);
                }
            }
            Stmt::If { cond, then_block: then_stmt, else_block: else_stmt } =>
            {
                let cond_value= cond.koopa_append(func_data, context, state);
                let then_bb = func_data.dfg_mut().new_bb().basic_block(None);
                func_data.layout_mut().bbs_mut().extend([then_bb]);
                if let Some(else_stmt) = else_stmt
                {
                    let else_bb = func_data.dfg_mut().new_bb().basic_block(None);
                    func_data.layout_mut().bbs_mut().extend([else_bb]);

                    let if_value = func_data.dfg_mut().new_value().branch(cond_value, then_bb, else_bb);
                    state.ints_list.push_back(if_value);

                    state.set_current_bb(else_bb, func_data);
                    else_stmt.koopa_append(func_data, context, state);
                }
                else {
                    let if_value = func_data.dfg_mut().new_value().branch(cond_value, then_bb, context.next_bb.unwrap());
                    state.ints_list.push_back(if_value);
                }

                state.set_current_bb(then_bb, func_data);
                then_stmt.koopa_append(func_data, context, state);
            },
            Stmt::While { cond, block } =>
            {
                let cond_bb = func_data.dfg_mut().new_bb().basic_block(None);
                let body_bb = func_data.dfg_mut().new_bb().basic_block(None);
                let next_bb = func_data.dfg_mut().new_bb().basic_block(None);
                func_data.layout_mut().bbs_mut().extend([cond_bb, body_bb, next_bb]);
                state.ints_list.push_back(
                    func_data.dfg_mut().new_value().jump(cond_bb)
                );
                state.set_current_bb(cond_bb, func_data);
                let cond_value = cond.koopa_append(func_data, IRContext { next_bb: Some(next_bb), ..context }, state);
                state.ints_list.push_back(
                func_data.dfg_mut().new_value().branch(cond_value, body_bb, next_bb)
                );
                state.set_current_bb(body_bb, func_data);
                block.koopa_append(func_data, IRContext { next_bb: Some(cond_bb), out_bb: Some(next_bb), cond_bb: Some(cond_bb) }, state);
                state.set_current_bb(next_bb, func_data);
            },
            Stmt::Break =>
            {
                let out_bb = context.out_bb.expect("Break out of loop");
                state.ints_list.push_back(
                    func_data.dfg_mut().new_value().jump(out_bb)
                );
            }
            Stmt::Continue =>
            {
                let cond_bb = context.cond_bb.expect("Continue out of loop");
                state.ints_list.push_back(
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
                    let next_bb = func_data.dfg_mut().new_bb().basic_block(None);
                    func_data.layout_mut().bbs_mut().extend([next_bb]);
                    item.koopa_append(func_data,IRContext {  next_bb: Some(next_bb), ..context }, state);
                    state.set_current_bb(next_bb, func_data);
                    continue;
                },
                BlockItem::Stmt {stmt: Stmt::Block {block}} =>
                {
                    let block_bb= func_data.dfg_mut().new_bb().basic_block(None);
                    let next_bb = func_data.dfg_mut().new_bb().basic_block(None);
                    func_data.layout_mut().bbs_mut().extend([block_bb, next_bb]);
                    let jmp_value = func_data.dfg_mut().new_value().jump(block_bb);
                    state.ints_list.push_back(jmp_value);
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
                state.ints_list.push_back(jmp_value);
            }
            else 
            {
                let defalut_return: Stmt = Stmt::Return { exp: None };
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
        let params = self.params.iter().map(|param| (&param.btype).into()).collect::<Vec<_>>();
        let func = program.new_func(koopa_ir::FunctionData::new(
            "@".to_string() + &self.ident.name,
            params,
            (&self.func_type).into(),
        ));
        symtable::insert(self.ident.name.as_str(), SymValue::Function(func));
        symtable::push_scope();
        let func_data = program.func_mut(func);
        let entry_bb = func_data.dfg_mut().new_bb().basic_block(None);
        func_data.layout_mut().bbs_mut().extend([entry_bb]);
        state.set_current_bb(entry_bb, func_data);
        self.params.iter().enumerate().for_each(|(i, param)| {
            let alloc_value = func_data.dfg_mut().new_value().
            alloc((&param.btype).into());
            let ith_param = func_data.params()[i];
            let store_value = func_data.dfg_mut().new_value().store(
                ith_param, alloc_value);
            symtable::insert(self.params[i].ident.name.as_str(), SymValue::Value(alloc_value, None));
            state.ints_list.extend([alloc_value, store_value]);
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
            Exp::Ident { ident } => 
            {
                let value = symtable::get(ident.name.as_str())
                .expect("Use unrecognized symbol in const Exp");
                if let SymValue::Value(_, Some(constant)) = value
                {
                    return Ok(constant);
                }
                Err(format!("Use a function {} in const Exp", ident.name))
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
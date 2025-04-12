mod symtable;
use super::ast::{*};

use std::collections::LinkedList;
use koopa::ir::{builder_traits::*, Value};
use koopa::ir as koopa_ir;

pub fn build_koopa(ast: CompUnit) -> koopa_ir::Program {
    let mut koopa_program = koopa_ir::Program::new();
    ast.koopa_append(&mut koopa_program);
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

type ValueList = LinkedList<koopa_ir::Value>;
trait KoopaAppend<T, U> {
    fn koopa_append(&self, koopa_entity: &mut T) -> U;
}

impl KoopaAppend<koopa_ir::dfg::DataFlowGraph, koopa_ir::Value> for Number {
    fn koopa_append(&self, dfg: &mut koopa_ir::dfg::DataFlowGraph) -> koopa_ir::Value {
        dfg.new_value().integer(self.value)
    }
}

impl KoopaAppend<koopa_ir::dfg::DataFlowGraph, (koopa_ir::Value, ValueList)> for Ident {
    fn koopa_append(&self, dfg: &mut koopa_ir::dfg::DataFlowGraph) -> (koopa_ir::Value, ValueList) {
        let entry = symtable::get(&self.name).expect("Use a symbol that is not declared");
        let (value, constant) = entry;
        match constant {
            Some(_) => (value, ValueList::new()),
            None =>
            {
                let load_inst = dfg.new_value().load(value);
                let mut value_list = ValueList::new();
                value_list.push_front(load_inst);
                (load_inst, value_list)
            }
        }
    }
}

impl KoopaAppend<koopa_ir::dfg::DataFlowGraph, (koopa_ir::Value, ValueList)> for Exp {

    /// # Returns
    /// (Value, ValueList)
    /// - `Value` is the value for Exp itself
    /// - `ValueList` contains values to be added to insts
    fn koopa_append(&self, dfg: &mut koopa_ir::dfg::DataFlowGraph) -> (koopa_ir::Value, ValueList) {
        match self {
            Exp::Number{value} => (value.koopa_append(dfg), ValueList::new()),
            Exp::Ident { ident} => ident.koopa_append(dfg),
            Exp::UnaryExp{unary_op : UnaryOp::Plus, exp} => exp.koopa_append(dfg),
            Exp::UnaryExp{unary_op, exp} => {
                let (value, mut value_list) = exp.koopa_append(dfg);
                let zero = dfg.new_value().integer(0);
                let value = dfg.new_value().binary(unary_op.into(), zero, value);
                value_list.push_back(value);
                (value, value_list)
            }
            Exp::BinaryExp { binary_op, lhs, rhs } =>
            {
                let (lhs_value, mut lhs_value_list) = lhs.koopa_append(dfg);
                let (rhs_value, mut rhs_value_list) = rhs.koopa_append(dfg);
                let koopa_binary_op: Result<koopa_ir::BinaryOp, _> = binary_op.try_into();
                let mut new_value_list: LinkedList<Value> = LinkedList::new();
                let value;
                if let Ok(binary_op) = koopa_binary_op
                {
                    value = dfg.new_value().binary(binary_op, lhs_value, rhs_value);
                    new_value_list.push_back(value);
                }
                else {
                    match &binary_op {
                        BinaryOp::LogicAnd =>
                        {   
                            let zero = dfg.new_value().integer(0);
                            let l = dfg.new_value().binary(koopa_ir::BinaryOp::NotEq,
                                zero, lhs_value);
                            let r = dfg.new_value().binary(koopa_ir::BinaryOp::NotEq,
                                zero, rhs_value);
                            value = dfg.new_value().binary(koopa_ir::BinaryOp::And,
                                l, r);
                            new_value_list.push_back(l);
                            new_value_list.push_back(r);
                            new_value_list.push_back(value);
                        },
                        BinaryOp::LogicOr =>
                        {
                            let zero = dfg.new_value().integer(0);
                            let or = dfg.new_value().binary(koopa_ir::BinaryOp::Or,
                                lhs_value, rhs_value);
                            value = dfg.new_value().binary(koopa_ir::BinaryOp::NotEq,
                                zero, or);
                            new_value_list.push_back(or);
                            new_value_list.push_back(value);
                        },
                        _ => panic!("Binary Op Not expected")
                    };
                }
                
                lhs_value_list.append(&mut rhs_value_list);
                lhs_value_list.append(&mut new_value_list);
                (value, lhs_value_list)
            }
        }
    }
}

impl KoopaAppend<koopa_ir::dfg::DataFlowGraph, LinkedList<koopa_ir::Value>> for BlockItem {
    fn koopa_append(&self, dfg: &mut koopa_ir::dfg::DataFlowGraph) -> ValueList {
        match self {
            BlockItem::Return { exp } => {
                let (value, mut value_list) = exp.koopa_append(dfg);
                value_list.push_back(dfg.new_value().ret(Some(value)));
                value_list
            },
            BlockItem::ConstDecl { btype, const_defs } =>
            {
                assert!(btype.clone() == ValType::Int);
                const_defs.iter().for_each(|(ident, exp)|
                {
                    let constant : i32 = exp.try_into().expect("ConstDecl expect a const value at compile time");
                    let value = dfg.new_value().integer(constant);
                    symtable::insert(ident.name.as_str(), value, Some(constant));
                });
                ValueList::new()
            },
            BlockItem::VarDecl { btype, var_defs } =>
            {
                assert!(btype.clone() == ValType::Int);
                let mut value_list = ValueList::new();
                var_defs.iter().for_each(|(ident, exp)|
                {
                    let var = dfg.new_value().alloc(btype.into());
                    symtable::insert(ident.name.as_str(), var, None);
                    value_list.push_back(var);
                    if let Some(exp) = exp
                    {
                        let (val, values)  = exp.koopa_append(dfg);
                        value_list.extend(values);
                        value_list.push_back(
                            dfg.new_value().store(val, var)
                        );
                    }
                });
                value_list
            },
            BlockItem::Assign { ident, exp } =>
            {
                let mut value_list = ValueList::new();
                let (var, constant) = symtable::get(&ident.name).expect("Assign a variable before declared");
                assert!(constant.is_none(), "Try to assign a constant");
                let (val, values)  = exp.koopa_append(dfg);
                value_list.extend(values);
                value_list.push_back(
                    dfg.new_value().store(val, var)
                );  
                value_list
            },
            BlockItem::Exp {exp} => ValueList::new(),
            BlockItem::Block { block } => block.koopa_append(dfg).1
        }
    }
}

impl KoopaAppend<koopa_ir::dfg::DataFlowGraph, (koopa_ir::BasicBlock, ValueList)> for Block {
    fn koopa_append(&self, dfg: &mut koopa_ir::dfg::DataFlowGraph) -> (koopa_ir::BasicBlock, ValueList) {
        symtable::push_scope();
        let entry = dfg.new_bb().basic_block(None);
        let mut ret_values = ValueList::new();
        for item in &self.block_items
        {
            let values = item.koopa_append(dfg);
            ret_values.extend(values);
            if let BlockItem::Return {..} = item // Because we are linear IR now. Will be removed in the following tasks
            {
                break;
            }
        }
        symtable::pop_scope();
        (entry, ret_values)
    }
}

impl KoopaAppend<koopa_ir::Program, koopa_ir::Function> for FuncDef {
    fn koopa_append(&self, program: &mut koopa_ir::Program) -> koopa_ir::Function {
        let func = program.new_func(koopa_ir::FunctionData::new(
            "@".to_string() + &self.ident.name,
            vec![],
            (&self.func_type).into(),
        ));
        let func_data = program.func_mut(func);
        let dfg = func_data.dfg_mut();

        let (entry, values) = self.block.koopa_append(dfg);
        func_data.layout_mut().bbs_mut().extend([entry]);
        func_data
            .layout_mut()
            .bb_mut(entry)
            .insts_mut()
            .extend(values);
        func
    }
}

impl KoopaAppend<koopa_ir::Program, ()> for CompUnit {
    fn koopa_append(&self, program: &mut koopa_ir::Program) -> () {
        self.func_def.koopa_append(program);
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
                .expect("Use unrecognized symbol in const Exp").1.expect("Use variable in const Exp");
                Ok(value)
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
            }
        }
    }
}
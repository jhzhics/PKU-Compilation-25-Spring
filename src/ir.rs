
use super::ast::*;

use std::collections::LinkedList;
use koopa::ir::builder_traits::*;
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

impl From<&BinaryOp> for koopa_ir::BinaryOp {
    fn from(value: &BinaryOp) -> Self {
        match value {
            BinaryOp::Add => koopa_ir::BinaryOp::Add,
            BinaryOp::Sub => koopa_ir::BinaryOp::Sub,
            BinaryOp::Mul => koopa_ir::BinaryOp::Mul,
            BinaryOp::Div => koopa_ir::BinaryOp::Div,
            BinaryOp::Mod => koopa_ir::BinaryOp::Mod,
        }
    }
}

impl From<&FuncType> for koopa_ir::Type {
    fn from(func_type: &FuncType) -> Self {
        match func_type {
            FuncType::Int => koopa_ir::Type::get_i32(),
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

impl KoopaAppend<koopa_ir::dfg::DataFlowGraph, (koopa_ir::Value, ValueList)> for Exp {

    /// # Returns
    /// (Value, ValueList)
    /// - `Value` is the value for Exp itself
    /// - `ValueList` contains values to be added to insts
    fn koopa_append(&self, dfg: &mut koopa_ir::dfg::DataFlowGraph) -> (koopa_ir::Value, ValueList) {
        match self {
            Exp::Number{value} => (value.koopa_append(dfg), ValueList::new()),
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
                let value = dfg.new_value().binary(binary_op.into(), lhs_value, rhs_value);
                lhs_value_list.append(&mut rhs_value_list);
                lhs_value_list.push_back(value);
                (value, lhs_value_list)
            }
        }
    }
}

impl KoopaAppend<koopa_ir::dfg::DataFlowGraph, LinkedList<koopa_ir::Value>> for Stmt {
    fn koopa_append(&self, dfg: &mut koopa_ir::dfg::DataFlowGraph) -> ValueList {
        let (_, mut value_list) = self.exp.koopa_append(dfg);
        value_list.push_back(dfg.new_value().ret(value_list.back().clone().map(|v| v.clone())));
        value_list
    }
}

impl KoopaAppend<koopa_ir::dfg::DataFlowGraph, (koopa_ir::BasicBlock, ValueList)> for Block {
    fn koopa_append(&self, dfg: &mut koopa_ir::dfg::DataFlowGraph) -> (koopa_ir::BasicBlock, ValueList) {
        let entry = dfg.new_bb().basic_block(Some("%entry".to_string()));
        let values = self.stmt.koopa_append(dfg);
        (entry, values)
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

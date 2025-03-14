
use super::ast::*;

use std::collections::LinkedList;
use koopa::ir::builder_traits::*;
use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::*;

pub fn build_koopa(ast: CompUnit) -> Program {
    let mut koopa_program = Program::new();
    ast.koopa_append(&mut koopa_program);
    koopa_program
}

impl From<&UnaryOp> for koopa::ir::BinaryOp {
    fn from(value: &UnaryOp) -> Self {
        match value {
            UnaryOp::Plus => BinaryOp::Add,
            UnaryOp::Minus => BinaryOp::Sub,
            UnaryOp::Not => BinaryOp::Eq,
        }
    }
}

impl From<&FuncType> for Type {
    fn from(func_type: &FuncType) -> Self {
        match func_type {
            FuncType::Int => Type::get_i32(),
        }
    }
}

type ValueList = LinkedList<Value>;
trait KoopaAppend<T, U> {
    fn koopa_append(&self, koopa_entity: &mut T) -> U;
}

impl KoopaAppend<DataFlowGraph, Value> for Number {
    fn koopa_append(&self, dfg: &mut DataFlowGraph) -> Value {
        dfg.new_value().integer(self.value)
    }
}

impl KoopaAppend<DataFlowGraph, (Value, ValueList)> for Exp {

    /// # Returns
    /// (Value, ValueList)
    /// - `Value` is the value for Exp itself
    /// - `ValueList` contains values to be added to insts
    fn koopa_append(&self, dfg: &mut DataFlowGraph) -> (Value, ValueList) {
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
        }
    }
}

impl KoopaAppend<DataFlowGraph, LinkedList<Value>> for Stmt {
    fn koopa_append(&self, dfg: &mut DataFlowGraph) -> ValueList {
        let (_, mut value_list) = self.exp.koopa_append(dfg);
        value_list.push_back(dfg.new_value().ret(value_list.back().clone().map(|v| v.clone())));
        value_list
    }
}

impl KoopaAppend<DataFlowGraph, (BasicBlock, ValueList)> for Block {
    fn koopa_append(&self, dfg: &mut DataFlowGraph) -> (BasicBlock, ValueList) {
        let entry = dfg.new_bb().basic_block(Some("%entry".to_string()));
        let values = self.stmt.koopa_append(dfg);
        (entry, values)
    }
}

impl KoopaAppend<Program, Function> for FuncDef {
    fn koopa_append(&self, program: &mut Program) -> Function {
        let func = program.new_func(FunctionData::new(
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

impl KoopaAppend<Program, ()> for CompUnit {
    fn koopa_append(&self, program: &mut Program) -> () {
        self.func_def.koopa_append(program);
    }
}

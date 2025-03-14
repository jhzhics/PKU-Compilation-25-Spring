use super::ast::*;

use koopa::ir::builder_traits::*;
use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::*;

pub fn build_koopa(ast: CompUnit) -> Result<Program, String> {
    let mut koopa_program = Program::new();
    ast.koopa_append(&mut koopa_program)
        .map_err(|_| "Failed to append AST to Koopa program".to_string())?;
    Ok(koopa_program)
}

trait KoopaAppend<T, U = ()> {
    fn koopa_append(&self, koopa_entity: &mut T) -> Result<U, ()>;
}

impl KoopaAppend<DataFlowGraph, Value> for Number {
    fn koopa_append(&self, dfg: &mut DataFlowGraph) -> Result<Value, ()> {
        Ok(dfg.new_value().integer(self.value))
    }
}

impl KoopaAppend<DataFlowGraph, Value> for Stmt {
    fn koopa_append(&self, dfg: &mut DataFlowGraph) -> Result<Value, ()> {
        let value = self.number.koopa_append(dfg)?;
        Ok(dfg.new_value().ret(Some(value)))
    }
}

impl KoopaAppend<DataFlowGraph, (BasicBlock, Vec<Value>)> for Block {
    fn koopa_append(&self, dfg: &mut DataFlowGraph) -> Result<(BasicBlock, Vec<Value>), ()> {
        let entry = dfg.new_bb().basic_block(Some("%entry".to_string()));
        let value = self.stmt.koopa_append(dfg)?;
        Ok((entry, vec![value]))
    }
}

impl From<FuncType> for Type {
    fn from(func_type: FuncType) -> Self {
        match func_type {
            FuncType::Int => Type::get_i32(),
        }
    }
}

impl KoopaAppend<Program, Function> for FuncDef {
    fn koopa_append(&self, program: &mut Program) -> Result<Function, ()> {
        let func = program.new_func(FunctionData::new(
            "@".to_string() + &self.ident.name,
            vec![],
            self.func_type.into(),
        ));
        let func_data = program.func_mut(func);
        let dfg = func_data.dfg_mut();

        let (entry, values) = self.block.koopa_append(dfg)?;
        func_data.layout_mut().bbs_mut().extend([entry]);
        func_data
            .layout_mut()
            .bb_mut(entry)
            .insts_mut()
            .extend(values);
        Ok(func)
    }
}

impl KoopaAppend<Program> for CompUnit {
    fn koopa_append(&self, program: &mut Program) -> Result<(), ()> {
        self.func_def.koopa_append(program)?;
        Ok(())
    }
}

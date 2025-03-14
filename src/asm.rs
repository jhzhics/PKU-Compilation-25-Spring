use koopa::ir::{dfg::DataFlowGraph, entities::ValueData, *};
use std::{collections::LinkedList, thread::panicking};

use crate::ast::FuncDef;

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

impl GenerateAsm for FunctionData {
    fn generate_asm(&self, asm: &mut LinkedList<String>) {
        let name = if self.name().len() > 1 {
            &self.name()[1..]
        } else {
            panic!("An invalid function name {}", self.name())
        };
        asm.push_back(format!("{}:", name));

        for (&bb, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                let value_data = self.dfg().value(inst);
                value_data.generate_ins(asm, self.dfg());
            }
        }
    }
}

impl GenerateIns<DataFlowGraph> for ValueData {
    fn generate_ins(&self, asm: &mut LinkedList<String>, dfg: &DataFlowGraph) {
        match self.kind() {
            ValueKind::Return(ins) => {
                let value = ins.value();
                if value.is_none() {
                    asm.push_back("li a0, 0".to_string());
                    asm.push_back("ret".to_string());
                    return;
                };
                let value = value.unwrap();
                let value = dfg.value(value);
                if let ValueKind::Integer(value) = value.kind() {
                    let value = value.value();
                    asm.push_back(format!("li a0, {}", value));
                    asm.push_back("ret".to_string());
                } else {
                    panic!("Return Ins with a value that is not a numer");
                }
            }
            other => panic!("Not Implemented for value type {:#?}", other),
        }
    }
}

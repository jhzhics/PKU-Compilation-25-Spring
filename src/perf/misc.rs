use std::collections::LinkedList;

use koopa::ir::{entities::ValueData, Program, ValueKind};

pub fn generate_dataseg(prog: &Program, name_prefix: &str) -> LinkedList<String> {
    fn generate_initdata(asm: &mut LinkedList<String>, value_data: &ValueData, prog: &Program) {
        let type_size = value_data.ty().size();
        match value_data.kind() {
            ValueKind::Integer(ins) => {
                assert!(type_size == 4);
                let val = ins.value();
                asm.push_back(format!(".word {}", val));
            }
            ValueKind::ZeroInit(_ins) => {
                asm.push_back(format!(".zero {}", type_size));
            }
            ValueKind::Aggregate(ins) => {
                for value in ins.elems() {
                    let value_data = prog.borrow_value(*value);
                    generate_initdata(asm, &value_data, prog);
                }
            }
            _ => panic!("Not Implemented for value type {:#?}", value_data.kind()),
        }
    }
    let mut asm = LinkedList::new();
    asm.push_back(format!(".data"));
    for (_value, value_data) in prog.borrow_values().iter() {
        if let ValueKind::GlobalAlloc(ins) = value_data.kind() {
            let name = &value_data
                .name()
                .as_ref()
                .expect("The global alloc does not have a name")[1..];
            let name = format!("{}{}", name_prefix, name);
            asm.push_back(format!(".globl {}", name));
            asm.push_back(format!("{}:", name));
            let init_value = prog.borrow_value(ins.init());
            generate_initdata(&mut asm, &init_value, prog);
            asm.push_back(String::new());
        }
    }
    asm
}

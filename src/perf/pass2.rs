use super::ssa_form::SSABlock;
use super::pass1;
use std::collections::{HashMap, LinkedList};

#[derive(Debug, Clone)]
pub struct Pass2Func
{
    pub blocks: HashMap<String, SSABlock>,
    pub name: String,
    pub entry: String,
    pub args: Vec<String>,
}

impl Pass2Func {
    pub fn dump(&self) -> LinkedList<String>
    {
        let mut inst_list: LinkedList<String> = LinkedList::new();
        inst_list.push_back(format!("{}({}):", self.name, self.args.join(", ")));
        let entry_block = self.blocks.get(&self.entry).expect("Entry block should be set for a function");
        inst_list.extend(entry_block.block.dump());
        inst_list.push_back("".to_string());

        self.blocks.iter().for_each(|(name, block)| {
            if name == &self.entry {
                return; // Skip the entry block, it has been added already
            }
            inst_list.extend(block.dump());
            inst_list.push_back("".to_string());
        });

        inst_list
    }
}

pub fn pass2(prog: pass1::Pass1Func) -> Pass2Func {
    let mut pass2func = Pass2Func {
        blocks: HashMap::new(),
        name: prog.name,
        entry: prog.entry.expect("Entry should be set for a function"),
        args: prog.args,
    };   

    prog.blocks.iter().for_each(|(name, block)| {
        pass2func.blocks.insert(name.clone(),
            SSABlock {
                block: block.block.clone(),
                ..Default::default()
            });
    });

    pass2func
}
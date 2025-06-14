use super::ssa_form::SSABlock;
use super::pass1;
use super::active_aly;
use std::collections::BTreeSet;
use std::collections::HashSet;
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
        inst_list.extend(entry_block.dump());
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

pub fn pass2(func: pass1::Pass1Func) -> Pass2Func {
    let mut pass2func = Pass2Func {
        blocks: HashMap::new(),
        name: func.name,
        entry: func.entry.expect("Entry should be set for a function"),
        args: func.args,
    };   

    func.blocks.iter().for_each(|(name, block)| {
        pass2func.blocks.insert(name.clone(),
            SSABlock {
                block: block.block.clone(),
                next: block.next.iter().map(|n| (n.clone(), vec![])).collect(),
                ..Default::default()
            });
    });

    add_prev(&mut pass2func);

    phase1_analyze_params(&mut pass2func);

    pass2func
}


fn add_prev(func: &mut Pass2Func) {
    //! This function is used to add the previous blocks to the next blocks.
    //! It is used to fill in the next blocks of the SSA form.
    let edges = func.blocks.iter()
        .flat_map(|(name, block)| {
            block.next.iter().map(move |(next_name, _)| (name.clone(), next_name.clone()))
        })
        .collect::<Vec<_>>();

    for (from, to) in edges {
        if let Some(block) = func.blocks.get_mut(&to) {
            block.prev.push(from);
        } else {
            panic!("Block {} not found in function {}", to, func.name);
        }
    }
}


/// This function is used to analyze the parameters of the function.
fn phase1_analyze_params(func: &mut Pass2Func)
{
    let mut modified_set = func.blocks.keys()
        .map(|k| k.clone())
        .collect::<BTreeSet<String>>();
    while !modified_set.is_empty() {
        let head = modified_set.pop_first().expect("Modified set should not be empty");
        let block = func.blocks.get(&head).expect("Block should exist");
        let next_blocks = block.next.iter()
            .map(|(next_name, _)| func.blocks.get(next_name).expect("Next block should exist"))
            .collect::<Vec<_>>();

        let out = next_blocks.iter()
            .flat_map(|b| b.params.iter())
            .cloned()
            .collect::<HashSet<String>>();
        let block = func.blocks.get_mut(&head).expect("Block should exist");
        let block_in = active_aly::active_analyze(&block.block, out, None);
        if block_in != block.params {
            block.params = block_in;
            modified_set.extend(block.prev.iter().cloned());
        }
    }
    let keys = func.blocks.keys().cloned().collect::<Vec<_>>();
    for block_name in keys {
        let block = func.blocks.get(&block_name).expect("Block should exist");
        let next_blocks = block.next.iter()
            .map(|(next_name, _)| func.blocks.get(next_name).expect("Next block should exist"))
            .collect::<Vec<_>>();
        let new_next = next_blocks.iter()
            .map(|b| {
                (b.block.name.clone(), b.params.iter().cloned().collect::<Vec<_>>())
            }).collect::<Vec<_>>();
        let block = func.blocks.get_mut(&block_name).expect("Block should exist");
        block.next = new_next;
    }
}
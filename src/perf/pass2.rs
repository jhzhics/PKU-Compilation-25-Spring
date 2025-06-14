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

    number_ssa(&mut pass2func);

    phase2_eliminate_params(&mut pass2func);
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

fn number_ssa(func: &mut Pass2Func) {
    let mut state = HashMap::new();
    func.blocks.iter_mut().for_each(|(_, block)| {
        number_ssa_block(block, &mut state);
        state.iter_mut().for_each(|(_var, count)| {
            *count += 1;
        });
    });
}

fn number_ssa_block(block: &mut SSABlock, state: &mut HashMap<String, usize>) 
{
    let number_write = |var: String, state: &mut HashMap<String, usize>|
    {
        if let Some(count) = state.get_mut(&var) {
            *count += 1;
            format!("{}_{}", var, count)
        } else {
            state.insert(var.clone(), 1);
            format!("{}_1", &var)
        }
    };

    let number_read = |var: String, state: &mut HashMap<String, usize>|
    {
        if let Some(count) = state.get_mut(&var) {
            format!("{}_{}", var, count)
        } else {
            state.insert(var.clone(), 1);
            format!("{}_1", &var)
        }
    };

    block.params = block.params.iter()
        .map(|var| number_read(var.clone(), state))
        .collect::<HashSet<String>>();

    block.block.instrs.iter_mut().for_each(|inst| {
        inst.map_read_vars(|var| {
            number_read(var.clone(), state)
        });
        inst.map_write_vars(|var| {
            number_write(var.clone(), state)
        });
    });

    block.next.iter_mut().for_each(|(next_name, next_params)| {
        *next_params = next_params.iter()
            .map(|var| number_read(var.clone(), state))
            .collect::<Vec<_>>();
    });
}

fn block_substitute_var(block: &mut SSABlock, old_var: &str, new_var: &str) {
    //! This function is used to substitute a variable in the block.
    block.params = block.params.iter()
        .map(|var| if var == old_var { new_var.to_string() } else { var.clone() })
        .collect::<HashSet<String>>();
    block.block.instrs.iter_mut().for_each(|inst| {
        inst.map_read_vars(|var| if var == old_var { new_var.to_string() } else { var.clone() });
        inst.map_write_vars(|var| if var == old_var { new_var.to_string() } else { var.clone() });
    });
    block.next.iter_mut().for_each(|(_, next_params)| {
        *next_params = next_params.iter()
            .map(|var| if var == old_var { new_var.to_string() } else { var.clone() })
            .collect::<Vec<_>>();
    });
}

fn phase2_eliminate_params(func: &mut Pass2Func) {
    //! This function is used to eliminate the parameters of the function.
    
    let mut modified_set = func.blocks.keys()
        .map(|k| k.clone())
        .collect::<BTreeSet<String>>();
    
    while !modified_set.is_empty() {
        let head = modified_set.pop_first().expect("Modified set should not be empty");
        let block = func.blocks.get(&head).expect("Block should exist");
        let prev_blocks = block.prev.iter()
            .map(|prev_name| func.blocks.get(prev_name).expect("Previous block should exist"))
            .collect::<Vec<_>>();
        if prev_blocks.is_empty() {
            continue; // No previous blocks, nothing to eliminate
        }
        let params = block.params.iter().cloned();
        let mut erased: Vec<(String, String)> = Vec::new();
        for param in params {
            let prefix_param = param.split('_');
            let prefinx_parts_count = prefix_param.clone().count();
            let prefix_param = prefix_param.take(prefinx_parts_count - 1).collect::<Vec<_>>().join("_");
            let mut record: Option<String> = None;
            let mut is_prev_coherent = true;
            for prev_block in &prev_blocks {
                if !is_prev_coherent {
                    break;
                }
                prev_block.next.iter().filter(|(next_name, _) | next_name == &head)
                    .for_each(|(_, next_params)| {
                        let mut found = false;
                        for next_param in next_params {
                            if next_param.starts_with(&prefix_param) {
                                assert!(found == false, "Found multiple parameters with the same prefix in the previous blocks");
                                found = true;
                                if let Some(ref rec) = record {
                                    if rec != next_param {
                                        is_prev_coherent = false;
                                        break;
                                    }
                                } else {
                                    record = Some(next_param.clone());

                                }
                            }
                        }
                        assert!(found);
                    });

            }
            if is_prev_coherent {
                erased.push((param.clone(), record.expect("Record should be set")));
            }
        }
        if erased.is_empty() {
            continue;
        }
        block.next.iter().for_each(|(next_name, _next_params)|
            { modified_set.insert(next_name.clone()); }
        );
        let prev_block_names = block.prev.clone();
        prev_block_names.into_iter().for_each(|prev_name| {
            if let Some(prev_block) = func.blocks.get_mut(&prev_name) {
                prev_block.next.iter_mut().filter(|(next_name, _)| next_name == &head)
                    .for_each(|(_, next_params)| {
                        next_params.retain(|var| !erased.iter().any(|(_, old)| old == var));
                    });
            }
            else {
                panic!("Previous block {} not found in function {}", prev_name, func.name);
            }
        });
        let block = func.blocks.get_mut(&head).expect("Block should exist");
        block.params = block.params.iter()
            .filter(|var| !erased.iter().any(|(old, _)| old == *var))
            .cloned()
            .collect::<HashSet<String>>();
        for (old_var, new_var) in erased {
            block_substitute_var(block, &old_var, &new_var);
        }
    }
}
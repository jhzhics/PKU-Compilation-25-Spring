use std::collections::{HashMap, VecDeque};
use std::sync::Mutex;
use koopa::ir::{Value, Function};
use lazy_static::lazy_static;

#[derive(Debug, Clone, Copy)]
pub enum VarSymbol
{
    Variable(Value),
    Constant(i32),
}

#[derive(Debug, Clone, Copy)]
pub enum SymValue {
    VarSymbol(VarSymbol),
    Function(Function),
}
struct Entry
{
    pub value: SymValue,
    pub depth: i32
}

struct Symtable
{
    table : HashMap<String, VecDeque<Entry>>,
    depth : i32,
}

impl Symtable {
    pub fn new() -> Symtable
    {
        Symtable { table: HashMap::new(), depth: 0 }
    }

    pub fn insert(&mut self ,s: &str, val: SymValue)
    {
        let q;

        if let Some(q_) = self.table.get_mut(s)
        {
            q = q_;
        }
        else {
            self.table.insert(s.to_string(), VecDeque::new());
            q = self.table.get_mut(s).unwrap();
        }

        if let Some(e) = q.back()
        {
            assert!(e.depth < self.depth, "Multiple Definitions conflict with current scope");
        }
        q.push_back(Entry { value: val, depth: self.depth });
        
    }

    pub fn get(&self, s: &str) -> Option<SymValue>
    {
        self.table.get(s).map(|q|{
            let e = q.back().expect("An existing deque is not empty");
            e.value
        })
    }

    pub fn push_scope(&mut self)
    {
        self.depth += 1;
    }

    pub fn pop_scope(&mut self)
    {
        let mut to_removed_var_keys = Vec::<String>::new();
        for item in &mut self.table
        {
            if item.1.back().expect("An existing deque is not empty").depth >= self.depth
            {
                item.1.pop_back();
            }
            if item.1.is_empty()
            {
                to_removed_var_keys.push(item.0.clone());
            }
        }
        for k in to_removed_var_keys
        {
            self.table.remove(&k);
        }
        self.depth -= 1;
    }
    
    
}

lazy_static!{
    static ref GLOBAL_MAP: Mutex<Symtable> = Mutex::new(Symtable::new());
}


pub fn insert(s: &str, val: SymValue)
{
    let mut global_symtable = GLOBAL_MAP.lock().unwrap();
    global_symtable.insert(s, val);
    
}

pub fn get(s: &str) -> Option<SymValue>
{
    let global_symtable = GLOBAL_MAP.lock().unwrap();
    global_symtable.get(s)
}

pub fn push_scope()
{
    let mut global_symtable = GLOBAL_MAP.lock().unwrap();
    global_symtable.push_scope();
}

pub fn pop_scope()
{
    let mut global_symtable = GLOBAL_MAP.lock().unwrap();
    global_symtable.pop_scope();
}
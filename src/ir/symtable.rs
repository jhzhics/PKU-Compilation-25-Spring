use std::collections::{HashMap, VecDeque};
use std::sync::Mutex;
use koopa::ir::Value;
use lazy_static::lazy_static;

struct Entry
{
    pub value: Value,
    pub const_int: Option<i32>,
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

    pub fn insert(&mut self ,s: &str, val: Value, constant: Option<i32>)
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
            assert!(e.depth < self.depth, "Multiple Definitions");
        }
        q.push_back(Entry { value: val, const_int: constant, depth: self.depth });
        
    }

    pub fn get(&self, s: &str) -> Option<(Value, Option<i32>)>
    {
        self.table.get(s).map(|q|{
            let e = q.back().expect("An existing deque is not empty");
            (e.value, e.const_int)
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


pub fn insert(s: &str, val: Value, constant: Option<i32>)
{
    let mut global_symtable = GLOBAL_MAP.lock().unwrap();
    global_symtable.insert(s, val, constant);
    
}

pub fn get(s: &str) -> Option<(Value, Option<i32>)>
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
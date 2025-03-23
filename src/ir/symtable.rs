use std::collections::HashMap;
use std::sync::Mutex;
use koopa::ir::Value;
use lazy_static::lazy_static;

lazy_static!{
    static ref GLOBAL_MAP: Mutex<HashMap<String, (Value, Option<i32>)>> = Mutex::new(HashMap::new());
}


pub fn insert(s: &str, val: Value, constant: Option<i32>)
{
    let mut global_map = GLOBAL_MAP.lock().unwrap();
    assert!(global_map.get(s).is_none(), "Duplicate Definition");
    global_map.insert(s.to_string(), (val, constant));
}

pub fn get(s: &str) -> Option<(Value, Option<i32>)>
{
    let global_map = GLOBAL_MAP.lock().unwrap();
    global_map.get(s).map(|i|i.clone())
}
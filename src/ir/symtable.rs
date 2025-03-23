use std::collections::HashMap;
use std::sync::Mutex;
use lazy_static::lazy_static;

lazy_static!{
    static ref GLOBAL_MAP: Mutex<HashMap<String, i32>> = Mutex::new(HashMap::new());
}


pub fn insert(s: &str, val: i32)
{
    let mut global_map = GLOBAL_MAP.lock().unwrap();
    global_map.insert(s.to_string(), val);
}

pub fn get(s: &str) -> Option<i32>
{
    let global_map = GLOBAL_MAP.lock().unwrap();
    global_map.get(s).map(|i|i.clone())
}
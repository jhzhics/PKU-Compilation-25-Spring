use koopa::ir::Value;
use lazy_static::lazy_static;
use std::collections::{HashMap, VecDeque};
use std::sync::Mutex;

lazy_static! {
    static ref GLOBAL_MAP: Mutex<HashMap<Value, String>> = Mutex::new(HashMap::new());
    
    static ref REG_POOL: Mutex<VecDeque<String>> = Mutex::new(VecDeque::from(vec![
        "t0".to_string(), "t1".to_string(), "t2".to_string(), "t3".to_string(),
        "t4".to_string(), "t5".to_string(), "t6".to_string(), "a0".to_string(),
        "a1".to_string(), "a2".to_string(), "a3".to_string(), "a4".to_string(),
        "a5".to_string(), "a6".to_string(), "a7".to_string()
    ]));
}

fn get_new_reg() -> Option<String> {
    let mut pool = REG_POOL.lock().unwrap();
    pool.pop_front()
}

// pub fn get_ins_reg(value: &Value) -> Option<String> {
//     let global_map = GLOBAL_MAP.lock().unwrap();
//     global_map.get(value).cloned()
// }

pub fn remove_reg(value: &Value) -> Option<String> {
    let mut global_map = GLOBAL_MAP.lock().unwrap();
    global_map.remove(value).map(|reg| {
        REG_POOL.lock().unwrap().push_front(reg.clone());
        reg
    })
}

pub fn alloc_ins_reg(value: &Value) -> String {
    let target_reg= get_new_reg().expect("No available registers!");
    
    let mut global_map = GLOBAL_MAP.lock().unwrap();
    global_map.insert(value.clone(), target_reg.clone());
    target_reg
}
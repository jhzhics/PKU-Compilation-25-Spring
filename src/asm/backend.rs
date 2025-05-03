use koopa::ir::{BasicBlock, Value};
use lazy_static::lazy_static;
use std::collections::{HashMap, VecDeque};
use std::sync::Mutex;

lazy_static! {
    static ref GLOBAL_REG_MAP: Mutex<HashMap<Value, String>> = Mutex::new(HashMap::new());
    
    static ref REG_POOL: Mutex<VecDeque<String>> = Mutex::new(VecDeque::from(vec![
        // "t0".to_string(),  //t0 is reserved for temporary values
        "t1".to_string(), "t2".to_string(), "t3".to_string(),
        "t4".to_string(), "t5".to_string(), "t6".to_string()
    ]));

    static ref GLOBAL_LABEL_MAP: Mutex<HashMap<BasicBlock, String>> = Mutex::new(HashMap::new());
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
    let mut global_reg_map = GLOBAL_REG_MAP.lock().unwrap();
    global_reg_map.remove(value).map(|reg| {
        REG_POOL.lock().unwrap().push_front(reg.clone());
        reg
    })
}

pub fn alloc_ins_reg(value: &Value) -> String {
    let target_reg= get_new_reg().expect("No available registers!");
    
    let mut global_reg_map = GLOBAL_REG_MAP.lock().unwrap();
    global_reg_map.insert(value.clone(), target_reg.clone());
    target_reg
}

pub fn alloc_label(bb: &BasicBlock) -> String {
    let mut global_label_map = GLOBAL_LABEL_MAP.lock().unwrap();
    let label = format!("L{}", global_label_map.len());
    global_label_map.insert(bb.clone(), label.clone());
    label
}

pub fn get_label(bb: &BasicBlock) -> Option<String> {
    let global_label_map = GLOBAL_LABEL_MAP.lock().unwrap();
    global_label_map.get(bb).cloned()
}
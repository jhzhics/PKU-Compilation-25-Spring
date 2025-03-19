use koopa::ir::Value;
use lazy_static::lazy_static;
use std::collections::HashMap;
use std::sync::Mutex;
lazy_static! {
    static ref GLOBAL_MAP: Mutex<HashMap<Value, String>> = Mutex::new(HashMap::new());
}

fn get_new_reg() -> &'static str
{
    const LEN: usize = 15;
    static REG_LIST: [&str; LEN] = ["t0",
                                    "t1",
                                    "t2",
                                    "t3",
                                    "t4",
                                    "t5",
                                    "t6",
                                    "a0",
                                    "a1",
                                    "a2",
                                    "a3",
                                    "a4",
                                    "a5",
                                    "a6",
                                    "a7"];
    static mut I: usize = 0;
    unsafe {
        assert!(I < LEN, "Not enough registers!");
        let ret = REG_LIST[I];
        I += 1;
        ret
    }
}

pub fn get_ins_reg(value: &Value) -> Option<String>
{
    let global_map = GLOBAL_MAP.lock().unwrap();
    global_map.get(value).map(|s| s.clone())
}

pub fn alloc_ins_reg(value: &Value, reg: Option<&str>) ->String
{
    let s = if reg.is_none() { get_new_reg() } else 
    { 
        let s = reg.unwrap();
        if s == "x0"
        {
            get_new_reg()
        }
        else 
        {
            s    
        }
    };
    let mut global_map = GLOBAL_MAP.lock().unwrap();
    global_map.insert(value.clone(), s.to_string());
    s.to_string()
}
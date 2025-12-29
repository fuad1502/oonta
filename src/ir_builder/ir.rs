use std::collections::HashMap;

#[derive(Default)]
pub struct Module {
    global_vars: Vec<GlobalVar>,
    functions: HashMap<String, Function>,
}

pub struct GlobalVar {
    name: String,
    ir_typ: IRType,
}

pub struct Function {
    name: String,
    ret_typ: IRType,
    params: Vec<(String, IRType)>,
    bbs: Vec<BasicBlock>,
    used_names: HashMap<String, usize>,
}

pub struct BasicBlock {
    label: String,
    instrs: Vec<Instr>,
}

pub struct Instr {
    class: InstrClass,
    result: IRValue,
    args: Vec<IRValue>,
}

#[derive(Debug, Clone)]
pub enum IRType {
    Void,
    I32,
    Ptr,
    Struct(Vec<IRType>),
}

pub enum InstrClass {
    Load(IRType),
    Add(IRType),
    Store,
    Call(String),
    GetElemPtr(IRType),
}

#[derive(Debug, Clone)]
pub enum IRValue {
    Void,
    Primitive(IRPrimitive),
    Register(String, IRType),
    Global(String),
}

#[derive(Debug, Clone)]
pub enum IRPrimitive {
    Integer(i32),
}

impl Module {
    pub fn new_global_var(&mut self, name: String, ir_typ: IRType) {
        let global = GlobalVar { name, ir_typ };
        self.global_vars.push(global);
    }

    pub fn new_function(&mut self, name: String, function: Function) {
        self.functions.insert(name, function);
    }

    pub fn get_function(&mut self, name: &str) -> Option<&mut Function> {
        self.functions.get_mut(name)
    }
}

impl Function {
    pub fn new(name: String, ret_typ: IRType, params: Vec<(String, IRType)>) -> Self {
        let mut fun = Self {
            name,
            ret_typ,
            params: vec![],
            bbs: vec![],
            used_names: HashMap::new(),
        };
        let params = params
            .into_iter()
            .map(|(n, t)| (fun.new_name(&n), t))
            .collect();
        fun.params = params;
        let label = fun.new_name("");
        fun.bbs.push(BasicBlock::new(label));
        fun
    }

    pub fn push_instr(&mut self, instr: Instr) {
        self.bbs.last_mut().unwrap().push_instr(instr);
    }

    fn new_name(&mut self, base: &str) -> String {
        if let Some(idx) = self.used_names.get_mut(base) {
            let name = format!("{base}{idx}");
            *idx += 1;
            name
        } else {
            let name = base.to_string();
            self.used_names.insert(name.clone(), 0);
            name
        }
    }
}

impl BasicBlock {
    fn new(label: String) -> Self {
        Self {
            label,
            instrs: vec![],
        }
    }

    fn push_instr(&mut self, instr: Instr) {
        self.instrs.push(instr);
    }
}

impl Instr {
    pub fn store(src: IRValue, dst: IRValue) -> Self {
        Self {
            class: InstrClass::Store,
            result: IRValue::Void,
            args: vec![src, dst],
        }
    }
}

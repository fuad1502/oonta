pub struct Module {
    global_vars: Vec<GlobalVar>,
    functions: Vec<Function>,
}

pub struct GlobalVar {
    name: String,
    typ: IRType,
}

pub struct Function {
    name: String,
    ret_typ: IRType,
    params: Vec<(String, IRType)>,
    bbs: Vec<BasicBlock>,
}

pub struct BasicBlock {
    label: String,
    instrs: Vec<Instr>,
}

struct Instr {
    class: InstrClass,
    result: Value,
    args: Vec<Value>,
}

pub enum IRType {
    I32,
    Ptr,
    Struct(Vec<IRType>),
    Arr(Box<IRType>, usize),
}

pub enum InstrClass {
    Load(IRType),
    Add(IRType),
    Store,
    Call(String),
    GetElemPtr(IRType),
}

pub enum Value {
    Void,
    Register(String, IRType),
    Global(String),
}

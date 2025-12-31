use std::collections::HashMap;

use crate::typ::{Primitive, Type, Variable, normalize_typ};

#[derive(Default)]
pub struct Module {
    global_vars: Vec<GlobalVar>,
    function_defs: HashMap<String, Function>,
    function_decls: HashMap<String, FunSignature>,
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

pub struct FunSignature {
    name: String,
    ret_typ: IRType,
    params: Vec<IRType>,
}

pub struct BasicBlock {
    label: String,
    instrs: Vec<Instr>,
}

#[derive(Clone)]
pub struct Instr {
    class: InstrClass,
    res: IRValue,
    args: Vec<IRValue>,
}

#[derive(Debug, Clone)]
pub enum IRType {
    Void,
    I32,
    I64,
    Ptr,
    Struct(Vec<IRType>),
}

#[derive(Clone)]
pub enum InstrClass {
    Load(IRType),
    Add(IRType),
    Store,
    Call(String),
    GetElemPtr(IRType),
    Return,
}

#[derive(Debug, Clone)]
pub enum IRValue {
    Void,
    Pri(IRPri),
    Reg(String, IRType),
    Global(String, IRType),
}

#[derive(Debug, Clone)]
pub enum IRPri {
    I32(i32),
    I64(i64),
}

impl Module {
    pub fn new_global_var(&mut self, name: String, ir_typ: IRType) {
        let global = GlobalVar { name, ir_typ };
        self.global_vars.push(global);
    }

    pub fn new_function(&mut self, name: String, function: Function) {
        self.function_defs.insert(name, function);
    }

    pub fn new_function_decl(&mut self, name: String, signature: FunSignature) {
        self.function_decls.insert(name, signature);
    }

    pub fn get_function(&mut self, name: &str) -> Option<&mut Function> {
        self.function_defs.get_mut(name)
    }

    pub fn get_function_decl(&mut self, name: &str) -> Option<&FunSignature> {
        self.function_decls.get(name)
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

    pub fn from_typ(name: String, param_names: Vec<String>, typ: Type) -> Self {
        if let Type::Fun(typs) = typ {
            let mut ir_typs = typs
                .into_iter()
                .map(normalize_typ)
                .map(IRType::from)
                .collect::<Vec<IRType>>();
            let ret_typ = ir_typs.pop().unwrap();
            let params = ir_typs
                .into_iter()
                .zip(param_names.into_iter())
                .map(|(ir_typ, name)| (name, ir_typ))
                .collect::<Vec<(String, IRType)>>();
            Function::new(name, ret_typ, params)
        } else {
            unreachable!()
        }
    }

    pub fn add_param(&mut self, param: (String, IRType)) {
        let name = self.new_name(&param.0);
        self.params.push((name, param.1));
    }

    pub fn param(&self, idx: usize) -> IRValue {
        IRValue::Reg(self.params[idx].0.clone(), self.params[idx].1.clone())
    }

    pub fn num_of_params(&self) -> usize {
        self.params.len()
    }

    pub fn getelemptr(&mut self, typ: IRType, src: IRValue, indexes: &[i32]) -> IRValue {
        let mut args = vec![src];
        let mut indexes = indexes
            .iter()
            .map(|i| IRValue::Pri(IRPri::I32(*i)))
            .collect();
        args.append(&mut indexes);
        let res_name = self.new_name("");
        let instr = Instr {
            class: InstrClass::GetElemPtr(typ),
            res: IRValue::Reg(res_name, IRType::Ptr),
            args,
        };
        self.push_instr(instr.clone());
        instr.value()
    }

    pub fn load(&mut self, typ: IRType, src: IRValue) -> IRValue {
        let res_name = self.new_name("");
        let instr = Instr {
            class: InstrClass::Load(typ.clone()),
            res: IRValue::Reg(res_name, typ),
            args: vec![src],
        };
        self.push_instr(instr.clone());
        instr.value()
    }

    pub fn store(&mut self, src: IRValue, dst: IRValue) {
        let instr = Instr {
            class: InstrClass::Store,
            res: IRValue::Void,
            args: vec![src, dst],
        };
        self.push_instr(instr);
    }

    pub fn ret(&mut self, value: IRValue) {
        let instr = Instr {
            class: InstrClass::Return,
            res: IRValue::Void,
            args: vec![value],
        };
        self.push_instr(instr);
    }

    pub fn call(&mut self, name: String, typ: IRType, args: Vec<IRValue>) -> IRValue {
        let res_name = self.new_name("");
        let instr = Instr {
            class: InstrClass::Call(name),
            res: IRValue::Reg(res_name, typ),
            args,
        };
        self.push_instr(instr.clone());
        instr.value()
    }

    pub fn add(&mut self, typ: IRType, lhs: IRValue, rhs: IRValue) -> IRValue {
        let res_name = self.new_name("");
        let instr = Instr {
            class: InstrClass::Add(typ.clone()),
            res: IRValue::Reg(res_name, typ),
            args: vec![lhs, rhs],
        };
        self.push_instr(instr.clone());
        instr.value()
    }

    fn push_instr(&mut self, instr: Instr) {
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

impl FunSignature {
    pub fn new(name: String, ret_typ: IRType, params: Vec<IRType>) -> Self {
        Self {
            name,
            ret_typ,
            params,
        }
    }

    pub fn ret_typ(&self) -> &IRType {
        &self.ret_typ
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
    pub fn value(self) -> IRValue {
        self.res
    }
}

impl IRValue {
    pub fn typ(&self) -> &IRType {
        match self {
            IRValue::Void => &IRType::Void,
            IRValue::Pri(IRPri::I32(_)) => &IRType::I32,
            IRValue::Pri(IRPri::I64(_)) => &IRType::I64,
            IRValue::Reg(_, ir_type) => ir_type,
            IRValue::Global(_, ir_type) => ir_type,
        }
    }
}

impl From<Type> for IRType {
    fn from(typ: Type) -> Self {
        match typ {
            Type::Fun(_) => IRType::Ptr,
            Type::Primitive(Primitive::Integer) => IRType::I32,
            Type::Variable(Variable::Unbound(_)) => todo!(),
            Type::Variable(Variable::Link(_)) => panic!(""),
        }
    }
}

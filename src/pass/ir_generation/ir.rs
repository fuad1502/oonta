use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::fmt::Formatter;
use std::io::Write;
use std::rc::Rc;

use crate::ast::Operator;
use crate::typ::{Primitive, Type, Variable, normalize_typ};

#[derive(Default)]
pub struct Module {
    global_vars: Vec<GlobalVar>,
    global_constants: Vec<GlobalVar>,
    function_defs: BTreeMap<String, Function>,
    function_decls: BTreeMap<String, FunSignature>,
    used_names: HashMap<String, usize>,
}

pub struct GlobalVar {
    name: String,
    ir_typ: IRType,
    init: Option<IRValue>,
    constant: bool,
}

// TODO: Refactor to include FunSignature struct
pub struct Function {
    name: String,
    ret_typ: IRType,
    params: Vec<Param>,
    options: FunOptions,
    bbs: Vec<BasicBlock>,
    curr_bb: String,
    used_names: HashMap<String, usize>,
}

struct Param(String, IRType);

pub struct FunSignature {
    name: String,
    ret_typ: IRType,
    params: Vec<IRType>,
    options: FunOptions,
}

pub struct FunOptions {
    is_varargs: bool,
    allocator: bool,
    fastcc: bool,
}

pub struct BasicBlock {
    label: String,
    instrs: Vec<Instr>,
}

#[derive(Clone)]
pub struct Instr {
    class: InstrClass,
    res: IRValue,
}

#[derive(Debug, Clone)]
pub enum IRType {
    Void,
    I1,
    I8,
    I32,
    I64,
    Double,
    Ptr,
    Struct(Vec<IRType>),
    Array(Box<IRType>, usize),
}

#[derive(Clone)]
pub enum InstrClass {
    Load(IRType, IRValue),
    Add(IRType, IRValue, IRValue),
    Sub(IRType, IRValue, IRValue),
    Mul(IRType, IRValue, IRValue),
    Div(IRType, IRValue, IRValue),
    FAdd(IRType, IRValue, IRValue),
    FSub(IRType, IRValue, IRValue),
    FMul(IRType, IRValue, IRValue),
    FDiv(IRType, IRValue, IRValue),
    Eq(IRType, IRValue, IRValue),
    Neq(IRType, IRValue, IRValue),
    Lte(IRType, IRValue, IRValue),
    Lt(IRType, IRValue, IRValue),
    Gte(IRType, IRValue, IRValue),
    Gt(IRType, IRValue, IRValue),
    And(IRType, IRValue, IRValue),
    Store(IRValue, IRValue),
    Call(IRValue, IRType, Vec<IRValue>, bool),
    Alloca(IRType),
    CondBrk(IRValue, String, String),
    Brk(String),
    Switch(IRValue, String, Vec<(IRValue, String)>),
    GetElemPtr(IRType, IRValue, Vec<IRValue>),
    Return(IRValue),
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
    I1(bool),
    I32(i32),
    I64(i64),
    Double(f64),
    Str(&'static str),
}

impl Module {
    pub fn new_global_var(&mut self, name: &str, ir_typ: IRType, init: Option<IRValue>) -> String {
        let name = self.new_name(name);
        let global = GlobalVar {
            name: name.clone(),
            ir_typ,
            init,
            constant: false,
        };
        self.global_vars.push(global);
        name
    }

    pub fn new_global_constant(&mut self, name: &str, init: IRValue) -> String {
        let name = self.new_name(name);
        let ir_typ = init.typ();
        let global = GlobalVar {
            name: name.clone(),
            ir_typ,
            init: Some(init),
            constant: true,
        };
        self.global_constants.push(global);
        name
    }

    pub fn new_function(&mut self, mut function: Function) -> String {
        let name = self.new_name(&function.name);
        function.name = name.clone();
        self.function_defs.insert(name.clone(), function);
        name
    }

    pub fn new_function_decl(&mut self, signature: FunSignature) {
        self.function_decls
            .insert(signature.name.clone(), signature);
    }

    pub fn get_function(&mut self, name: &str) -> Option<&mut Function> {
        self.function_defs.get_mut(name)
    }

    pub fn get_function_decl(&mut self, name: &str) -> Option<&FunSignature> {
        self.function_decls.get(name)
    }

    pub fn serialize(&self, mut wr: Box<dyn Write>) -> std::io::Result<()> {
        self.function_decls
            .values()
            .try_for_each(|decl| writeln!(wr, "{decl}"))?;
        writeln!(wr)?;
        self.global_constants
            .iter()
            .try_for_each(|var| writeln!(wr, "{var}"))?;
        writeln!(wr)?;
        self.global_vars
            .iter()
            .try_for_each(|var| writeln!(wr, "{var}"))?;
        writeln!(wr)?;
        self.function_defs
            .values()
            .try_for_each(|fun| writeln!(wr, "{fun}\n"))
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

impl std::fmt::Display for GlobalVar {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        let name = &self.name;
        let typ = &self.ir_typ;
        let init = if let Some(val) = &self.init {
            &val.name()
        } else {
            match typ {
                IRType::I32 | IRType::I64 | IRType::I1 => "0",
                IRType::Double => "0.0",
                IRType::Ptr => "null",
                _ => unreachable!(),
            }
        };
        let global_or_const = if self.constant { "constant" } else { "global" };
        write!(fmt, "@{name} = {global_or_const} {typ} {init}")
    }
}

impl Function {
    pub fn new(name: String, ret_typ: IRType, params: Vec<(String, IRType)>) -> Self {
        let mut fun = Self {
            name,
            ret_typ,
            options: FunOptions::default(),
            params: vec![],
            bbs: vec![],
            curr_bb: String::new(),
            used_names: HashMap::new(),
        };
        _ = fun.new_name("");
        let params = params
            .into_iter()
            .map(|(n, t)| Param(fun.new_name(&n), t))
            .collect();
        let entry_label = fun.new_name("entry");
        let entry_bb = BasicBlock::new(entry_label.clone());
        fun.params = params;
        fun.bbs.push(entry_bb);
        fun.curr_bb = entry_label;
        fun
    }

    pub fn from_typ(name: String, param_names: Vec<String>, typ: Rc<RefCell<Type>>) -> Self {
        if let Type::Fun(typs) = normalize_typ(typ) {
            let mut ir_typs = typs.into_iter().map(IRType::from).collect::<Vec<IRType>>();
            let ret_typ = ir_typs.pop().unwrap();
            let params = ir_typs
                .into_iter()
                .zip(param_names)
                .map(|(ir_typ, name)| (name, ir_typ))
                .collect::<Vec<(String, IRType)>>();
            Function::new(name, ret_typ, params)
        } else {
            unreachable!()
        }
    }

    pub fn add_new_bb(&mut self, label: &str) -> String {
        let bb_label = self.new_name(label);
        let bb = BasicBlock::new(bb_label.clone());
        self.bbs.push(bb);
        bb_label
    }

    pub fn create_bb(&mut self, label: &str) -> BasicBlock {
        let bb_label = self.new_name(label);
        BasicBlock::new(bb_label.clone())
    }

    pub fn add_bb(&mut self, bb: BasicBlock) {
        self.bbs.push(bb);
    }

    pub fn set_bb(&mut self, label: String) {
        self.curr_bb = label;
    }

    pub fn add_param(&mut self, param: (String, IRType)) {
        let name = self.new_name(&param.0);
        self.params.push(Param(name, param.1));
    }

    pub fn param(&self, idx: usize) -> IRValue {
        IRValue::Reg(self.params[idx].0.clone(), self.params[idx].1.clone())
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn num_of_params(&self) -> usize {
        self.params.len()
    }

    pub fn getelemptr(&mut self, typ: IRType, src: IRValue, indexes: &[i32]) -> IRValue {
        let indexes = indexes
            .iter()
            .map(|i| IRValue::Pri(IRPri::I32(*i)))
            .collect();
        let res_name = self.new_name("r");
        let instr = Instr {
            class: InstrClass::GetElemPtr(typ, src, indexes),
            res: IRValue::Reg(res_name, IRType::Ptr),
        };
        self.push_instr(instr.clone());
        instr.value()
    }

    pub fn load(&mut self, typ: IRType, src: IRValue) -> IRValue {
        // TODO: better void handling
        if typ.is_void() {
            return IRValue::Void;
        }
        let res_name = self.new_name("r");
        let instr = Instr {
            class: InstrClass::Load(typ.clone(), src),
            res: IRValue::Reg(res_name, typ),
        };
        self.push_instr(instr.clone());
        instr.value()
    }

    pub fn store(&mut self, src: IRValue, dst: IRValue) {
        // TODO: better void handling
        if src.is_void() || dst.is_void() {
            return;
        }
        let instr = Instr {
            class: InstrClass::Store(src, dst),
            res: IRValue::Void,
        };
        self.push_instr(instr);
    }

    pub fn ret(&mut self, value: IRValue) {
        let instr = Instr {
            class: InstrClass::Return(value),
            res: IRValue::Void,
        };
        self.push_instr(instr);
    }

    pub fn fast_call(&mut self, fun: IRValue, typ: IRType, args: Vec<IRValue>) -> IRValue {
        self.call(fun, typ, args, true)
    }

    pub fn normal_call(&mut self, fun: IRValue, typ: IRType, args: Vec<IRValue>) -> IRValue {
        self.call(fun, typ, args, false)
    }

    fn call(&mut self, fun: IRValue, typ: IRType, args: Vec<IRValue>, fast: bool) -> IRValue {
        let res = if typ.is_void() {
            IRValue::Void
        } else {
            let res_name = self.new_name("r");
            IRValue::Reg(res_name, typ.clone())
        };
        let instr = Instr {
            class: InstrClass::Call(fun, typ, args, fast),
            res,
        };
        self.push_instr(instr.clone());
        instr.value()
    }

    pub fn alloca(&mut self, typ: IRType) -> IRValue {
        // TODO: better void handling
        if typ.is_void() {
            return IRValue::Void;
        }
        let res_name = self.new_name("r");
        let instr = Instr {
            class: InstrClass::Alloca(typ),
            res: IRValue::Reg(res_name, IRType::Ptr),
        };
        self.push_instr(instr.clone());
        instr.value()
    }

    pub fn cond_brk(&mut self, cond: IRValue, then_label: String, else_label: String) {
        let instr = Instr {
            class: InstrClass::CondBrk(cond, then_label, else_label),
            res: IRValue::Void,
        };
        self.push_instr(instr);
    }

    pub fn brk(&mut self, label: String) {
        let instr = Instr {
            class: InstrClass::Brk(label),
            res: IRValue::Void,
        };
        self.push_instr(instr);
    }

    pub fn switch(&mut self, value: IRValue, default_label: String, cases: Vec<(usize, String)>) {
        let cases = cases
            .into_iter()
            .map(|(val, label)| (IRValue::Pri(IRPri::I64(val as i64)), label))
            .collect();
        let instr = Instr {
            class: InstrClass::Switch(value, default_label, cases),
            res: IRValue::Void,
        };
        self.push_instr(instr);
    }

    pub fn binop(
        &mut self,
        typ: IRType,
        lhs: IRValue,
        rhs: IRValue,
        operator: Operator,
    ) -> IRValue {
        let res_name = self.new_name("r");
        let op_typ = lhs.typ().clone();
        let class = match operator {
            Operator::Plus => InstrClass::Add(op_typ, lhs, rhs),
            Operator::Minus => InstrClass::Sub(op_typ, lhs, rhs),
            Operator::Star => InstrClass::Mul(op_typ, lhs, rhs),
            Operator::Slash => InstrClass::Div(op_typ, lhs, rhs),
            Operator::PointPlus => InstrClass::FAdd(op_typ, lhs, rhs),
            Operator::PointMinus => InstrClass::FSub(op_typ, lhs, rhs),
            Operator::PointStar => InstrClass::FMul(op_typ, lhs, rhs),
            Operator::PointSlash => InstrClass::FDiv(op_typ, lhs, rhs),
            Operator::Eq => InstrClass::Eq(op_typ, lhs, rhs),
            Operator::Neq => InstrClass::Neq(op_typ, lhs, rhs),
            Operator::Lte => InstrClass::Lte(op_typ, lhs, rhs),
            Operator::Lt => InstrClass::Lt(op_typ, lhs, rhs),
            Operator::Gte => InstrClass::Gte(op_typ, lhs, rhs),
            Operator::Gt => InstrClass::Gt(op_typ, lhs, rhs),
        };
        let instr = Instr {
            class,
            res: IRValue::Reg(res_name, typ),
        };
        self.push_instr(instr.clone());
        instr.value()
    }

    pub fn and(&mut self, lhs: IRValue, rhs: IRValue) -> IRValue {
        let res_name = self.new_name("r");
        let typ = lhs.typ();
        let instr = Instr {
            class: InstrClass::And(typ.clone(), lhs, rhs),
            res: IRValue::Reg(res_name, typ.clone()),
        };
        self.push_instr(instr.clone());
        instr.value()
    }

    fn push_instr(&mut self, instr: Instr) {
        self.bbs
            .iter_mut()
            .find(|bb| bb.label == self.curr_bb)
            .unwrap()
            .push_instr(instr);
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

impl std::fmt::Display for Function {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        let ret_typ = &self.ret_typ;
        let name = &self.name;
        write!(fmt, "define {} {ret_typ} @{name}(", self.options.cc_str())?;
        write_comma_separated(&self.params, fmt)?;
        if self.options.is_varargs {
            write!(fmt, ", ...")?;
        }
        writeln!(fmt, ") {} {{", self.options.fun_attr_str())?;
        self.bbs.iter().try_for_each(|bb| write!(fmt, "{bb}"))?;
        write!(fmt, "}}")
    }
}

impl std::fmt::Display for Param {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        let name = &self.0;
        let typ = &self.1;
        write!(fmt, "{typ} %{name}")
    }
}

impl FunSignature {
    pub fn new(name: String, ret_typ: IRType, params: Vec<IRType>) -> Self {
        Self {
            name,
            ret_typ,
            params,
            options: FunOptions::default(),
        }
    }

    pub fn varargs(mut self) -> Self {
        self.options.is_varargs = true;
        self
    }

    pub fn ccc(mut self) -> Self {
        self.options.fastcc = false;
        self
    }

    pub fn alloc(mut self) -> Self {
        self.options.allocator = true;
        self
    }

    pub fn ret_typ(&self) -> &IRType {
        &self.ret_typ
    }
}

impl std::fmt::Display for FunSignature {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        let ret_typ = &self.ret_typ;
        let name = &self.name;
        write!(fmt, "declare {} {ret_typ} @{name}(", self.options.cc_str())?;
        write_comma_separated(&self.params, fmt)?;
        if self.options.is_varargs {
            write!(fmt, ", ...")?;
        }
        write!(fmt, ") {}", self.options.fun_attr_str())?;
        Ok(())
    }
}

impl FunOptions {
    fn cc_str(&self) -> &'static str {
        // TODO: use other calling convention for fast call
        if self.fastcc { "ccc" } else { "" }
    }

    fn fun_attr_str(&self) -> &'static str {
        if self.allocator {
            "mustprogress nofree nounwind willreturn allockind(\"alloc,uninitialized\") allocsize(0) memory(inaccessiblemem: readwrite)"
        } else {
            ""
        }
    }
}

impl Default for FunOptions {
    fn default() -> Self {
        Self {
            is_varargs: false,
            allocator: false,
            fastcc: true,
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

    pub fn label(&self) -> &str {
        &self.label
    }
}

impl std::fmt::Display for BasicBlock {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        let label = &self.label;
        writeln!(fmt, "{label}:")?;
        self.instrs
            .iter()
            .try_for_each(|i| writeln!(fmt, "    {i}"))
    }
}

impl Instr {
    pub fn value(self) -> IRValue {
        self.res
    }
}

impl std::fmt::Display for Instr {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        if let Some(name) = self.res.reg_name() {
            write!(fmt, "%{name} = ")?;
        }
        write!(fmt, "{}", &self.class)
    }
}

impl std::fmt::Display for InstrClass {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            InstrClass::Load(irtype, src) => write!(fmt, "load {irtype}, ptr {}", src.name()),
            InstrClass::Add(irtype, lhs, rhs) => {
                write!(fmt, "add {irtype} {}, {}", lhs.name(), rhs.name())
            }
            InstrClass::Sub(irtype, lhs, rhs) => {
                write!(fmt, "sub {irtype} {}, {}", lhs.name(), rhs.name())
            }
            InstrClass::Mul(irtype, lhs, rhs) => {
                write!(fmt, "mul {irtype} {}, {}", lhs.name(), rhs.name())
            }
            InstrClass::Div(irtype, lhs, rhs) => {
                write!(fmt, "sdiv {irtype} {}, {}", lhs.name(), rhs.name())
            }
            InstrClass::FAdd(irtype, lhs, rhs) => {
                write!(fmt, "fadd {irtype} {}, {}", lhs.name(), rhs.name())
            }
            InstrClass::FSub(irtype, lhs, rhs) => {
                write!(fmt, "fsub {irtype} {}, {}", lhs.name(), rhs.name())
            }
            InstrClass::FMul(irtype, lhs, rhs) => {
                write!(fmt, "fmul {irtype} {}, {}", lhs.name(), rhs.name())
            }
            InstrClass::FDiv(irtype, lhs, rhs) => {
                write!(fmt, "fdiv {irtype} {}, {}", lhs.name(), rhs.name())
            }
            InstrClass::Eq(irtype, lhs, rhs) => {
                write!(fmt, "icmp eq {irtype} {}, {}", lhs.name(), rhs.name())
            }
            InstrClass::Neq(irtype, lhs, rhs) => {
                write!(fmt, "icmp ne {irtype} {}, {}", lhs.name(), rhs.name())
            }
            InstrClass::Lte(irtype, lhs, rhs) => {
                write!(fmt, "icmp sle {irtype} {}, {}", lhs.name(), rhs.name())
            }
            InstrClass::Lt(irtype, lhs, rhs) => {
                write!(fmt, "icmp slt {irtype} {}, {}", lhs.name(), rhs.name())
            }
            InstrClass::Gte(irtype, lhs, rhs) => {
                write!(fmt, "icmp sge {irtype} {}, {}", lhs.name(), rhs.name())
            }
            InstrClass::Gt(irtype, lhs, rhs) => {
                write!(fmt, "icmp sgt {irtype} {}, {}", lhs.name(), rhs.name())
            }
            InstrClass::And(irtype, lhs, rhs) => {
                write!(fmt, "and {irtype} {}, {}", lhs.name(), rhs.name())
            }
            InstrClass::Store(src, dst) => write!(fmt, "store {src}, ptr {}", dst.name()),
            InstrClass::Call(fun_ptr, ret_typ, args, fast) => {
                // TODO: use other calling convention for fast call
                let cc = if *fast { "ccc" } else { "" };
                write!(fmt, "call {cc} {ret_typ} {}(", fun_ptr.name())?;
                write_comma_separated(args, fmt)?;
                write!(fmt, ")")
            }
            InstrClass::GetElemPtr(irtype, src, indexes) => {
                write!(fmt, "getelementptr {irtype}, {src}, ")?;
                write_comma_separated(indexes, fmt)
            }
            InstrClass::Return(val) => write!(fmt, "ret {val}"),
            InstrClass::Alloca(irtype) => write!(fmt, "alloca {irtype}"),
            InstrClass::CondBrk(irvalue, then_label, else_label) => {
                write!(
                    fmt,
                    "br {irvalue}, label %{then_label}, label %{else_label}"
                )
            }
            InstrClass::Brk(label) => write!(fmt, "br label %{label}"),
            InstrClass::Switch(value, default_label, cases) => {
                write!(fmt, "switch {value}, label %{default_label} [ ")?;
                cases
                    .iter()
                    .try_for_each(|(value, label)| write!(fmt, "{value}, label %{label} "))?;
                write!(fmt, "]")
            }
        }
    }
}

impl IRValue {
    pub fn typ(&self) -> IRType {
        match self {
            IRValue::Void => IRType::Void,
            IRValue::Pri(IRPri::I1(_)) => IRType::I1,
            IRValue::Pri(IRPri::I32(_)) => IRType::I32,
            IRValue::Pri(IRPri::I64(_)) => IRType::I64,
            IRValue::Pri(IRPri::Double(_)) => IRType::Double,
            IRValue::Pri(IRPri::Str(str)) => IRType::Array(Box::new(IRType::I8), str.len() + 1),
            IRValue::Reg(_, ir_type) => ir_type.clone(),
            IRValue::Global(_, ir_type) => ir_type.clone(),
        }
    }

    pub fn is_void(&self) -> bool {
        matches!(self, IRValue::Void)
    }

    fn reg_name(&self) -> Option<&String> {
        if let IRValue::Reg(name, _) = self {
            Some(name)
        } else {
            None
        }
    }

    fn name(&self) -> String {
        match self {
            IRValue::Reg(name, _) => format!("%{name}"),
            IRValue::Global(name, _) => format!("@{name}"),
            IRValue::Pri(IRPri::I1(true)) => "1".to_string(),
            IRValue::Pri(IRPri::I1(false)) => "0".to_string(),
            IRValue::Pri(IRPri::I32(val)) => val.to_string(),
            IRValue::Pri(IRPri::I64(val)) => val.to_string(),
            IRValue::Pri(IRPri::Double(val)) => val.to_string(),
            IRValue::Pri(IRPri::Str(val)) => format!("c\"{}\"", hex_string(val)),
            IRValue::Void => "void".to_string(),
        }
    }
}

impl std::fmt::Display for IRValue {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            IRValue::Void => write!(fmt, "void"),
            IRValue::Pri(irpri) => write!(fmt, "{irpri}"),
            IRValue::Reg(name, irtype) => write!(fmt, "{irtype} %{name}"),
            IRValue::Global(name, irtype) => write!(fmt, "{irtype} @{name}"),
        }
    }
}

impl IRType {
    pub fn is_void(&self) -> bool {
        matches!(self, IRType::Void)
    }
}

impl From<Rc<RefCell<Type>>> for IRType {
    fn from(typ: Rc<RefCell<Type>>) -> Self {
        (&normalize_typ(typ)).into()
    }
}

impl From<&Type> for IRType {
    fn from(typ: &Type) -> Self {
        match typ {
            Type::Fun(_) | Type::Tuple(_) | Type::Custom(_, _) => IRType::Ptr,
            Type::Primitive(Primitive::Integer) => IRType::I64,
            Type::Primitive(Primitive::Float) => IRType::Double,
            Type::Primitive(Primitive::Bool) | Type::Primitive(Primitive::Unit) => IRType::I1,
            Type::Variable(Variable::Unbound(_)) => {
                unreachable!("Type should be monomorphized in IR building phase")
            }
            Type::Variable(Variable::Link(_)) => {
                unreachable!("Type should be normalized to reach here")
            }
        }
    }
}

impl std::fmt::Display for IRType {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            IRType::Void => write!(fmt, "void"),
            IRType::I1 => write!(fmt, "i1"),
            IRType::I8 => write!(fmt, "i8"),
            IRType::I32 => write!(fmt, "i32"),
            IRType::I64 => write!(fmt, "i64"),
            IRType::Double => write!(fmt, "double"),
            IRType::Ptr => write!(fmt, "ptr"),
            IRType::Array(typ, sz) => write!(fmt, "[{sz} x {typ}]"),
            IRType::Struct(typs) => {
                write!(fmt, "{{")?;
                write_comma_separated(typs, fmt)?;
                write!(fmt, "}}")
            }
        }
    }
}

impl std::fmt::Display for IRPri {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            IRPri::I1(val) => write!(fmt, "i1 {val}"),
            IRPri::I32(val) => write!(fmt, "i32 {val}"),
            IRPri::I64(val) => write!(fmt, "i64 {val}"),
            IRPri::Double(val) if val.fract() == 0.0 => write!(fmt, "double {val}.0"),
            IRPri::Double(val) => write!(fmt, "double {val}"),
            IRPri::Str(val) => write!(fmt, "[i8 x {}] c\"{}\"", val.len() + 1, hex_string(val)),
        }
    }
}

fn write_comma_separated<T: std::fmt::Display>(
    items: &[T],
    fmt: &mut Formatter,
) -> Result<(), std::fmt::Error> {
    for item in items.iter().take(1) {
        write!(fmt, "{item}")?;
    }
    for item in items.iter().skip(1) {
        write!(fmt, ", {item}")?;
    }
    Ok(())
}

fn hex_string(str: &str) -> String {
    str.as_bytes()
        .iter()
        .map(|b| format!("\\{b:02X}"))
        .collect::<Vec<String>>()
        .join("")
        + "\\00"
}

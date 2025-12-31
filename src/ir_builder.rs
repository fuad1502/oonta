use std::collections::HashMap;

use crate::{
    ast::{Ast, BinOpExpr, Expr, FunExpr, LiteralExpr, VarExpr},
    ir_builder::ir::{FunSignature, Function, GlobalVar, IRPri, IRType, IRValue, Module},
    lexer::Lexer,
    typ::{Type, TypeMap, normalize_typ},
};

pub mod ir;

pub struct IRBuilder<'a> {
    type_map: &'a TypeMap,
    lexer: &'a Lexer,
    context: Option<Context>,
    module: Module,
    anon_fun_count: usize,
}

struct Context {
    ir_values: HashMap<String, IRValue>,
    fun_name: String,
    parent: Option<Box<Context>>,
}

impl<'a> IRBuilder<'a> {
    pub fn new(type_map: &'a TypeMap, lexer: &'a Lexer) -> Self {
        let ocaml = "ocaml".to_string();
        let main_function = Function::new(ocaml.clone(), IRType::Void, vec![]);
        let mut module = Module::default();
        module.new_function(ocaml.clone(), main_function);
        Self {
            type_map,
            lexer,
            context: Some(Context::new(ocaml)),
            module,
            anon_fun_count: 0,
        }
    }

    pub fn build(mut self, ast: &Ast) -> Module {
        for binding in &ast.binds {
            let name = self.lexer.str_from_span(&binding.name).to_string();
            let ir_typ = self.get_ir_typ(&*binding.expr as *const Expr);
            self.module.new_global_var(name.clone(), ir_typ.clone());
            let global_val = IRValue::Global(name.clone(), ir_typ);
            self.insert_name_to_ctx(name, global_val.clone());
            let expr_val = self.visit_expr(&binding.expr);
            self.curr_fun().store(expr_val, global_val);
        }
        self.curr_fun().ret(IRValue::Void);
        self.module
    }

    fn visit_expr(&mut self, expr: &Expr) -> IRValue {
        let expr_ptr = expr as *const Expr;
        match expr {
            Expr::Literal(literal_expr) => self.visit_literal_expr(literal_expr),
            Expr::Var(var_expr) => self.visit_var_expr(var_expr),
            Expr::Fun(fun_expr) => self.visit_fun_expr(fun_expr, expr_ptr),
            Expr::Application(application_expr) => todo!(),
            Expr::LetIn(let_in_expr) => todo!(),
            Expr::BinOp(bin_op_expr) => self.visit_bin_op_expr(bin_op_expr, expr_ptr),
        }
    }

    fn visit_fun_expr(&mut self, fun_expr: &FunExpr, expr_ptr: *const Expr) -> IRValue {
        // 1. Create function
        let fun_name = self.new_anon_fun_name();
        let param_names: Vec<String> = fun_expr
            .params
            .iter()
            .map(|p| self.lexer.str_from_span(p).to_string())
            .collect();
        let typ = self.get_typ(expr_ptr);
        let mut fun = Function::from_typ(fun_name.clone(), param_names.clone(), typ);
        fun.add_param(("env".to_string(), IRType::Ptr));

        // > add function to module
        self.module.new_function(fun_name.clone(), fun);

        // 2. Populate context

        // > insert parameters to context
        self.push_ctx(fun_name.clone());
        for (i, name) in param_names.into_iter().enumerate() {
            let param = self.curr_fun().param(i);
            self.insert_name_to_ctx(name, param);
        }

        // > insert captures to context
        let num_of_params = self.curr_fun().num_of_params();
        let env_ptr = self.curr_fun().param(num_of_params - 1);
        let env_values: Vec<IRValue> = fun_expr
            .captures
            .iter()
            .map(|e| self.get_value_from_ctx(e))
            .collect();
        let env_typs: Vec<IRType> = env_values.iter().map(|v| v.typ()).cloned().collect();
        let env_typ = IRType::Struct(env_typs.clone());
        for (name, typ) in fun_expr.captures.iter().zip(env_typs) {
            let ptr = self
                .curr_fun()
                .getelemptr(env_typ.clone(), env_ptr.clone(), &[0, 0]);
            let val = self.curr_fun().load(typ, ptr);
            self.insert_name_to_ctx(name.to_string(), val);
        }

        // 3. Create function body
        let value = self.visit_expr(&fun_expr.body);
        self.curr_fun().ret(value);

        // 4. Create closure
        self.pop_ctx();
        let closure_ptr = self.malloc(4 * (1 + fun_expr.captures.len()));
        let closure_typ = IRType::Struct(vec![IRType::Ptr, env_typ]);

        // > store anon function ptr
        let ptr = self
            .curr_fun()
            .getelemptr(closure_typ.clone(), closure_ptr.clone(), &[0, 0]);
        self.curr_fun()
            .store(IRValue::Global(fun_name, IRType::Ptr), ptr);

        // > store env values
        for (i, value) in env_values.into_iter().enumerate() {
            let value = if let IRValue::Global(_, typ) = &value {
                self.curr_fun().load(typ.clone(), value)
            } else {
                value
            };
            let ptr = self.curr_fun().getelemptr(
                closure_typ.clone(),
                closure_ptr.clone(),
                &[0, 1, i as i32],
            );
            self.curr_fun().store(value, ptr);
        }

        closure_ptr
    }

    fn visit_var_expr(&mut self, var_expr: &VarExpr) -> IRValue {
        let name = self.lexer.str_from_span(&var_expr.id);
        let val = self.get_value_from_ctx(name);
        if let IRValue::Global(_, typ) = &val {
            self.curr_fun().load(typ.clone(), val)
        } else {
            val
        }
    }

    fn visit_bin_op_expr(&mut self, bin_op_expr: &BinOpExpr, expr_ptr: *const Expr) -> IRValue {
        let lhs = self.visit_expr(&bin_op_expr.lhs);
        let rhs = self.visit_expr(&bin_op_expr.rhs);
        let typ = self.get_ir_typ(expr_ptr);
        self.curr_fun().add(typ, lhs, rhs)
    }

    fn visit_literal_expr(&mut self, literal_expr: &LiteralExpr) -> IRValue {
        match literal_expr {
            LiteralExpr::Integer(value, _) => IRValue::Pri(IRPri::I32(*value)),
        }
    }

    fn get_ir_typ(&self, expr_ptr: *const Expr) -> IRType {
        IRType::from(self.get_typ(expr_ptr))
    }

    fn get_typ(&self, expr_ptr: *const Expr) -> Type {
        self.type_map
            .get(expr_ptr)
            .map(normalize_typ)
            .expect("Expr not in type_map")
    }

    fn curr_fun(&mut self) -> &mut Function {
        if let Some(context) = &self.context {
            self.module
                .get_function(&context.fun_name)
                .expect("context function not in module")
        } else {
            panic!()
        }
    }

    fn insert_name_to_ctx(&mut self, name: String, ir_value: IRValue) {
        if let Some(context) = &mut self.context {
            context.insert(name, ir_value);
        } else {
            panic!("context unassigned")
        }
    }

    fn get_value_from_ctx(&self, name: &str) -> IRValue {
        if let Some(context) = &self.context {
            context.get(name).expect("name not in context").clone()
        } else {
            panic!("context unassigned")
        }
    }

    fn push_ctx(&mut self, function_name: String) {
        let mut new_ctx = Context::new(function_name);
        new_ctx.parent = self.context.take().map(Box::new);
        self.context = Some(new_ctx);
    }

    fn pop_ctx(&mut self) {
        let parent = self.context.take().map(|c| c.parent);
        if let Some(Some(parent)) = parent {
            self.context = Some(*parent);
        }
    }

    fn new_anon_fun_name(&mut self) -> String {
        let name = format!("anon_{}", self.anon_fun_count);
        self.anon_fun_count += 1;
        name
    }

    fn malloc(&mut self, sz: usize) -> IRValue {
        let sz = IRValue::Pri(IRPri::I64(sz as i64));
        let ret_typ = match self.module.get_function_decl("malloc") {
            Some(malloc) => malloc.ret_typ().clone(),
            None => {
                let malloc = "malloc".to_string();
                let ret_typ = IRType::Ptr;
                let params = vec![IRType::I64];
                let signature = FunSignature::new(malloc.clone(), ret_typ.clone(), params);
                self.module.new_function_decl(malloc, signature);
                ret_typ
            }
        };
        let fun_ptr = IRValue::Global("malloc".to_string(), IRType::Ptr);
        self.curr_fun().call(fun_ptr, ret_typ, vec![sz])
    }
}

impl Context {
    fn new(function_name: String) -> Self {
        Self {
            ir_values: HashMap::new(),
            parent: None,
            fun_name: function_name,
        }
    }

    fn insert(&mut self, name: String, ir_value: IRValue) {
        self.ir_values.insert(name, ir_value);
    }

    fn get(&self, name: &str) -> Option<&IRValue> {
        match self.ir_values.get(name) {
            Some(val) => Some(val),
            None => match &self.parent {
                Some(parent) => parent.get(name),
                None => None,
            },
        }
    }
}

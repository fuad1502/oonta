use core::convert::From;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{
        ApplicationExpr, Ast, BinOpExpr, CondExpr, Expr, FunExpr, LetInExpr, LiteralExpr, Operator,
        Pattern, PatternMatchExpr, TupleExpr, VarExpr,
    },
    ir_builder::ir::{FunSignature, Function, IRPri, IRType, IRValue, Module},
    lexer::Lexer,
    typ::{Type, TypeMap, extract_fun_typs, extract_tuple_typs, normalize_typ},
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
    pub fn new(type_map: &'a TypeMap, lexer: &'a Lexer, is_top_level: bool) -> Self {
        let main_fun_name = if is_top_level {
            "main".to_string()
        } else {
            "caml_main".to_string()
        };
        let main_function = Function::new(main_fun_name.clone(), IRType::Void, vec![]);
        let mut module = Module::default();
        module.new_function(main_fun_name.clone(), main_function);
        let builder = Self {
            type_map,
            lexer,
            context: Some(Context::new(main_fun_name)),
            module,
            anon_fun_count: 0,
        };
        builder.populate_builtins()
    }

    pub fn build(mut self, ast: &Ast) -> Module {
        for binding in &ast.binds {
            let expr_val = self.visit_expr(&binding.expr.borrow());
            let ir_typ = expr_val.typ().clone();

            // TODO: better void handling
            match (&binding.name, ir_typ.is_void()) {
                (Some(name), false) => {
                    let name = self.lexer.str_from_span(name).to_string();
                    self.module
                        .new_global_var(name.clone(), ir_typ.clone(), None);
                    let global_val = IRValue::Global(name.clone(), ir_typ);
                    self.insert_name_to_ctx(name, global_val.clone());
                    self.curr_fun().store(expr_val, global_val);
                }
                (Some(name), true) => {
                    let name = self.lexer.str_from_span(name).to_string();
                    self.insert_name_to_ctx(name, IRValue::Void);
                }
                (None, _) => (),
            }
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
            Expr::Tuple(tuple_expr) => self.visit_tuple_expr(tuple_expr),
            Expr::Application(application_expr) => {
                self.visit_application_expr(application_expr, expr_ptr)
            }
            Expr::LetIn(let_in_expr) => self.visit_let_in_expr(let_in_expr),
            Expr::BinOp(bin_op_expr) => self.visit_bin_op_expr(bin_op_expr, expr_ptr),
            Expr::Conditional(cond_expr) => self.visit_cond_expr(cond_expr, expr_ptr),
            Expr::PatternMatch(pattern_match_expr) => {
                self.visit_patt_mat_expr(pattern_match_expr, expr_ptr)
            }
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
        self.push_ctx(fun_name.clone());

        // > insert parameters to context
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
        let env_typs: Vec<IRType> = env_values.iter().map(|v| v.typ()).collect();
        let closure_typ = if env_typs.is_empty() {
            IRType::Struct(vec![IRType::Ptr])
        } else {
            let env_typ = IRType::Struct(env_typs.clone());
            IRType::Struct(vec![IRType::Ptr, env_typ])
        };
        for (i, (name, typ)) in fun_expr.captures.iter().zip(env_typs).enumerate() {
            let ptr =
                self.curr_fun()
                    .getelemptr(closure_typ.clone(), env_ptr.clone(), &[0, 1, i as i32]);
            let val = self.curr_fun().load(typ, ptr);
            self.insert_name_to_ctx(name.to_string(), val);
        }

        // > insert recursive name to context
        if let Some(name) = &fun_expr.recursive_bind {
            self.insert_name_to_ctx(name.to_string(), env_ptr);
        }

        // 3. Create function body
        let value = self.visit_expr(&fun_expr.body.borrow());
        self.curr_fun().ret(value);

        // 4. Create closure
        self.pop_ctx();
        let closure_ptr = self.malloc(8 * (1 + fun_expr.captures.len()));

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

    fn visit_application_expr(
        &mut self,
        application_expr: &ApplicationExpr,
        expr_ptr: *const Expr,
    ) -> IRValue {
        let mut fun_typs = {
            let fun_expr_ptr = &*application_expr.fun.borrow() as *const Expr;
            let fun_typ = normalize_typ(self.type_map.get(fun_expr_ptr).unwrap());
            extract_fun_typs(fun_typ).unwrap()
        };

        let num_of_remainding_args = fun_typs.len() - 1 - application_expr.binds.len();
        if num_of_remainding_args > 0 {
            let fun_typs = fun_typs.split_off(application_expr.binds.len());
            let ret_typ = normalize_typ(fun_typs.last().unwrap().clone());
            return self.visit_partial_application_expr(
                application_expr,
                num_of_remainding_args,
                fun_typs,
                ret_typ,
            );
        }

        let mut args = application_expr
            .binds
            .iter()
            .map(|e| self.visit_expr(&e.borrow()))
            .collect::<Vec<IRValue>>();
        let closure = self.visit_expr(&application_expr.fun.borrow());
        args.push(closure.clone());
        let fun = self.curr_fun().load(IRType::Ptr, closure);
        let res_typ = self.get_ir_typ(expr_ptr);
        self.curr_fun().call(fun, res_typ, args)
    }

    fn visit_partial_application_expr(
        &mut self,
        application_expr: &ApplicationExpr,
        num_of_remainding_args: usize,
        dispatch_fun_typs: Vec<Rc<RefCell<Type>>>,
        dispatch_ret_typ: Type,
    ) -> IRValue {
        // 1. Create function
        let dispatch_fun_name = self.new_anon_fun_name();
        let dispath_param_names = vec![String::new(); num_of_remainding_args];
        let mut dispatch_fun = Function::from_typ(
            dispatch_fun_name.clone(),
            dispath_param_names.clone(),
            Type::Fun(dispatch_fun_typs),
        );
        dispatch_fun.add_param(("env".to_string(), IRType::Ptr));

        // > add function to module
        self.module
            .new_function(dispatch_fun_name.clone(), dispatch_fun);

        // 2. Create dispath closure
        let closure = self.visit_expr(&application_expr.fun.borrow());
        let args: Vec<IRValue> = application_expr
            .binds
            .iter()
            .map(|e| self.visit_expr(&e.borrow()))
            .collect();
        let arg_typs: Vec<IRType> = args.iter().map(|v| v.typ()).collect();
        let mut env_typs = vec![closure.typ().clone()];
        env_typs.extend(arg_typs.clone());
        let dispath_closure_typ =
            IRType::Struct(vec![IRType::Ptr, IRType::Struct(env_typs.clone())]);
        let dispath_closure_ptr = self.malloc(8 * (1 + env_typs.len()));

        // > store anon function ptr
        let ptr = self.curr_fun().getelemptr(
            dispath_closure_typ.clone(),
            dispath_closure_ptr.clone(),
            &[0, 0],
        );
        self.curr_fun()
            .store(IRValue::Global(dispatch_fun_name.clone(), IRType::Ptr), ptr);

        // > store env values (fun)
        let ptr = self.curr_fun().getelemptr(
            dispath_closure_typ.clone(),
            dispath_closure_ptr.clone(),
            &[0, 1, 0],
        );
        self.curr_fun().store(closure, ptr);

        // > store env values (args)
        for (i, value) in args.into_iter().enumerate() {
            let ptr = self.curr_fun().getelemptr(
                dispath_closure_typ.clone(),
                dispath_closure_ptr.clone(),
                &[0, 1, (i + 1) as i32],
            );
            self.curr_fun().store(value, ptr);
        }

        // 3. Create function body
        self.push_ctx(dispatch_fun_name);
        let num_of_params = self.curr_fun().num_of_params();
        let env = self.curr_fun().param(num_of_params - 1);

        // > grab fun
        let ptr = self
            .curr_fun()
            .getelemptr(dispath_closure_typ.clone(), env.clone(), &[0, 1, 0]);
        let closure = self.curr_fun().load(IRType::Ptr, ptr);

        // > grab args
        let mut args: Vec<IRValue> = arg_typs
            .into_iter()
            .enumerate()
            .map(|(i, typ)| {
                let ptr = self.curr_fun().getelemptr(
                    dispath_closure_typ.clone(),
                    env.clone(),
                    &[0, 1, (i + 1) as i32],
                );
                self.curr_fun().load(typ, ptr)
            })
            .collect();
        let remainding_args = (0..num_of_remainding_args).map(|i| self.curr_fun().param(i));
        args.extend(remainding_args);
        args.push(closure.clone());

        // > call fun with args
        let fun = self.curr_fun().load(IRType::Ptr, closure);
        let res_typ = IRType::from(&dispatch_ret_typ);
        let res = self.curr_fun().call(fun, res_typ, args);
        self.curr_fun().ret(res);

        self.pop_ctx();

        dispath_closure_ptr
    }

    fn visit_tuple_expr(&mut self, tuple_expr: &TupleExpr) -> IRValue {
        let tuple_ptr = self.malloc(8 * tuple_expr.elements.len());
        let values: Vec<IRValue> = tuple_expr
            .elements
            .iter()
            .map(|expr| self.visit_expr(&expr.borrow()))
            .collect();
        let typs: Vec<IRType> = values.iter().map(|val| val.typ()).collect();
        let tuple_typ = IRType::Struct(typs);
        values.into_iter().enumerate().for_each(|(i, val)| {
            let ptr =
                self.curr_fun()
                    .getelemptr(tuple_typ.clone(), tuple_ptr.clone(), &[0, i as i32]);
            self.curr_fun().store(val, ptr);
        });
        tuple_ptr
    }

    fn visit_cond_expr(&mut self, cond_expr: &CondExpr, expr_ptr: *const Expr) -> IRValue {
        let cond_val = self.visit_expr(&cond_expr.cond.borrow());
        let typ = self.get_ir_typ(expr_ptr);
        let res_ptr = self.curr_fun().alloca(typ.clone());
        let then_label = self.curr_fun().add_new_bb("then");
        let else_label = self.curr_fun().add_new_bb("else");
        let follow_label = self.curr_fun().add_new_bb("follow");
        self.curr_fun()
            .cond_brk(cond_val, then_label.clone(), else_label.clone());
        self.curr_fun().set_bb(then_label);
        let val = self.visit_expr(&cond_expr.yes.borrow());
        self.curr_fun().store(val, res_ptr.clone());
        self.curr_fun().brk(follow_label.clone());
        self.curr_fun().set_bb(else_label);
        let val = self.visit_expr(&cond_expr.no.borrow());
        self.curr_fun().store(val, res_ptr.clone());
        self.curr_fun().brk(follow_label.clone());
        self.curr_fun().set_bb(follow_label);
        self.curr_fun().load(typ, res_ptr)
    }

    fn visit_patt_mat_expr(
        &mut self,
        patt_mat_expr: &PatternMatchExpr,
        expr_ptr: *const Expr,
    ) -> IRValue {
        // TODO: Handle case where none of the branch are hit

        // 1. Prepare return location
        let typ = self.get_ir_typ(expr_ptr);
        let res_ptr = self.curr_fun().alloca(typ.clone());

        // 2. Visit matched expression
        let mat_val = self.visit_expr(&patt_mat_expr.matched.borrow());
        let mat_expr_ptr = &*patt_mat_expr.matched.borrow() as *const Expr;
        let mat_typ = self.get_typ(mat_expr_ptr);

        // 3. Prepare exit block
        let exit_bb = self.curr_fun().create_bb("exit");
        let exit_label = exit_bb.label().to_string();

        // 4. Visit branches
        for (patt, expr) in &patt_mat_expr.branches {
            let conds = self.gather_conds(patt, mat_typ.clone(), mat_val.clone());
            if !conds.is_empty() {
                // > Create breakage
                let cond = self.conjunction(conds);
                let then_label = self.curr_fun().add_new_bb("then");
                let follow_label = self.curr_fun().add_new_bb("follow");
                self.curr_fun()
                    .cond_brk(cond, then_label.clone(), follow_label.clone());
                // > Visit branch
                self.curr_fun().set_bb(then_label);
                let binds = self.gather_binds(patt, mat_typ.clone(), mat_val.clone());
                self.visit_branch_expr(binds, &expr.borrow(), res_ptr.clone(), exit_label.clone());
                self.curr_fun().set_bb(follow_label);
            } else {
                // > Visit branch
                let binds = self.gather_binds(patt, mat_typ.clone(), mat_val.clone());
                self.visit_branch_expr(binds, &expr.borrow(), res_ptr.clone(), exit_label.clone());
            }
        }

        // 5. Load result
        self.curr_fun().add_bb(exit_bb);
        self.curr_fun().set_bb(exit_label);
        self.curr_fun().load(typ, res_ptr)
    }

    fn visit_branch_expr(
        &mut self,
        bindings: Vec<(String, IRValue)>,
        expr: &Expr,
        store_ptr: IRValue,
        exit_label: String,
    ) {
        let fun_name = self.curr_fun().name().to_string();
        self.push_ctx(fun_name);
        bindings
            .into_iter()
            .for_each(|(name, val)| self.insert_name_to_ctx(name, val));
        let val = self.visit_expr(expr);
        self.curr_fun().store(val, store_ptr.clone());
        self.curr_fun().brk(exit_label);
        self.pop_ctx();
    }

    fn visit_let_in_expr(&mut self, let_in_expr: &LetInExpr) -> IRValue {
        let fun_name = self.curr_fun().name().to_string();
        self.push_ctx(fun_name);
        let bind_val = self.visit_expr(&let_in_expr.bind.1.borrow());
        let bind_name = self.lexer.str_from_span(&let_in_expr.bind.0).to_string();
        self.insert_name_to_ctx(bind_name, bind_val);
        let val = self.visit_expr(&let_in_expr.expr.borrow());
        self.pop_ctx();
        val
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
        let lhs = self.visit_expr(&bin_op_expr.lhs.borrow());
        let rhs = self.visit_expr(&bin_op_expr.rhs.borrow());
        let typ = self.get_ir_typ(expr_ptr);
        self.curr_fun().binop(typ, lhs, rhs, bin_op_expr.op)
    }

    fn visit_literal_expr(&mut self, literal_expr: &LiteralExpr) -> IRValue {
        match literal_expr {
            LiteralExpr::Integer(value, _) => IRValue::Pri(IRPri::I64(*value)),
            LiteralExpr::Unit(_) => IRValue::Void,
        }
    }

    fn conjunction(&mut self, mut conditions: Vec<IRValue>) -> IRValue {
        let mut value = match (conditions.pop(), conditions.pop()) {
            (Some(value), None) => value,
            (Some(val_b), Some(val_a)) => self.curr_fun().and(val_a, val_b),
            (None, _) => IRValue::Pri(IRPri::I1(true)),
        };
        for condition in conditions {
            value = self.curr_fun().and(value, condition);
        }
        value
    }

    fn gather_conds(&mut self, pattern: &Pattern, typ: Type, value: IRValue) -> Vec<IRValue> {
        // TODO: Refactor
        match pattern {
            Pattern::Tuple(elements) => {
                let mut conditions = vec![];
                let element_typs: Vec<Type> = extract_tuple_typs(typ)
                    .unwrap()
                    .into_iter()
                    .map(normalize_typ)
                    .collect();
                let element_ir_typs = element_typs.iter().map(IRType::from).collect();
                let pattern_type = IRType::Struct(element_ir_typs);
                for (i, (element, element_typ)) in elements.iter().zip(element_typs).enumerate() {
                    if !element.has_literal() {
                        continue;
                    }
                    let ptr = self.curr_fun().getelemptr(
                        pattern_type.clone(),
                        value.clone(),
                        &[0, i as i32],
                    );
                    let element_type = IRType::from(&element_typ);
                    let element_value = self.curr_fun().load(element_type, ptr);
                    let mut new_conditions = self.gather_conds(element, element_typ, element_value);
                    conditions.append(&mut new_conditions);
                }
                conditions
            }
            Pattern::Literal(literal_expr) => {
                let literal_value = self.visit_literal_expr(literal_expr);
                let conditional_value =
                    self.curr_fun()
                        .binop(IRType::I1, literal_value, value, Operator::Eq);
                vec![conditional_value]
            }
            Pattern::Identifier(_) | Pattern::None => vec![],
        }
    }

    fn gather_binds(
        &mut self,
        pattern: &Pattern,
        typ: Type,
        value: IRValue,
    ) -> Vec<(String, IRValue)> {
        // TODO: Refactor
        match pattern {
            Pattern::Tuple(elements) => {
                let mut bindings = vec![];
                let element_typs: Vec<Type> = extract_tuple_typs(typ)
                    .unwrap()
                    .into_iter()
                    .map(normalize_typ)
                    .collect();
                let element_ir_typs = element_typs.iter().map(IRType::from).collect();
                let pattern_type = IRType::Struct(element_ir_typs);
                for (i, (element, element_typ)) in elements.iter().zip(element_typs).enumerate() {
                    if !element.has_identifier() {
                        continue;
                    }
                    let ptr = self.curr_fun().getelemptr(
                        pattern_type.clone(),
                        value.clone(),
                        &[0, i as i32],
                    );
                    let element_type = IRType::from(&element_typ);
                    let element_value = self.curr_fun().load(element_type, ptr);
                    let mut new_bindings = self.gather_binds(element, element_typ, element_value);
                    bindings.append(&mut new_bindings);
                }
                bindings
            }
            Pattern::Identifier(span) => {
                let name = self.lexer.str_from_span(span);
                vec![(name.to_string(), value)]
            }
            Pattern::Literal(_) | Pattern::None => vec![],
        }
    }

    fn populate_builtins(mut self) -> Self {
        // 1. Insert format strings
        let fmt_str_name = "fmt".to_string();
        let typ = IRType::Array(Box::new(IRType::I8), 4);
        let init = IRValue::Pri(IRPri::Str("%d\n"));
        self.module
            .new_global_constant(fmt_str_name.clone(), typ, Some(init));

        // 2. Insert printf declaration
        let printf_fun_name = "printf".to_string();
        let ret_typ = IRType::I32;
        let params = vec![IRType::Ptr];
        let signature = FunSignature::new(printf_fun_name.clone(), ret_typ, params, true);
        self.module
            .new_function_decl(printf_fun_name.clone(), signature);

        // 3. Define print_int function
        let anon_fun_name = self.new_anon_fun_name();
        let ret_typ = IRType::Void;
        let params = vec![(String::new(), IRType::I32)];
        let mut fun = Function::new(anon_fun_name.clone(), ret_typ, params);
        let printf_fun_ptr = IRValue::Global(printf_fun_name, IRType::Ptr);
        let fmt_str_ptr = IRValue::Global(fmt_str_name, IRType::Ptr);
        let printf_args = vec![fmt_str_ptr, fun.param(0)];
        fun.call(printf_fun_ptr, IRType::I32, printf_args);
        fun.ret(IRValue::Void);
        self.module.new_function(anon_fun_name.clone(), fun);

        // 4. Insert closure
        let closure_name = "_print_int".to_string();
        let typ = IRType::Ptr;
        let init = IRValue::Global(anon_fun_name, IRType::Ptr);
        self.module
            .new_global_constant(closure_name.clone(), typ, Some(init));

        // 5. Insert global var
        let print_int_name = "print_int".to_string();
        let typ = IRType::Ptr;
        let init = IRValue::Global(closure_name, IRType::Ptr);
        self.module
            .new_global_constant(print_int_name.clone(), typ, Some(init));
        self.insert_name_to_ctx(
            print_int_name.clone(),
            IRValue::Global(print_int_name, IRType::Ptr),
        );

        self
    }

    fn get_ir_typ(&self, expr_ptr: *const Expr) -> IRType {
        IRType::from(&self.get_typ(expr_ptr))
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
                let name = "malloc".to_string();
                let ret_typ = IRType::Ptr;
                let params = vec![IRType::I64];
                let signature = FunSignature::new(name.clone(), ret_typ.clone(), params, false);
                self.module.new_function_decl(name, signature);
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

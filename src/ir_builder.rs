use core::convert::From;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    ast::{
        ApplicationExpr, Ast, BinOpExpr, Bind, CondExpr, ConstructExpr, Expr, FunExpr, LetInExpr,
        LiteralExpr, Operator, Pattern, PatternMatchExpr, TupleExpr, VarExpr,
    },
    custom_types::CustomTypes,
    ir_builder::ir::{FunSignature, Function, IRPri, IRType, IRValue, Module},
    lexer::Lexer,
    monomorphization_pass::MonoExprs,
    typ::{
        Primitive, Type, TypeMap, extract_fun_typs, extract_tuple_typs, extract_variant_args,
        is_polymorphic, link_unbounds, normalize_typ,
    },
};

pub mod ir;

pub struct IRBuilder<'a> {
    type_map: &'a TypeMap,
    custom_types: &'a CustomTypes,
    lexer: &'a Lexer,
    context: Option<Context>,
    module: Module,
    bind_name: Option<String>,
}

struct Context {
    ir_values: HashMap<String, IRValue>,
    fun_name: String,
    recursive_bind: Option<String>,
    parent: Option<Box<Context>>,
}

impl<'a> IRBuilder<'a> {
    pub fn new(
        type_map: &'a TypeMap,
        custom_types: &'a CustomTypes,
        lexer: &'a Lexer,
        is_top_level: bool,
    ) -> Self {
        let main_fun_name = if is_top_level {
            "main".to_string()
        } else {
            "oonta_main".to_string()
        };
        let main_function = Function::new(main_fun_name.clone(), IRType::Void, vec![]);
        let mut module = Module::default();
        let main_fun_name = module.new_function(main_function);
        let builder = Self {
            type_map,
            custom_types,
            lexer,
            context: Some(Context::new(main_fun_name, None)),
            module,
            bind_name: None,
        };
        builder.populate_builtins()
    }

    pub fn build(mut self, ast: &Ast, mono_exprs: &MonoExprs) -> Module {
        self.visit_mono_exprs(mono_exprs);
        self.visit_bindings(ast);
        self.curr_fun().ret(IRValue::Void);
        self.module
    }

    fn visit_mono_exprs(&mut self, mono_exprs: &MonoExprs) {
        for (name, expr) in mono_exprs.binds.iter().rev() {
            let name = Some(name.clone());
            self.visit_bind(name, &expr.borrow());
        }
    }

    fn visit_bindings(&mut self, ast: &Ast) {
        for binding in &ast.binds {
            if self.is_polymorphic(binding) {
                continue;
            }
            let name = binding
                .name
                .clone()
                .map(|span| self.lexer.str_from_span(&span).to_string());
            self.visit_bind(name, &binding.expr.borrow());
        }
    }

    fn visit_bind(&mut self, name: Option<String>, expr: &Expr) {
        self.bind_name = name.clone();
        let expr_val = self.visit_expr(expr);
        let ir_typ = expr_val.typ().clone();

        // TODO: better void handling
        match (name, ir_typ.is_void()) {
            (Some(name), false) => {
                let glb_name = Self::glb_name(&name);
                let glb_name = self.module.new_global_var(&glb_name, ir_typ.clone(), None);
                let glb_val = IRValue::Global(glb_name.clone(), ir_typ);
                self.insert_name_to_ctx(name, glb_val.clone());
                self.curr_fun().store(expr_val, glb_val);
            }
            (Some(name), true) => {
                self.insert_name_to_ctx(name, IRValue::Void);
            }
            (None, _) => (),
        }
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
            Expr::Construction(construct_expr) => self.visit_construct_expr(construct_expr),
        }
    }

    fn visit_fun_expr(&mut self, fun_expr: &FunExpr, expr_ptr: *const Expr) -> IRValue {
        // 1. Create function
        let fun_name = self.create_fun_name();
        let param_names: Vec<String> = fun_expr
            .params
            .iter()
            .map(|p| self.lexer.str_from_span(p).to_string())
            .collect();
        let typ = self.get_typ(expr_ptr);
        let mut fun = Function::from_typ(fun_name.clone(), param_names.clone(), typ);
        fun.add_param(("env".to_string(), IRType::Ptr));

        // > add function to module
        let fun_name = self.module.new_function(fun);

        // 2. Populate context
        self.push_ctx(fun_name.clone(), fun_expr.recursive_bind.clone());

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
            .map(|e| self.get_value_from_ctx(e).unwrap())
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
            let fun_typ = self.type_map.get(fun_expr_ptr).unwrap();
            extract_fun_typs(fun_typ).unwrap()
        };

        let num_of_remainding_args = fun_typs.len() - 1 - application_expr.binds.len();
        if num_of_remainding_args > 0 {
            let fun_typs = fun_typs.split_off(application_expr.binds.len());
            let ret_typ = fun_typs.last().unwrap().clone();
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
            .filter(|val| !val.is_void())
            .collect::<Vec<IRValue>>();
        let closure = self.visit_expr(&application_expr.fun.borrow());
        args.push(closure.clone());
        let fun = if let Expr::Var(VarExpr { id, .. }) = &*application_expr.fun.borrow()
            && let Some(recursive_name) = self.get_ctx_recursive_bind()
            && self.lexer.str_from_span(id) == recursive_name
        {
            IRValue::Global(self.curr_fun().name().to_string(), IRType::Ptr)
        } else {
            self.curr_fun().load(IRType::Ptr, closure)
        };
        let res_typ = self.get_ir_typ(expr_ptr);
        self.curr_fun().fast_call(fun, res_typ, args)
    }

    fn visit_partial_application_expr(
        &mut self,
        application_expr: &ApplicationExpr,
        num_of_remainding_args: usize,
        dispatch_fun_typs: Vec<Rc<RefCell<Type>>>,
        dispatch_ret_typ: Rc<RefCell<Type>>,
    ) -> IRValue {
        // 1. Create function
        let dispatch_fun_name = self.create_fun_name();
        let dispath_param_names = vec!["p".to_string(); num_of_remainding_args];
        let mut dispatch_fun = Function::from_typ(
            dispatch_fun_name.clone(),
            dispath_param_names.clone(),
            Rc::new(RefCell::new(Type::Fun(dispatch_fun_typs))),
        );
        dispatch_fun.add_param(("env".to_string(), IRType::Ptr));

        // > add function to module
        let dispatch_fun_name = self.module.new_function(dispatch_fun);

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
        self.push_ctx(dispatch_fun_name, None);
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
        let res_typ = IRType::from(dispatch_ret_typ);
        let res = self.curr_fun().fast_call(fun, res_typ, args);
        self.curr_fun().ret(res);

        self.pop_ctx();

        dispath_closure_ptr
    }

    fn visit_construct_expr(&mut self, construct_expr: &ConstructExpr) -> IRValue {
        let cons_name = self.lexer.str_from_span(&construct_expr.cons);
        let tag = self.custom_types.get_constructor_idx(cons_name);
        let tag = IRValue::Pri(IRPri::I64(tag as i64));
        let variant_ptr = self.malloc(8 * 2);
        let variant_typ = IRType::Struct(vec![IRType::I64, IRType::Ptr]);
        let ptr = self
            .curr_fun()
            .getelemptr(variant_typ.clone(), variant_ptr.clone(), &[0, 0]);
        self.curr_fun().store(tag, ptr);
        if let Some(arg) = &construct_expr.arg {
            let ptr = self
                .curr_fun()
                .getelemptr(variant_typ, variant_ptr.clone(), &[0, 1]);
            let value = self.visit_expr(&arg.borrow());
            self.curr_fun().store(value, ptr);
        }
        variant_ptr
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
        for (i, (patt, expr)) in patt_mat_expr.branches.iter().enumerate() {
            let conds = self.gather_conds(patt, mat_typ.clone(), mat_val.clone());
            if !conds.is_empty() {
                let is_last_branch = i == patt_mat_expr.branches.len() - 1;
                // > Create breakage
                let cond = self.conjunction(conds);
                let then_label = self.curr_fun().add_new_bb("then");
                let follow_label = if is_last_branch {
                    exit_label.clone()
                } else {
                    self.curr_fun().add_new_bb("follow")
                };
                self.curr_fun()
                    .cond_brk(cond, then_label.clone(), follow_label.clone());
                // > Visit branch
                self.curr_fun().set_bb(then_label);
                let binds = self.gather_binds(patt, mat_typ.clone(), mat_val.clone());
                self.visit_branch_expr(binds, &expr.borrow(), res_ptr.clone(), exit_label.clone());
                if !is_last_branch {
                    self.curr_fun().set_bb(follow_label);
                }
            } else {
                // > Visit branch
                let binds = self.gather_binds(patt, mat_typ.clone(), mat_val.clone());
                self.visit_branch_expr(binds, &expr.borrow(), res_ptr.clone(), exit_label.clone());
                break;
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
        self.dup_ctx();
        bindings
            .into_iter()
            .for_each(|(name, val)| self.insert_name_to_ctx(name, val));
        let val = self.visit_expr(expr);
        self.curr_fun().store(val, store_ptr.clone());
        self.curr_fun().brk(exit_label);
        self.pop_ctx();
    }

    fn visit_let_in_expr(&mut self, let_in_expr: &LetInExpr) -> IRValue {
        self.dup_ctx();
        let bind_val = self.visit_expr(&let_in_expr.bind.1.borrow());
        let bind_name = self.lexer.str_from_span(&let_in_expr.bind.0).to_string();
        self.insert_name_to_ctx(bind_name, bind_val);
        let val = self.visit_expr(&let_in_expr.expr.borrow());
        self.pop_ctx();
        val
    }

    fn visit_var_expr(&mut self, var_expr: &VarExpr) -> IRValue {
        let mono_name = var_expr.mono_name(self.lexer);
        let val = self.get_value_from_ctx(&mono_name).unwrap();
        if let IRValue::Global(_, typ) = &val {
            self.curr_fun().load(typ.clone(), val)
        } else {
            val
        }
    }

    fn visit_bin_op_expr(&mut self, bin_op_expr: &BinOpExpr, expr_ptr: *const Expr) -> IRValue {
        let lhs = self.visit_expr(&bin_op_expr.lhs.borrow());
        let rhs = self.visit_expr(&bin_op_expr.rhs.borrow());
        match bin_op_expr.op {
            Operator::Plus | Operator::Minus | Operator::Star | Operator::Slash => {
                let typ = self.get_ir_typ(expr_ptr);
                self.curr_fun().binop(typ, lhs, rhs, bin_op_expr.op)
            }
            Operator::Eq
            | Operator::Neq
            | Operator::Lte
            | Operator::Lt
            | Operator::Gte
            | Operator::Gt => {
                let operand_typ = {
                    let expr_ptr = &*bin_op_expr.lhs.borrow() as *const Expr;
                    normalize_typ(self.get_typ(expr_ptr))
                };
                self.handle_comparison_operation(lhs, rhs, bin_op_expr.op, operand_typ)
            }
        }
    }

    fn visit_literal_expr(&mut self, literal_expr: &LiteralExpr) -> IRValue {
        match literal_expr {
            LiteralExpr::Integer(value, _) => IRValue::Pri(IRPri::I64(*value)),
            LiteralExpr::Unit(_) => IRValue::Void,
        }
    }

    fn handle_comparison_operation(
        &mut self,
        lhs: IRValue,
        rhs: IRValue,
        operator: Operator,
        operand_typ: Type,
    ) -> IRValue {
        match operand_typ {
            Type::Primitive(Primitive::Integer) | Type::Primitive(Primitive::Bool) => {
                return self.curr_fun().binop(IRType::I1, lhs, rhs, operator);
            }
            Type::Primitive(Primitive::Unit) => return self.handle_unit_comparison(operator),
            Type::Fun(_) => panic!("Cannot compare functions"),
            Type::Variable(_) => unreachable!(),
            _ => (),
        }

        let cmp_fun_name =
            operator.cmp_fun_prefix().to_string() + "." + &operand_typ.poly_arg_str();
        let fun_ptr = IRValue::Global(cmp_fun_name.clone(), IRType::Ptr);

        if self.module.get_function(&cmp_fun_name).is_none() {
            let operands = vec![
                ("lhs".to_string(), IRType::from(&operand_typ)),
                ("rhs".to_string(), IRType::from(&operand_typ)),
            ];
            let fun = Function::new(cmp_fun_name.clone(), IRType::I1, operands);
            let lhs = fun.param(0);
            let rhs = fun.param(1);
            self.module.new_function(fun);
            self.push_ctx(cmp_fun_name, None);
            let ret = match &operand_typ {
                Type::Tuple(typs) => self.handle_tuple_comparison(lhs, rhs, operator, typs),
                Type::Custom(name, args) => {
                    self.handle_variant_comparison(lhs, rhs, operator, name, args)
                }
                _ => unreachable!(),
            };
            self.curr_fun().ret(ret.clone());
            self.pop_ctx();
        }

        self.curr_fun()
            .fast_call(fun_ptr, IRType::I1, vec![lhs, rhs])
    }

    fn handle_tuple_comparison(
        &mut self,
        lhs: IRValue,
        rhs: IRValue,
        operator: Operator,
        typs: &[Rc<RefCell<Type>>],
    ) -> IRValue {
        let res_ptr = self.curr_fun().alloca(IRType::I1);
        self.curr_fun()
            .store(IRValue::Pri(IRPri::I1(false)), res_ptr.clone());
        let exit_bb = self.curr_fun().create_bb("exit");
        let exit_label = exit_bb.label().to_string();
        let true_bb = self.curr_fun().create_bb("true");
        let true_label = true_bb.label().to_string();
        let tuple_typ = IRType::Struct(typs.iter().map(|t| IRType::from(t.clone())).collect());
        for (i, typ) in typs.iter().enumerate() {
            // 1. Load elements
            let ptr = self
                .curr_fun()
                .getelemptr(tuple_typ.clone(), lhs.clone(), &[0, i as i32]);
            let lhs = self.curr_fun().load(IRType::from(typ.clone()), ptr);
            let ptr = self
                .curr_fun()
                .getelemptr(tuple_typ.clone(), rhs.clone(), &[0, i as i32]);
            let rhs = self.curr_fun().load(IRType::from(typ.clone()), ptr);
            let operand_typ = normalize_typ(typ.clone());

            if i != typs.len() - 1 {
                // 2. Check if certainly false
                let operator = match operator {
                    Operator::Eq => Operator::Neq,
                    Operator::Neq => Operator::Eq,
                    Operator::Lte | Operator::Lt => Operator::Gt,
                    Operator::Gte | Operator::Gt => Operator::Lt,
                    _ => unreachable!(),
                };
                let cond = self.handle_comparison_operation(
                    lhs.clone(),
                    rhs.clone(),
                    operator,
                    operand_typ.clone(),
                );

                // 3. Early break
                let follow_label = self.curr_fun().add_new_bb("follow");
                self.curr_fun()
                    .cond_brk(cond, exit_label.clone(), follow_label.clone());
                self.curr_fun().set_bb(follow_label);

                if operator != Operator::Eq && operator != Operator::Neq {
                    // 4. Check is certainly true
                    let cond =
                        self.handle_comparison_operation(lhs, rhs, Operator::Eq, operand_typ);

                    // 5. Early break
                    let follow_label = self.curr_fun().add_new_bb("follow");
                    self.curr_fun()
                        .cond_brk(cond, follow_label.clone(), true_label.clone());
                    self.curr_fun().set_bb(follow_label);
                }
            } else {
                // 2. Compare elements
                let cond = self.handle_comparison_operation(lhs, rhs, operator, operand_typ);
                self.curr_fun()
                    .cond_brk(cond, true_label.clone(), exit_label.clone());

                // 3. True BB
                self.curr_fun().add_bb(true_bb);
                self.curr_fun().set_bb(true_label.clone());
                self.curr_fun()
                    .store(IRValue::Pri(IRPri::I1(true)), res_ptr.clone());
                self.curr_fun().brk(exit_label.clone());
                break;
            }
        }

        self.curr_fun().add_bb(exit_bb);
        self.curr_fun().set_bb(exit_label);
        self.curr_fun().load(IRType::I1, res_ptr)
    }

    fn handle_variant_comparison(
        &mut self,
        lhs: IRValue,
        rhs: IRValue,
        operator: Operator,
        ctor_name: &str,
        variant_args: &[Rc<RefCell<Type>>],
    ) -> IRValue {
        let res_ptr = self.curr_fun().alloca(IRType::I1);
        self.curr_fun()
            .store(IRValue::Pri(IRPri::I1(false)), res_ptr.clone());
        let exit_bb = self.curr_fun().create_bb("exit");
        let exit_label = exit_bb.label().to_string();
        let true_bb = self.curr_fun().create_bb("true");
        let true_label = true_bb.label().to_string();
        let variant_typ = IRType::Struct(vec![IRType::I64, IRType::Ptr]);

        // 1. Check if certainly false
        let ptr = self
            .curr_fun()
            .getelemptr(variant_typ.clone(), lhs.clone(), &[0, 0]);
        let tag_l = self.curr_fun().load(IRType::I64, ptr);
        let ptr = self
            .curr_fun()
            .getelemptr(variant_typ.clone(), rhs.clone(), &[0, 0]);
        let tag_r = self.curr_fun().load(IRType::I64, ptr);
        let op_negation = match operator {
            Operator::Eq => Operator::Neq,
            Operator::Neq => Operator::Eq,
            Operator::Lte | Operator::Lt => Operator::Gt,
            Operator::Gte | Operator::Gt => Operator::Lt,
            _ => unreachable!(),
        };
        let cond = self
            .curr_fun()
            .binop(IRType::I1, tag_l.clone(), tag_r.clone(), op_negation);

        // 2. Early exit
        let follow_label = self.curr_fun().add_new_bb("follow");
        self.curr_fun()
            .cond_brk(cond, exit_label.clone(), follow_label.clone());
        self.curr_fun().set_bb(follow_label);

        if operator != Operator::Eq && operator != Operator::Neq {
            // 3. Check if definitely true
            let cond = self
                .curr_fun()
                .binop(IRType::I1, tag_l.clone(), tag_r, Operator::Eq);

            // 4. Early exit
            let follow_label = self.curr_fun().add_new_bb("follow");
            self.curr_fun()
                .cond_brk(cond, follow_label.clone(), true_label.clone());
            self.curr_fun().set_bb(follow_label);
        }

        // 5. Compare inner value
        let mut case_bbs = vec![];
        let mut case_typs = vec![];
        for constructor in self.custom_types.get_constructors(ctor_name) {
            let case_typ = self.custom_types.get_constructor_arg(constructor);
            if let Some(case_typ) = &case_typ {
                link_unbounds(case_typ.clone(), variant_args);
            }
            let case_bb = self.curr_fun().create_bb(constructor);
            case_typs.push(case_typ);
            case_bbs.push(case_bb);
        }
        // > Create switch instruction
        let mut cases: Vec<(usize, String)> = case_bbs
            .iter()
            .enumerate()
            .map(|(i, bb)| (i, bb.label().to_string()))
            .collect();
        let default_label = cases.pop().unwrap().1;
        self.curr_fun().switch(tag_l, default_label, cases);
        // > Create case BB
        for (case_bb, case_typ) in case_bbs.into_iter().zip(case_typs) {
            let case_label = case_bb.label().to_string();
            self.curr_fun().add_bb(case_bb);
            self.curr_fun().set_bb(case_label);
            if let Some(operand_typ) = case_typ.map(normalize_typ) {
                let ptr = self
                    .curr_fun()
                    .getelemptr(variant_typ.clone(), lhs.clone(), &[0, 1]);
                let lhs = self.curr_fun().load(IRType::from(&operand_typ), ptr);
                let ptr = self
                    .curr_fun()
                    .getelemptr(variant_typ.clone(), rhs.clone(), &[0, 1]);
                let rhs = self.curr_fun().load(IRType::from(&operand_typ), ptr);
                let cond = self.handle_comparison_operation(lhs, rhs, operator, operand_typ);
                self.curr_fun()
                    .cond_brk(cond, true_label.clone(), exit_label.clone());
            } else {
                self.curr_fun().brk(true_label.clone());
            }
        }

        // 6. True BB
        self.curr_fun().add_bb(true_bb);
        self.curr_fun().set_bb(true_label);
        self.curr_fun()
            .store(IRValue::Pri(IRPri::I1(true)), res_ptr.clone());
        self.curr_fun().brk(exit_label.clone());

        // 7. Exit BB
        self.curr_fun().add_bb(exit_bb);
        self.curr_fun().set_bb(exit_label);
        self.curr_fun().load(IRType::I1, res_ptr)
    }

    fn handle_unit_comparison(&mut self, operator: Operator) -> IRValue {
        match operator {
            Operator::Eq | Operator::Lte | Operator::Gte => IRValue::Pri(IRPri::I1(true)),
            Operator::Lt | Operator::Gt => IRValue::Pri(IRPri::I1(false)),
            _ => unreachable!(),
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

    fn gather_conds(
        &mut self,
        pattern: &Pattern,
        typ: Rc<RefCell<Type>>,
        value: IRValue,
    ) -> Vec<IRValue> {
        // TODO: Refactor
        match pattern {
            Pattern::Tuple(elements) => {
                let mut conditions = vec![];
                let element_typs: Vec<Rc<RefCell<Type>>> =
                    extract_tuple_typs(typ).unwrap().into_iter().collect();
                let element_ir_typs = element_typs.iter().cloned().map(IRType::from).collect();
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
                    let element_type = IRType::from(element_typ.clone());
                    let element_value = self.curr_fun().load(element_type, ptr);
                    let mut new_conditions = self.gather_conds(element, element_typ, element_value);
                    conditions.append(&mut new_conditions);
                }
                conditions
            }
            Pattern::Constructor(span, arg) => {
                let mut conditions = vec![];
                // Check tag
                let ctor_name = self.lexer.str_from_span(span);
                let expected_tag = self.custom_types.get_constructor_idx(ctor_name);
                let expected_tag = IRValue::Pri(IRPri::I64(expected_tag as i64));
                let variant_typ = IRType::Struct(vec![IRType::I64, IRType::Ptr]);
                let ptr = self
                    .curr_fun()
                    .getelemptr(variant_typ.clone(), value.clone(), &[0, 0]);
                let tag = self.curr_fun().load(IRType::I64, ptr);
                let is_tag_eq = self
                    .curr_fun()
                    .binop(IRType::I1, expected_tag, tag, Operator::Eq);
                conditions.push(is_tag_eq);
                // Check arg
                if let Some(patt) = arg
                    && patt.has_literal()
                {
                    let variant_args = extract_variant_args(typ).unwrap();
                    let typ = self.custom_types.get_constructor_arg(ctor_name).unwrap();
                    link_unbounds(typ.clone(), &variant_args);
                    let ir_typ = IRType::from(typ.clone());
                    let ptr = self.curr_fun().getelemptr(variant_typ, value, &[0, 1]);
                    let value = self.curr_fun().load(ir_typ, ptr);
                    conditions.append(&mut self.gather_conds(patt, typ, value));
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
        typ: Rc<RefCell<Type>>,
        value: IRValue,
    ) -> Vec<(String, IRValue)> {
        // TODO: Refactor
        match pattern {
            Pattern::Tuple(elements) => {
                let mut bindings = vec![];
                let element_typs: Vec<Rc<RefCell<Type>>> =
                    extract_tuple_typs(typ).unwrap().into_iter().collect();
                let element_ir_typs = element_typs.iter().cloned().map(IRType::from).collect();
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
                    let element_type = IRType::from(element_typ.clone());
                    let element_value = self.curr_fun().load(element_type, ptr);
                    let mut new_bindings = self.gather_binds(element, element_typ, element_value);
                    bindings.append(&mut new_bindings);
                }
                bindings
            }
            Pattern::Constructor(span, Some(pattern)) => {
                let ctor_name = self.lexer.str_from_span(span);
                let variant_args = extract_variant_args(typ).unwrap();
                let typ = self.custom_types.get_constructor_arg(ctor_name).unwrap();
                link_unbounds(typ.clone(), &variant_args);
                let ir_typ = IRType::from(typ.clone());
                let variant_typ = IRType::Struct(vec![IRType::I64, IRType::Ptr]);
                let ptr = self.curr_fun().getelemptr(variant_typ, value, &[0, 1]);
                let value = self.curr_fun().load(ir_typ, ptr);
                self.gather_binds(pattern, typ, value)
            }
            Pattern::Identifier(span) => {
                let name = self.lexer.str_from_span(span);
                vec![(name.to_string(), value)]
            }
            Pattern::Constructor(_, None) | Pattern::Literal(_) | Pattern::None => vec![],
        }
    }

    fn populate_builtins(mut self) -> Self {
        let init = IRValue::Pri(IRPri::Str("%lld"));
        let fmt_str_name = self.module.new_global_constant("oonta.fmt_str", init);
        let fmt_str_ptr = IRValue::Global(fmt_str_name, IRType::Ptr);
        self.insert_print_int_builtin(fmt_str_ptr.clone())
            .insert_read_int_builtin(fmt_str_ptr)
    }

    fn insert_print_int_builtin(mut self, fmt_str_ptr: IRValue) -> Self {
        // 1. Insert printf declaration
        let printf = "printf".to_string();
        let ret_typ = IRType::I32;
        let params = vec![IRType::Ptr];
        let signature = FunSignature::new(printf.clone(), ret_typ, params)
            .varargs()
            .ccc();
        self.module.new_function_decl(signature);

        // 2. Define print_int function
        let ret_typ = IRType::Void;
        let params = vec![("p".to_string(), IRType::I64)];
        let mut fun = Function::new("oonta.print_int.fun".to_string(), ret_typ, params);
        let printf_fun_ptr = IRValue::Global(printf, IRType::Ptr);
        let printf_args = vec![fmt_str_ptr, fun.param(0)];
        fun.normal_call(printf_fun_ptr, IRType::I32, printf_args);
        fun.ret(IRValue::Void);
        let fun_name = self.module.new_function(fun);

        // 3. Insert closure
        let init = IRValue::Global(fun_name, IRType::Ptr);
        let closure_name = self
            .module
            .new_global_constant("oonta.print_int.closure", init);

        // 4. Insert global var
        let init = IRValue::Global(closure_name, IRType::Ptr);
        let print_int = "print_int".to_string();
        let glb_name = Self::glb_name(&print_int);
        let glb_name = self.module.new_global_constant(&glb_name, init);
        self.insert_name_to_ctx(print_int, IRValue::Global(glb_name, IRType::Ptr));

        self
    }

    fn insert_read_int_builtin(mut self, fmt_str_ptr: IRValue) -> Self {
        // 1. Insert scanf declaration
        let scanf = "scanf".to_string();
        let ret_typ = IRType::I32;
        let params = vec![IRType::Ptr];
        let signature = FunSignature::new(scanf.clone(), ret_typ, params)
            .varargs()
            .ccc();
        self.module.new_function_decl(signature);

        // 2. Define read_int function
        let ret_typ = IRType::I64;
        let params = vec![];
        let mut fun = Function::new("oonta.read_int.fun".to_string(), ret_typ, params);
        let scanf_fun_ptr = IRValue::Global(scanf, IRType::Ptr);
        let res_ptr = fun.alloca(IRType::I64);
        let scanf_args = vec![fmt_str_ptr, res_ptr.clone()];
        fun.normal_call(scanf_fun_ptr, IRType::I32, scanf_args);
        let res = fun.load(IRType::I64, res_ptr);
        fun.ret(res);
        let fun_name = self.module.new_function(fun);

        // 3. Insert closure
        let init = IRValue::Global(fun_name, IRType::Ptr);
        let closure_name = self
            .module
            .new_global_constant("oonta.read_int.closure", init);

        // 4. Insert global var
        let init = IRValue::Global(closure_name, IRType::Ptr);
        let read_int = "read_int".to_string();
        let glb_name = Self::glb_name(&read_int);
        let glb_name = self.module.new_global_constant(&glb_name, init);
        self.insert_name_to_ctx(read_int, IRValue::Global(glb_name, IRType::Ptr));

        self
    }

    fn get_ir_typ(&self, expr_ptr: *const Expr) -> IRType {
        IRType::from(self.get_typ(expr_ptr))
    }

    fn get_typ(&self, expr_ptr: *const Expr) -> Rc<RefCell<Type>> {
        self.type_map.get(expr_ptr).expect("Expr not in type_map")
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

    fn create_fun_name(&mut self) -> String {
        let current_bind = if let Some(name) = &self.bind_name {
            name
        } else {
            "unbound"
        };
        format!("oonta.{}.fun", current_bind)
    }

    fn glb_name(bind_name: &str) -> String {
        format!("oonta.{bind_name}")
    }

    fn insert_name_to_ctx(&mut self, name: String, ir_value: IRValue) {
        if let Some(context) = &mut self.context {
            context.insert(name, ir_value);
        } else {
            panic!("context unassigned")
        }
    }

    fn get_value_from_ctx(&self, name: &str) -> Option<IRValue> {
        if let Some(context) = &self.context {
            return context.get(name).cloned();
        }
        None
    }

    fn get_ctx_recursive_bind(&self) -> &Option<String> {
        if let Some(context) = &self.context {
            &context.recursive_bind
        } else {
            panic!("context unassigned")
        }
    }

    fn push_ctx(&mut self, function_name: String, recursive_bind: Option<String>) {
        let mut new_ctx = Context::new(function_name, recursive_bind);
        new_ctx.parent = self.context.take().map(Box::new);
        self.context = Some(new_ctx);
    }

    fn dup_ctx(&mut self) {
        if let Some(parent) = self.context.take() {
            let mut new_ctx = Context::new(parent.fun_name.clone(), parent.recursive_bind.clone());
            new_ctx.parent = Some(Box::new(parent));
            self.context = Some(new_ctx);
        } else {
            panic!("cannot call dup_ctx without parent ctx")
        }
    }

    fn pop_ctx(&mut self) {
        let parent = self.context.take().map(|c| c.parent);
        if let Some(Some(parent)) = parent {
            self.context = Some(*parent);
        }
    }

    fn malloc(&mut self, sz: usize) -> IRValue {
        let sz = IRValue::Pri(IRPri::I64(sz as i64));
        let gcmalloc = "gcmalloc".to_string();
        let ret_typ = match self.module.get_function_decl(&gcmalloc) {
            Some(malloc) => malloc.ret_typ().clone(),
            None => {
                let ret_typ = IRType::Ptr;
                let params = vec![IRType::I64];
                let signature = FunSignature::new(gcmalloc.clone(), ret_typ.clone(), params)
                    .alloc()
                    .ccc();
                self.module.new_function_decl(signature);
                ret_typ
            }
        };
        let fun_ptr = IRValue::Global(gcmalloc, IRType::Ptr);
        self.curr_fun().normal_call(fun_ptr, ret_typ, vec![sz])
    }

    fn is_polymorphic(&self, bind: &Bind) -> bool {
        let typ = {
            let expr_ptr = &*bind.expr.borrow() as *const Expr;
            self.get_typ(expr_ptr)
        };
        is_polymorphic(typ)
    }
}

impl Context {
    fn new(function_name: String, recursive_bind: Option<String>) -> Self {
        Self {
            ir_values: HashMap::new(),
            fun_name: function_name,
            recursive_bind,
            parent: None,
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

use std::collections::HashMap;

use crate::{
    ast::{Ast, Expr, FunExpr, LiteralExpr},
    ir_builder::ir::{Function, IRPrimitive, IRType, IRValue, Instr, Module},
    lexer::Lexer,
    typ::{Primitive, Type, TypeMap, Variable, normalize_typ},
};

pub mod ir;

pub struct IRBuilder<'a> {
    type_map: &'a TypeMap,
    lexer: &'a Lexer,
    context: Context,
    module: Module,
    curr_function: String,
}

#[derive(Default)]
struct Context {
    ir_values: HashMap<String, IRValue>,
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
            context: Context::default(),
            module,
            curr_function: ocaml,
        }
    }

    pub fn build(mut self, ast: &Ast) -> Module {
        for binding in &ast.binds {
            let name = self.lexer.str_from_span(&binding.name).to_string();
            let ir_typ = self.get_typ(&*binding.expr as *const Expr).unwrap();
            self.module.new_global_var(name.clone(), ir_typ);
            let global_val = IRValue::Global(name.clone());
            self.context.insert(name, global_val.clone());
            let expr_val = self.visit_expr(&binding.expr);
            let store_instr = Instr::store(expr_val, global_val);
            self.curr_function().push_instr(store_instr);
        }
        self.module
    }

    fn visit_expr(&mut self, expr: &Expr) -> IRValue {
        let expr_ptr = expr as *const Expr;
        match expr {
            Expr::Literal(literal_expr) => self.visit_literal_expr(literal_expr),
            Expr::Var(var_expr) => todo!(),
            Expr::Fun(fun_expr) => self.visit_fun_expr(fun_expr, expr_ptr),
            Expr::Application(application_expr) => todo!(),
            Expr::LetIn(let_in_expr) => todo!(),
            Expr::BinOp(bin_op_expr) => todo!(),
        }
    }

    fn visit_fun_expr(&mut self, fun_expr: &FunExpr, expr_ptr: *const Expr) -> IRValue {
        // create function
        // create closure
        // bind function to closure's fun
        // bind environment to closure's env
        todo!()
    }

    fn visit_literal_expr(&mut self, literal_expr: &LiteralExpr) -> IRValue {
        match literal_expr {
            LiteralExpr::Integer(value, _) => IRValue::Primitive(IRPrimitive::Integer(*value)),
        }
    }

    fn new_function(fun_expr: &FunExpr) -> Function {
        todo!()
    }

    fn get_typ(&self, expr_ptr: *const Expr) -> Option<IRType> {
        self.type_map
            .get(expr_ptr)
            .map(normalize_typ)
            .map(|t| match t {
                Type::Fun(_) => IRType::Ptr,
                Type::Primitive(Primitive::Integer) => IRType::I32,
                Type::Variable(Variable::Unbound(_)) => todo!(),
                Type::Variable(Variable::Link(_)) => unreachable!(),
            })
    }

    fn curr_function(&mut self) -> &mut Function {
        self.module.get_function(&self.curr_function).unwrap()
    }
}

impl Context {
    fn insert(&mut self, name: String, ir_value: IRValue) {
        self.ir_values.insert(name, ir_value);
    }
}

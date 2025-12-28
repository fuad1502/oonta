use std::{
    cell::RefCell,
    collections::HashMap,
    rc::{Rc, Weak},
};

use crate::{
    ast::{AnonymousFunExpr, Ast, BinOpExpr, Expr, FunExpr, LiteralExpr, VarExpr},
    lexer::Lexer,
};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    Fun(Vec<Rc<RefCell<Type>>>),
    Primitive(Primitive),
    Variable(Variable),
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum Primitive {
    Integer,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Variable {
    Unbound(usize),
    Link(Rc<RefCell<Type>>),
}

#[derive(Default, Debug)]
pub struct Context {
    types: HashMap<String, Rc<RefCell<Type>>>,
    parent: Option<Weak<RefCell<Context>>>,
    childrens: HashMap<*const Expr, Rc<RefCell<Context>>>,
}

pub struct TypeResolver<'a> {
    main_context: Rc<RefCell<Context>>,
    local_context: Option<(*const Expr, Rc<RefCell<Context>>)>,
    curr_context: Option<Rc<RefCell<Context>>>,
    var_id_in_local_ctx: usize,
    lexer: &'a Lexer,
}

impl<'a> TypeResolver<'a> {
    pub fn new(lexer: &'a Lexer) -> Self {
        let main_context = Rc::new(RefCell::new(Context::default()));
        Self {
            main_context,
            local_context: None,
            curr_context: None,
            var_id_in_local_ctx: 0,
            lexer,
        }
    }

    pub fn resolve_types(mut self, ast: Ast) -> Result<Context, String> {
        for binding in ast.binds {
            self.var_id_in_local_ctx = 0;
            let typ = self.infer_type(&binding.expr);
            let typ = rename(typ);
            let name = self.lexer.str_from_span(&binding.name);
            self.main_context.borrow_mut().insert(name, typ);
        }
        Ok(self.main_context.take())
    }

    fn infer_type(&mut self, expr: &Expr) -> Rc<RefCell<Type>> {
        match expr {
            Expr::Literal(literal_expr) => self.infer_literal_expr(literal_expr),
            Expr::Var(var_expr) => self.infer_var_expr(var_expr),
            Expr::Fun(FunExpr::Identifier(_)) => todo!(),
            Expr::Fun(FunExpr::Anonymous(anon_fun_expr)) => {
                self.push_curr_context(expr as *const Expr);
                let typ = self.infer_anon_fun_expr(anon_fun_expr);
                self.pop_curr_context();
                typ
            }
            Expr::Application(application_expr) => todo!(),
            Expr::BinOp(bin_op_expr) => self.infer_binop_expr(bin_op_expr),
            Expr::Partial(partial_expr) => todo!(),
            Expr::LetIn(let_in_expr) => todo!(),
        }
    }

    fn infer_anon_fun_expr(&mut self, anon_fun_expr: &AnonymousFunExpr) -> Rc<RefCell<Type>> {
        let mut fun_typ = vec![];
        for param in &anon_fun_expr.params {
            let name = self.lexer.str_from_span(param);
            let typ = self.new_var();
            fun_typ.push(typ.clone());
            self.insert_binding_to_local_ctx(name, typ);
        }
        let ret_typ = self.new_var();
        fun_typ.push(ret_typ.clone());
        let typ = self.infer_type(&anon_fun_expr.body);
        unify_typ(typ, ret_typ);
        Rc::new(RefCell::new(Type::Fun(fun_typ)))
    }

    fn infer_var_expr(&mut self, var_expr: &VarExpr) -> Rc<RefCell<Type>> {
        let name = self.lexer.str_from_span(&var_expr.id);
        if let Some(typ) = self.get_from_local_ctx(name) {
            return typ;
        }
        if let Some(typ) = self.get_from_main_context(name) {
            return self.instantiate_typ(typ);
        }
        todo!()
    }

    fn infer_binop_expr(&mut self, bin_op_expr: &BinOpExpr) -> Rc<RefCell<Type>> {
        match bin_op_expr.op {
            crate::ast::Operator::Plus
            | crate::ast::Operator::Minus
            | crate::ast::Operator::Star
            | crate::ast::Operator::Slash => {
                let int_typ = Rc::new(RefCell::new(Type::Primitive(Primitive::Integer)));
                let typ = self.infer_type(&bin_op_expr.lhs);
                unify_typ(typ, int_typ.clone());
                let typ = self.infer_type(&bin_op_expr.rhs);
                unify_typ(typ, int_typ.clone());
                let typ = self.new_var();
                unify_typ(typ.clone(), int_typ.clone());
                typ
            }
        }
    }

    fn infer_literal_expr(&mut self, literal_expr: &LiteralExpr) -> Rc<RefCell<Type>> {
        match literal_expr {
            LiteralExpr::Integer(_, _) => {
                Rc::new(RefCell::new(Type::Primitive(Primitive::Integer)))
            }
        }
    }

    fn insert_binding_to_local_ctx(&mut self, name: &str, typ: Rc<RefCell<Type>>) {
        if let Some(context) = &self.curr_context {
            context.borrow_mut().insert(name, typ);
        }
    }

    fn get_from_local_ctx(&self, name: &str) -> Option<Rc<RefCell<Type>>> {
        if let Some(ctx) = &self.curr_context {
            ctx.borrow().get(name)
        } else {
            None
        }
    }

    fn get_from_main_context(&self, name: &str) -> Option<Rc<RefCell<Type>>> {
        self.main_context.borrow().get(name)
    }

    fn instantiate_typ(&mut self, typ: Rc<RefCell<Type>>) -> Rc<RefCell<Type>> {
        match typ.borrow().clone() {
            Type::Primitive(_) => typ.clone(),
            Type::Variable(Variable::Link(typ)) => self.instantiate_typ(typ),
            Type::Fun(typs) => {
                let typs = typs.into_iter().map(|t| self.instantiate_typ(t)).collect();
                Rc::new(RefCell::new(Type::Fun(typs)))
            }
            Type::Variable(Variable::Unbound(i)) => {
                let inst_typ = Type::Variable(Variable::Unbound(i + self.var_id_in_local_ctx));
                self.var_id_in_local_ctx += 1;
                Rc::new(RefCell::new(inst_typ))
            }
        }
    }

    fn new_var(&mut self) -> Rc<RefCell<Type>> {
        let typ = Rc::new(RefCell::new(Type::Variable(Variable::Unbound(
            self.var_id_in_local_ctx,
        ))));
        self.var_id_in_local_ctx += 1;
        typ
    }

    fn push_curr_context(&mut self, expr_ptr: *const Expr) {
        let new_context = Rc::new(RefCell::new(Context::default()));
        match self.curr_context.take() {
            Some(context) => {
                context
                    .borrow_mut()
                    .childrens
                    .insert(expr_ptr, new_context.clone());
                new_context.borrow_mut().parent = Some(Rc::downgrade(&context));
                self.curr_context = Some(new_context);
            }
            None => {
                self.curr_context = Some(new_context.clone());
                self.local_context = Some((expr_ptr, new_context));
            }
        }
    }

    fn pop_curr_context(&mut self) {
        if let Some(curr_context) = self.curr_context.take() {
            if let Some(parent) = &curr_context.borrow().parent {
                let parent = parent.upgrade().unwrap();
                self.curr_context = Some(parent);
            } else {
                self.save_local_context();
                self.curr_context = None;
            }
        }
    }

    fn save_local_context(&mut self) {
        if let Some((expr_ptr, local_context)) = self.local_context.take() {
            local_context.borrow_mut().parent = Some(Rc::downgrade(&self.main_context));
            self.main_context
                .borrow_mut()
                .childrens
                .insert(expr_ptr, local_context);
        }
    }
}

fn unify_typ(typ_a: Rc<RefCell<Type>>, typ_b: Rc<RefCell<Type>>) {
    let unboxed_a = (typ_a.borrow()).clone();
    let unboxed_b = (typ_b.borrow()).clone();
    match (unboxed_a, unboxed_b) {
        (Type::Variable(Variable::Link(typ)), _) => unify_typ(typ, typ_b),
        (_, Type::Variable(Variable::Link(typ))) => unify_typ(typ_a, typ),
        (Type::Variable(Variable::Unbound(var)), _) if !occurs(var, typ_b.clone()) => {
            bind(typ_a, typ_b)
        }
        (_, Type::Variable(Variable::Unbound(var))) if !occurs(var, typ_a.clone()) => {
            bind(typ_b, typ_a)
        }
        (Type::Primitive(prim_a), Type::Primitive(prim_b)) if prim_a == prim_b => (),
        (Type::Fun(typs_a), Type::Fun(typs_b)) if typs_a.len() == typs_b.len() => typs_a
            .into_iter()
            .zip(typs_b)
            .for_each(|(typ_a, typ_b)| unify_typ(typ_a, typ_b)),
        _ => todo!(),
    }
}

fn occurs(var: usize, in_typ: Rc<RefCell<Type>>) -> bool {
    let unbounds = gather_unbounds(in_typ);
    for unbound in unbounds {
        if let Type::Variable(Variable::Unbound(v)) = *unbound.borrow()
            && var == v
        {
            return true;
        }
    }
    false
}

fn bind(from: Rc<RefCell<Type>>, to: Rc<RefCell<Type>>) {
    *from.borrow_mut() = Type::Variable(Variable::Link(to));
}

fn rename(typ: Rc<RefCell<Type>>) -> Rc<RefCell<Type>> {
    let unbounds = gather_unbounds(typ.clone());
    for (i, unbound) in unbounds.iter().enumerate() {
        *unbound.borrow_mut() = Type::Variable(Variable::Unbound(i));
    }
    typ
}

fn gather_unbounds(typ: Rc<RefCell<Type>>) -> Vec<Rc<RefCell<Type>>> {
    match typ.borrow().clone() {
        Type::Fun(typs) => {
            let mut unbounds = vec![];
            for typ in typs {
                unbounds.append(&mut gather_unbounds(typ));
            }
            unbounds
        }
        Type::Variable(Variable::Unbound(_)) => vec![typ.clone()],
        Type::Primitive(_) | Type::Variable(Variable::Link(_)) => vec![],
    }
}

impl Context {
    fn insert(&mut self, name: &str, typ: Rc<RefCell<Type>>) {
        self.types.insert(name.to_string(), typ);
    }

    fn get(&self, name: &str) -> Option<Rc<RefCell<Type>>> {
        match self.types.get(name) {
            Some(typ) => Some(typ.clone()),
            None => match &self.parent {
                Some(parent) => {
                    let parent = parent.upgrade().unwrap();
                    parent.borrow().get(name)
                }
                None => None,
            },
        }
    }
}

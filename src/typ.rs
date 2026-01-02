use core::cell::RefCell;
use core::fmt::Formatter;
use std::collections::HashMap;
use std::rc::{Rc, Weak};

use crate::ast::CondExpr;
use crate::{
    ast::{ApplicationExpr, Ast, BinOpExpr, Expr, FunExpr, LetInExpr, LiteralExpr},
    lexer::Lexer,
    symbol::Span,
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
    Bool,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Variable {
    Unbound(usize),
    Link(Rc<RefCell<Type>>),
}

#[derive(Default)]
pub struct TypeMap {
    map: HashMap<*const Expr, Rc<RefCell<Type>>>,
}

#[derive(Default, Debug)]
struct Context {
    types: HashMap<String, Rc<RefCell<Type>>>,
    parent: Option<Weak<RefCell<Context>>>,
    childrens: HashMap<*const Expr, Rc<RefCell<Context>>>,
}

pub struct TypeResolver<'a> {
    type_map: TypeMap,
    main_context: Rc<RefCell<Context>>,
    local_context: Option<(*const Expr, Rc<RefCell<Context>>)>,
    curr_context: Option<Rc<RefCell<Context>>>,
    var_id_in_local_ctx: usize,
    lexer: &'a Lexer,
}

#[derive(Debug)]
pub enum Error {
    CannotInferExprType(Span, Box<Error>),
    UnboundVariable(Span),
    CannotUnifyType(Rc<RefCell<Type>>, Rc<RefCell<Type>>),
    UnableToApply(Vec<Rc<RefCell<Type>>>, Rc<RefCell<Type>>),
}

type TypeResult = Result<Rc<RefCell<Type>>, Error>;

impl<'a> TypeResolver<'a> {
    pub fn new(lexer: &'a Lexer) -> Self {
        let main_context = Rc::new(RefCell::new(Context::default()));
        Self {
            type_map: TypeMap::default(),
            main_context,
            local_context: None,
            curr_context: None,
            var_id_in_local_ctx: 0,
            lexer,
        }
    }

    pub fn resolve_types(mut self, ast: &Ast) -> Result<TypeMap, Error> {
        for binding in &ast.binds {
            self.var_id_in_local_ctx = 0;
            let typ = self.infer_type(&binding.expr.borrow())?;
            let typ = rename(typ);
            let name = self.lexer.str_from_span(&binding.name);
            self.main_context.borrow_mut().insert(name, typ);
        }
        Ok(self.type_map)
    }

    fn infer_type(&mut self, expr: &Expr) -> TypeResult {
        let typ_res = match expr {
            Expr::Literal(literal_expr) => self.infer_literal_expr(literal_expr),
            Expr::Var(var_expr) => self.infer_var_expr(&var_expr.id),
            Expr::Fun(fun_expr) => {
                self.push_curr_context(expr as *const Expr);
                let typ = self.infer_fun_expr(fun_expr);
                self.pop_curr_context();
                typ
            }
            Expr::Application(application_expr) => self.infer_application_expr(application_expr),
            Expr::BinOp(bin_op_expr) => self.infer_binop_expr(bin_op_expr),
            Expr::LetIn(let_in_expr) => {
                self.push_curr_context(expr as *const Expr);
                let typ = self.infer_let_in_expr(let_in_expr);
                self.pop_curr_context();
                typ
            }
            Expr::Conditional(cond_expr) => self.infer_cond_expr(cond_expr),
        };
        let typ = typ_res.map_err(|e| match e {
            Error::CannotInferExprType(_, _) => e,
            _ => Error::CannotInferExprType(expr.span().clone(), Box::new(e)),
        })?;
        self.type_map.insert(expr as *const Expr, typ.clone());
        Ok(typ)
    }

    fn infer_fun_expr(&mut self, fun_expr: &FunExpr) -> TypeResult {
        let mut fun_typ = vec![];
        for param in &fun_expr.params {
            let name = self.lexer.str_from_span(param);
            let typ = self.new_var();
            fun_typ.push(typ.clone());
            self.insert_binding_to_local_ctx(name, typ);
        }
        let ret_typ = self.new_var();
        fun_typ.push(ret_typ.clone());
        let fun_typ = Rc::new(RefCell::new(Type::Fun(fun_typ)));
        if let Some(name) = &fun_expr.recursive_bind {
            self.insert_binding_to_local_ctx(name, fun_typ.clone());
        }
        let typ = self.infer_type(&fun_expr.body.borrow())?;
        if let Some(name) = &fun_expr.recursive_bind {
            self.remove_binding_from_local_ctx(name);
        }
        unify_typ(ret_typ, typ)?;
        Ok(fun_typ)
    }

    fn infer_cond_expr(&mut self, cond_expr: &CondExpr) -> TypeResult {
        let bool_typ = Rc::new(RefCell::new(Type::Primitive(Primitive::Bool)));
        let cond_typ = self.infer_type(&cond_expr.cond.borrow())?;
        unify_typ(bool_typ, cond_typ)?;
        let ret_typ = self.new_var();
        let yes_typ = self.infer_type(&cond_expr.yes.borrow())?;
        let no_typ = self.infer_type(&cond_expr.no.borrow())?;
        unify_typ(yes_typ.clone(), no_typ)?;
        unify_typ(ret_typ.clone(), yes_typ)?;
        Ok(ret_typ)
    }

    fn infer_let_in_expr(&mut self, let_in_expr: &LetInExpr) -> TypeResult {
        let name = self.lexer.str_from_span(&let_in_expr.bind.0);
        let typ = self.infer_type(&let_in_expr.bind.1.borrow())?;
        self.insert_binding_to_local_ctx(name, typ);
        let ret_typ = self.new_var();
        let typ = self.infer_type(&let_in_expr.expr.borrow())?;
        unify_typ(ret_typ.clone(), typ)?;
        Ok(ret_typ)
    }

    fn infer_application_expr(&mut self, application_expr: &ApplicationExpr) -> TypeResult {
        let mut arg_typs = vec![];
        for arg in &application_expr.binds {
            arg_typs.push(self.infer_type(&arg.borrow())?);
        }
        let ret_typ = self.new_var();
        let inferred_typ = self.infer_type(&application_expr.fun.borrow())?;
        self.unify_application_typ(arg_typs, ret_typ, inferred_typ)
    }

    fn unify_application_typ(
        &mut self,
        arg_typs: Vec<Rc<RefCell<Type>>>,
        ret_typ: Rc<RefCell<Type>>,
        inferred_typ: Rc<RefCell<Type>>,
    ) -> TypeResult {
        let unboxed_inferred_typ = inferred_typ.borrow().clone();
        match unboxed_inferred_typ {
            Type::Variable(Variable::Link(typ)) => {
                self.unify_application_typ(arg_typs, ret_typ, typ)
            }
            Type::Variable(_) => {
                let mut fun_typ = arg_typs;
                fun_typ.push(ret_typ.clone());
                unify_typ(Rc::new(RefCell::new(Type::Fun(fun_typ))), inferred_typ)?;
                Ok(ret_typ)
            }
            Type::Fun(inferred_fun_typs) if inferred_fun_typs.len() > arg_typs.len() => {
                let mut inferred_fun_params = inferred_fun_typs;
                let mut inferred_fun_ret = inferred_fun_params.split_off(arg_typs.len());
                let inferred_fun_ret = if inferred_fun_ret.len() == 1 {
                    inferred_fun_ret.pop().unwrap()
                } else {
                    Rc::new(RefCell::new(Type::Fun(inferred_fun_ret)))
                };
                arg_typs
                    .into_iter()
                    .zip(inferred_fun_params)
                    .try_for_each(|(typ_a, typ_b)| unify_typ(typ_a, typ_b))?;
                unify_typ(ret_typ.clone(), inferred_fun_ret)?;
                Ok(ret_typ)
            }
            Type::Fun(inferred_fun_typs) if does_returns_fun(inferred_typ.clone()) => {
                let mut first_arg_typs = arg_typs;
                let second_arg_typs = first_arg_typs.split_off(inferred_fun_typs.len() - 1);
                let first_ret_typ = self.new_var();
                self.unify_application_typ(first_arg_typs, first_ret_typ.clone(), inferred_typ)?;
                self.unify_application_typ(second_arg_typs, ret_typ.clone(), first_ret_typ)?;
                Ok(ret_typ)
            }
            _ => Err(Error::UnableToApply(arg_typs, inferred_typ)),
        }
    }

    fn infer_var_expr(&mut self, id: &Span) -> TypeResult {
        let name = self.lexer.str_from_span(id);
        if let Some(typ) = self.get_from_local_ctx(name) {
            Ok(typ)
        } else if let Some(typ) = self.get_from_main_context(name) {
            Ok(self.instantiate_typ(typ))
        } else {
            Err(Error::UnboundVariable(id.clone()))
        }
    }

    fn infer_binop_expr(&mut self, bin_op_expr: &BinOpExpr) -> TypeResult {
        match bin_op_expr.op {
            crate::ast::Operator::Plus
            | crate::ast::Operator::Minus
            | crate::ast::Operator::Star
            | crate::ast::Operator::Slash => {
                let int_typ = Rc::new(RefCell::new(Type::Primitive(Primitive::Integer)));
                let typ = self.infer_type(&bin_op_expr.lhs.borrow())?;
                unify_typ(int_typ.clone(), typ)?;
                let typ = self.infer_type(&bin_op_expr.rhs.borrow())?;
                unify_typ(int_typ.clone(), typ)?;
                Ok(int_typ)
            }
            crate::ast::Operator::Eq
            | crate::ast::Operator::Lte
            | crate::ast::Operator::Lt
            | crate::ast::Operator::Gte
            | crate::ast::Operator::Gt => {
                let int_typ = Rc::new(RefCell::new(Type::Primitive(Primitive::Integer)));
                let bool_typ = Rc::new(RefCell::new(Type::Primitive(Primitive::Bool)));
                let typ = self.infer_type(&bin_op_expr.lhs.borrow())?;
                unify_typ(int_typ.clone(), typ)?;
                let typ = self.infer_type(&bin_op_expr.rhs.borrow())?;
                unify_typ(int_typ.clone(), typ)?;
                Ok(bool_typ)
            }
        }
    }

    fn infer_literal_expr(&mut self, literal_expr: &LiteralExpr) -> TypeResult {
        match literal_expr {
            LiteralExpr::Integer(_, _) => {
                Ok(Rc::new(RefCell::new(Type::Primitive(Primitive::Integer))))
            }
        }
    }

    fn insert_binding_to_local_ctx(&mut self, name: &str, typ: Rc<RefCell<Type>>) {
        if let Some(context) = &self.curr_context {
            context.borrow_mut().insert(name, typ);
        }
    }

    fn remove_binding_from_local_ctx(&mut self, name: &str) {
        if let Some(context) = &self.curr_context {
            context.borrow_mut().remove(name);
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

fn unify_typ(typ_a: Rc<RefCell<Type>>, typ_b: Rc<RefCell<Type>>) -> Result<(), Error> {
    let unboxed_a = (typ_a.borrow()).clone();
    let unboxed_b = (typ_b.borrow()).clone();
    match (unboxed_a, unboxed_b) {
        (Type::Variable(Variable::Link(typ)), _) => unify_typ(typ, typ_b),
        (_, Type::Variable(Variable::Link(typ))) => unify_typ(typ_a, typ),
        (Type::Variable(Variable::Unbound(var)), _) if !occurs(var, typ_b.clone()) => {
            bind(typ_a, typ_b);
            Ok(())
        }
        (_, Type::Variable(Variable::Unbound(var))) if !occurs(var, typ_a.clone()) => {
            bind(typ_b, typ_a);
            Ok(())
        }
        (Type::Primitive(prim_a), Type::Primitive(prim_b)) if prim_a == prim_b => Ok(()),
        (Type::Variable(Variable::Unbound(var_a)), Type::Variable(Variable::Unbound(var_b)))
            if var_a == var_b =>
        {
            Ok(())
        }
        (Type::Fun(typs_a), Type::Fun(typs_b)) if typs_a.len() == typs_b.len() => typs_a
            .into_iter()
            .zip(typs_b)
            .try_for_each(|(typ_a, typ_b)| unify_typ(typ_a, typ_b)),
        _ => Err(Error::CannotUnifyType(typ_a, typ_b)),
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

pub fn normalize_typ(typ: Rc<RefCell<Type>>) -> Type {
    let typ = typ.borrow().clone();
    match typ {
        Type::Fun(typs) => {
            let typs = typs
                .into_iter()
                .map(normalize_typ)
                .map(RefCell::new)
                .map(Rc::new)
                .collect();
            Type::Fun(typs)
        }
        Type::Primitive(_) => typ,
        Type::Variable(Variable::Link(typ)) => normalize_typ(typ),
        Type::Variable(Variable::Unbound(_)) => typ,
    }
}

pub fn extract_fun_typs(typ: Type) -> Option<Vec<Rc<RefCell<Type>>>> {
    if let Type::Fun(typs) = typ {
        Some(typs)
    } else {
        None
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Type::Fun(typs) => {
                write!(fmt, "(")?;
                for (i, typ) in typs.iter().enumerate() {
                    if i == typs.len() - 1 {
                        write!(fmt, "{}", typ.borrow())?;
                    } else {
                        write!(fmt, "{} -> ", typ.borrow())?;
                    }
                }
                write!(fmt, ")")
            }
            Type::Variable(Variable::Unbound(var)) => {
                write!(
                    fmt,
                    "\'{}",
                    char::from_u32(*var as u32 + 'a' as u32).unwrap()
                )
            }
            Type::Primitive(primitive) => match primitive {
                Primitive::Integer => write!(fmt, "int"),
                Primitive::Bool => write!(fmt, "bool"),
            },
            Type::Variable(Variable::Link(typ)) => write!(fmt, "{}", typ.borrow()),
        }
    }
}

impl TypeMap {
    pub fn get(&self, expr_ptr: *const Expr) -> Option<Rc<RefCell<Type>>> {
        self.map.get(&expr_ptr).cloned()
    }

    pub fn insert(&mut self, expr_ptr: *const Expr, typ: Rc<RefCell<Type>>) {
        self.map.insert(expr_ptr, typ);
    }
}

impl Context {
    fn insert(&mut self, name: &str, typ: Rc<RefCell<Type>>) {
        self.types.insert(name.to_string(), typ);
    }

    fn remove(&mut self, name: &str) {
        self.types.remove(name);
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

impl Error {
    pub fn report(&self, lexer: &Lexer) -> String {
        match self {
            Error::CannotInferExprType(span, e) => {
                let line = lexer.show_span(span);
                format!(
                    "{line}\nError: cannot infer expression type: {}",
                    e.report(lexer)
                )
            }
            Error::UnboundVariable(span) => format!("Unbound value {}", lexer.str_from_span(span)),
            Error::CannotUnifyType(typ_a, typ_b) => {
                format!("Cannot unify {} with {}", typ_a.borrow(), typ_b.borrow())
            }
            Error::UnableToApply(args, fun_typ) => {
                let args: Vec<String> = args.iter().map(|t| t.borrow().to_string()).collect();
                let args = args.join(", ");
                format!(
                    "Unable to apply the arguments ({}) to function of type {}",
                    args,
                    fun_typ.borrow()
                )
            }
        }
    }
}

fn does_returns_fun(typ: Rc<RefCell<Type>>) -> bool {
    if let Type::Fun(typs) = normalize_typ(typ) {
        let ret_typ = typs.last().unwrap();
        matches!(normalize_typ(ret_typ.clone()), Type::Fun(_))
    } else {
        false
    }
}

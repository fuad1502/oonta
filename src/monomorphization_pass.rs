use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap},
    rc::Rc,
};

use crate::{
    ast::{
        ApplicationExpr, Ast, BinOpExpr, CondExpr, ConstructExpr, Expr, FunExpr, LetInExpr,
        PatternMatchExpr, TupleExpr, VarExpr,
    },
    lexer::Lexer,
    terminal_colors::{BLUE, END, YELLOW},
    typ::{Primitive, Type, TypeMap, Variable, is_polymorphic},
};

struct MonoPass<'a> {
    mono_binds: MonoBinds,
    poly_binds: HashMap<&'a str, Rc<RefCell<Expr>>>,
    binds_to_mono_names: HashMap<&'a str, Vec<String>>,
    binds_indices: HashMap<&'a str, usize>,
    debug: bool,
    type_map: &'a mut TypeMap,
    lexer: &'a Lexer,
}

#[derive(Default)]
pub struct MonoBinds {
    pub binds: Vec<MonoBind>,
    pub forced_mono_binds: Vec<usize>,
}

pub struct MonoBind {
    pub name: String,
    pub expr: Rc<RefCell<Expr>>,
    pub insertion_index: usize,
}

pub fn monomorphize(ast: &Ast, type_map: &mut TypeMap, lexer: &Lexer, debug: bool) -> MonoBinds {
    let mut pass = MonoPass {
        mono_binds: MonoBinds::default(),
        poly_binds: HashMap::new(),
        binds_to_mono_names: HashMap::new(),
        binds_indices: HashMap::new(),
        debug,
        type_map,
        lexer,
    };
    pass.visit_binds(ast);
    pass.mono_binds
}

impl<'a> MonoPass<'a> {
    fn visit_binds(&mut self, ast: &Ast) {
        for (i, bind) in ast.binds.iter().enumerate() {
            let typ = self.get_from_type_map(&bind.expr);
            let name = bind
                .name
                .clone()
                .map(|span| self.lexer.str_from_span(&span));
            if let Some(name) = name {
                self.binds_indices.insert(name, i);
                if let Some(mono_names) = self.binds_to_mono_names.get_mut(name) {
                    mono_names.clear();
                }
            }
            if is_polymorphic(typ) {
                if let Some(name) = name {
                    self.poly_binds.insert(name, bind.expr.clone());
                }
            } else {
                if let Some(name) = name {
                    self.poly_binds.remove(name);
                }
                self.transform_poly_applications(&bind.expr);
            }
        }
    }

    fn transform_poly_applications(&mut self, expr: &Rc<RefCell<Expr>>) {
        match &mut *expr.borrow_mut() {
            Expr::Application(application_expr) => {
                self.transform_poly_application(application_expr);
                application_expr
                    .binds
                    .iter()
                    .for_each(|e| self.transform_poly_applications(e));
            }
            Expr::Fun(fun_expr) => self.transform_poly_applications(&fun_expr.body),
            Expr::Tuple(tuple_expr) => {
                tuple_expr
                    .elements
                    .iter()
                    .for_each(|e| self.transform_poly_applications(e));
            }
            Expr::Construction(construction_expr) => {
                if let Some(arg) = &construction_expr.arg {
                    self.transform_poly_applications(arg);
                }
            }
            Expr::LetIn(let_in_expr) => {
                self.transform_poly_applications(&let_in_expr.bind.1);
                self.transform_poly_applications(&let_in_expr.expr);
            }
            Expr::BinOp(bin_op_expr) => {
                self.transform_poly_applications(&bin_op_expr.lhs);
                self.transform_poly_applications(&bin_op_expr.rhs);
            }
            Expr::Conditional(cond_expr) => {
                self.transform_poly_applications(&cond_expr.cond);
                self.transform_poly_applications(&cond_expr.yes);
                self.transform_poly_applications(&cond_expr.no);
            }
            Expr::PatternMatch(pattern_match_expr) => {
                self.transform_poly_applications(&pattern_match_expr.matched);
                pattern_match_expr
                    .branches
                    .iter()
                    .for_each(|(_, e)| self.transform_poly_applications(e));
            }
            Expr::Var(var) => {
                if self.is_polymorphic(var) {
                    let bind_idx = self.insertion_index(var);
                    if !self.mono_binds.forced_mono_binds.contains(&bind_idx) {
                        self.mono_binds.forced_mono_binds.push(bind_idx);
                        self.debug_force_mono(var);
                    }
                }
            }
            Expr::Literal(_) => (),
        }
    }

    fn transform_poly_application(&mut self, application_expr: &mut ApplicationExpr) {
        let mono_typ = self.get_from_type_map(&application_expr.fun);
        let mut application_fun = application_expr.fun.borrow_mut();
        let var = match &mut *application_fun {
            Expr::Var(var) => var,
            _ => return,
        };
        let poly_expr = match self.poly_binds.get(self.lexer.str_from_span(&var.id)) {
            Some(expr) => expr,
            None => return,
        };
        let poly_typ = self.get_from_type_map(poly_expr);

        let mut poly_args = BTreeMap::new();
        gather_poly_args(&poly_typ, &mono_typ, &mut poly_args);
        let poly_args_str = poly_args_to_string(&poly_args);
        var.poly_args.replace(poly_args_str.clone());

        if !self.is_monomorphized(var) {
            self.debug(var, &poly_typ, &mono_typ, &poly_args);
            let mono_name = var.mono_name(self.lexer);
            let mono_expr = self.monomorphize_expr(&poly_expr.clone(), &poly_args, &mono_name);
            let mono_bind = MonoBind::new(mono_name, mono_expr.clone(), self.insertion_index(var));
            self.mono_binds.binds.push(mono_bind);
            self.insert_mono_name(var);
            self.transform_poly_applications(&mono_expr);
        }
    }

    fn monomorphize_expr(
        &mut self,
        poly_expr: &Rc<RefCell<Expr>>,
        poly_args: &BTreeMap<usize, Rc<RefCell<Type>>>,
        current_bind: &str,
    ) -> Rc<RefCell<Expr>> {
        let expr = match &*poly_expr.borrow() {
            Expr::Fun(FunExpr {
                params,
                body,
                captures,
                recursive_bind,
                span,
            }) => {
                let params = params.clone();
                let body = self.monomorphize_expr(body, poly_args, current_bind);
                let captures = captures.clone();
                let mut recursive_bind = recursive_bind.clone();
                if let Some(bind) = &mut recursive_bind {
                    *bind = current_bind.to_string();
                }
                let span = span.clone();
                Expr::Fun(FunExpr {
                    params,
                    body,
                    captures,
                    recursive_bind,
                    span,
                })
            }
            Expr::Application(ApplicationExpr { fun, binds, span }) => {
                let fun = self.monomorphize_expr(fun, poly_args, current_bind);
                let binds = binds
                    .iter()
                    .map(|b| self.monomorphize_expr(b, poly_args, current_bind))
                    .collect();
                let span = span.clone();
                Expr::Application(ApplicationExpr { fun, binds, span })
            }
            Expr::Conditional(CondExpr {
                cond,
                yes,
                no,
                span,
            }) => {
                let cond = self.monomorphize_expr(cond, poly_args, current_bind);
                let yes = self.monomorphize_expr(yes, poly_args, current_bind);
                let no = self.monomorphize_expr(no, poly_args, current_bind);
                let span = span.clone();
                Expr::Conditional(CondExpr {
                    cond,
                    yes,
                    no,
                    span,
                })
            }
            Expr::PatternMatch(PatternMatchExpr {
                matched,
                branches,
                span,
            }) => {
                let matched = self.monomorphize_expr(matched, poly_args, current_bind);
                let branches = branches
                    .iter()
                    .map(|b| {
                        (
                            b.0.clone(),
                            self.monomorphize_expr(&b.1, poly_args, current_bind),
                        )
                    })
                    .collect();
                let span = span.clone();
                Expr::PatternMatch(PatternMatchExpr {
                    matched,
                    branches,
                    span,
                })
            }
            Expr::Tuple(TupleExpr { elements, span }) => {
                let elements = elements
                    .iter()
                    .map(|e| self.monomorphize_expr(e, poly_args, current_bind))
                    .collect();
                let span = span.clone();
                Expr::Tuple(TupleExpr { elements, span })
            }
            Expr::BinOp(BinOpExpr { op, lhs, rhs, span }) => {
                let op = *op;
                let lhs = self.monomorphize_expr(lhs, poly_args, current_bind);
                let rhs = self.monomorphize_expr(rhs, poly_args, current_bind);
                let span = span.clone();
                Expr::BinOp(BinOpExpr { op, lhs, rhs, span })
            }
            Expr::Construction(ConstructExpr { cons, arg, span }) => {
                let cons = cons.clone();
                let arg = arg
                    .clone()
                    .map(|expr| self.monomorphize_expr(&expr, poly_args, current_bind));
                let span = span.clone();
                Expr::Construction(ConstructExpr { cons, arg, span })
            }
            Expr::LetIn(LetInExpr { bind, expr, span }) => {
                let current_bind = self.lexer.str_from_span(&bind.0);
                let bind = (
                    bind.0.clone(),
                    self.monomorphize_expr(&bind.1, poly_args, current_bind),
                );
                let expr = self.monomorphize_expr(expr, poly_args, current_bind);
                let span = span.clone();
                Expr::LetIn(LetInExpr { bind, expr, span })
            }
            Expr::Literal(literal_expr) => Expr::Literal(literal_expr.clone()),
            Expr::Var(var_expr) => Expr::Var(var_expr.clone()),
        };
        let expr = Rc::new(RefCell::new(expr));
        let poly_typ = self.get_from_type_map(poly_expr);
        let typ = monomorphize_typ(&poly_typ, poly_args);
        self.insert_into_type_map(&expr, typ);
        expr
    }

    fn get_from_type_map(&self, expr: &Rc<RefCell<Expr>>) -> Rc<RefCell<Type>> {
        let expr_ptr = &*expr.borrow() as *const Expr;
        self.type_map.get(expr_ptr).unwrap()
    }

    fn insert_into_type_map(&mut self, expr: &Rc<RefCell<Expr>>, typ: Rc<RefCell<Type>>) {
        let expr_ptr = &*expr.borrow() as *const Expr;
        self.type_map.insert(expr_ptr, typ);
    }

    fn is_monomorphized(&self, var: &VarExpr) -> bool {
        let base_name = var.base_name(self.lexer);
        let mono_name = var.mono_name(self.lexer);
        self.binds_to_mono_names
            .get(base_name)
            .map(|mono_names| mono_names.contains(&mono_name))
            .unwrap_or(false)
    }

    fn insertion_index(&self, var: &VarExpr) -> usize {
        let base_name = var.base_name(self.lexer);
        *self.binds_indices.get(base_name).unwrap()
    }

    fn insert_mono_name(&mut self, var: &VarExpr) {
        let base_name = var.base_name(self.lexer);
        let mono_name = var.mono_name(self.lexer);
        if let Some(mono_names) = self.binds_to_mono_names.get_mut(base_name) {
            mono_names.push(mono_name);
        } else {
            self.binds_to_mono_names
                .insert(base_name, vec![mono_name.clone()]);
        }
    }

    fn is_polymorphic(&self, var: &VarExpr) -> bool {
        let base_name = var.base_name(self.lexer);
        self.poly_binds.contains_key(base_name)
    }

    fn debug(
        &self,
        var: &VarExpr,
        poly_typ: &Rc<RefCell<Type>>,
        mono_typ: &Rc<RefCell<Type>>,
        poly_args: &BTreeMap<usize, Rc<RefCell<Type>>>,
    ) {
        if self.debug {
            println!(
                "Monomorphing {YELLOW}'{}' {}{END} into {BLUE}{}{END}:",
                var.base_name(self.lexer),
                poly_typ.borrow(),
                mono_typ.borrow()
            );
            poly_args.values().enumerate().for_each(|(i, v)| {
                println!(
                    "{YELLOW}'{}{END} -> {BLUE}{}{END}",
                    char::from_u32(i as u32 + 'a' as u32).unwrap(),
                    v.borrow()
                )
            });
        }
    }

    fn debug_force_mono(&self, var: &VarExpr) {
        if self.debug {
            let base_name = var.base_name(self.lexer);
            println!("{BLUE}'{}'{END} bind forced as monomorphic", base_name);
        }
    }
}

fn gather_poly_args(
    poly_typ: &Rc<RefCell<Type>>,
    mono_typ: &Rc<RefCell<Type>>,
    typ_args: &mut BTreeMap<usize, Rc<RefCell<Type>>>,
) {
    match (&*poly_typ.borrow(), &*mono_typ.borrow()) {
        (_, Type::Variable(Variable::Link(mono_typ))) => {
            gather_poly_args(poly_typ, mono_typ, typ_args)
        }
        (Type::Variable(Variable::Link(poly_typ)), _) => {
            gather_poly_args(poly_typ, mono_typ, typ_args)
        }
        (Type::Variable(Variable::Unbound(v)), Type::Custom(_, _))
        | (Type::Variable(Variable::Unbound(v)), Type::Tuple(_))
        | (Type::Variable(Variable::Unbound(v)), Type::Primitive(_)) => {
            typ_args.insert(*v, mono_typ.clone());
        }
        (Type::Fun(poly_typs), Type::Fun(mono_typs))
        | (Type::Tuple(poly_typs), Type::Tuple(mono_typs))
        | (Type::Custom(_, poly_typs), Type::Custom(_, mono_typs)) => {
            poly_typs
                .iter()
                .zip(mono_typs)
                .for_each(|(poly_typ, mono_typ)| gather_poly_args(poly_typ, mono_typ, typ_args));
        }
        (Type::Variable(Variable::Unbound(v)), Type::Variable(Variable::Unbound(_))) => {
            // If there is still an unbound variable left, that means it does not matter what type
            // it is bound to. We'll use integer as default.
            // TODO: Use available monomorphized expression
            let int_typ = Rc::new(RefCell::new(Type::Primitive(Primitive::Integer)));
            typ_args.insert(*v, int_typ);
        }
        _ => (),
    }
}

fn poly_args_to_string(typ_args: &BTreeMap<usize, Rc<RefCell<Type>>>) -> String {
    typ_args
        .values()
        .map(|t| t.borrow().poly_arg_str())
        .collect::<Vec<String>>()
        .join(".")
}

fn monomorphize_typ(
    poly_typ: &Rc<RefCell<Type>>,
    typ_args: &BTreeMap<usize, Rc<RefCell<Type>>>,
) -> Rc<RefCell<Type>> {
    match &*poly_typ.borrow() {
        Type::Fun(typs) => {
            let typs = typs
                .iter()
                .map(|typ| monomorphize_typ(typ, typ_args))
                .collect();
            Rc::new(RefCell::new(Type::Fun(typs)))
        }
        Type::Tuple(typs) => {
            let typs = typs
                .iter()
                .map(|typ| monomorphize_typ(typ, typ_args))
                .collect();
            Rc::new(RefCell::new(Type::Tuple(typs)))
        }
        Type::Custom(name, args) => {
            let args = args
                .iter()
                .map(|arg| monomorphize_typ(arg, typ_args))
                .collect();
            Rc::new(RefCell::new(Type::Custom(name.clone(), args)))
        }
        Type::Variable(Variable::Unbound(var)) => typ_args.get(var).unwrap().clone(),
        Type::Variable(Variable::Link(to)) => monomorphize_typ(to, typ_args),
        Type::Primitive(_) => poly_typ.clone(),
    }
}

impl MonoBind {
    fn new(name: String, expr: Rc<RefCell<Expr>>, insertion_index: usize) -> Self {
        Self {
            name,
            expr,
            insertion_index,
        }
    }
}

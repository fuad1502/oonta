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
    typ::{Type, TypeMap, Variable, is_polymorphic},
};

pub fn monomorphize(ast: &Ast, type_map: &mut TypeMap, lexer: &Lexer, debug: bool) -> MonoExprs {
    let mut pass = MonoPass {
        mono_exprs: MonoExprs::default(),
        poly_binds: HashMap::new(),
        debug,
        type_map,
        lexer,
    };
    pass.visit_binds(ast);
    pass.mono_exprs
}

#[derive(Default)]
pub struct MonoExprs {
    pub binds: Vec<(String, Rc<RefCell<Expr>>)>,
}

struct MonoPass<'a> {
    mono_exprs: MonoExprs,
    poly_binds: HashMap<&'a str, Rc<RefCell<Expr>>>,
    debug: bool,
    type_map: &'a mut TypeMap,
    lexer: &'a Lexer,
}

impl<'a> MonoPass<'a> {
    fn visit_binds(&mut self, ast: &Ast) {
        for bind in &ast.binds {
            let typ = self.get_from_type_map(&bind.expr);
            if is_polymorphic(typ) {
                if let Some(span) = &bind.name {
                    let name = self.lexer.str_from_span(span);
                    self.poly_binds.insert(name, bind.expr.clone());
                }
            } else {
                self.transform_poly_applications(&bind.expr);
            }
        }
    }

    fn transform_poly_applications(&mut self, expr: &Rc<RefCell<Expr>>) {
        match &mut *expr.borrow_mut() {
            Expr::Application(application_expr) => {
                self.transform_poly_application(application_expr)
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
            Expr::Literal(_) | Expr::Var(_) => (),
        }
    }

    fn transform_poly_application(&mut self, application_expr: &mut ApplicationExpr) {
        // TODO: Refactor
        let mono_typ = self.get_from_type_map(&application_expr.fun);
        if let Expr::Var(VarExpr {
            id: var_id,
            poly_args: var_poly_args,
        }) = &mut *application_expr.fun.borrow_mut()
            && let Some(poly_expr) = self.poly_binds.get(self.lexer.str_from_span(var_id))
        {
            let poly_typ = self.get_from_type_map(poly_expr);
            let mut poly_args = BTreeMap::new();
            gather_poly_args(&poly_typ, &mono_typ, &mut poly_args);
            let poly_args_str = poly_args_to_string(&poly_args);
            var_poly_args.replace(poly_args_str.clone());

            let var = VarExpr {
                id: var_id.clone(),
                poly_args: var_poly_args.clone(),
            };
            let mono_name = var.mono_name(self.lexer);
            let has_monomorphized = self
                .mono_exprs
                .binds
                .iter()
                .any(|(name, _)| name == &mono_name);
            if !has_monomorphized {
                self.debug(&var, &poly_typ, &mono_typ, &poly_args);
                let mono_expr = self.monomorphize_expr(&poly_expr.clone(), &poly_args);
                self.mono_exprs.binds.push((mono_name, mono_expr.clone()));
                self.transform_poly_applications(&mono_expr);
            }
        }
    }

    fn monomorphize_expr(
        &mut self,
        poly_expr: &Rc<RefCell<Expr>>,
        poly_args: &BTreeMap<usize, Rc<RefCell<Type>>>,
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
                let body = self.monomorphize_expr(body, poly_args);
                let captures = captures.clone();
                // TODO: should the name be changed here?
                let recursive_bind = recursive_bind.clone();
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
                let fun = self.monomorphize_expr(fun, poly_args);
                let binds = binds
                    .iter()
                    .map(|b| self.monomorphize_expr(b, poly_args))
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
                let cond = self.monomorphize_expr(cond, poly_args);
                let yes = self.monomorphize_expr(yes, poly_args);
                let no = self.monomorphize_expr(no, poly_args);
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
                let matched = self.monomorphize_expr(matched, poly_args);
                let branches = branches
                    .iter()
                    .map(|b| (b.0.clone(), self.monomorphize_expr(&b.1, poly_args)))
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
                    .map(|e| self.monomorphize_expr(e, poly_args))
                    .collect();
                let span = span.clone();
                Expr::Tuple(TupleExpr { elements, span })
            }
            Expr::BinOp(BinOpExpr { op, lhs, rhs, span }) => {
                let op = *op;
                let lhs = self.monomorphize_expr(lhs, poly_args);
                let rhs = self.monomorphize_expr(rhs, poly_args);
                let span = span.clone();
                Expr::BinOp(BinOpExpr { op, lhs, rhs, span })
            }
            Expr::Construction(ConstructExpr { cons, arg, span }) => {
                let cons = cons.clone();
                let arg = arg
                    .clone()
                    .map(|expr| self.monomorphize_expr(&expr, poly_args));
                let span = span.clone();
                Expr::Construction(ConstructExpr { cons, arg, span })
            }
            Expr::LetIn(LetInExpr { bind, expr, span }) => {
                let bind = (bind.0.clone(), self.monomorphize_expr(&bind.1, poly_args));
                let expr = self.monomorphize_expr(expr, poly_args);
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
                var.mono_name(self.lexer),
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
        (Type::Variable(Variable::Unbound(v)), Type::Primitive(_))
        | (Type::Variable(Variable::Unbound(v)), Type::Custom(_)) => {
            typ_args.insert(*v, mono_typ.clone());
        }
        (Type::Fun(poly_typs), Type::Fun(mono_typs))
        | (Type::Tuple(poly_typs), Type::Tuple(mono_typs)) => {
            poly_typs
                .iter()
                .zip(mono_typs)
                .for_each(|(poly_typ, mono_typ)| gather_poly_args(poly_typ, mono_typ, typ_args));
        }
        _ => (),
    }
}

fn poly_args_to_string(typ_args: &BTreeMap<usize, Rc<RefCell<Type>>>) -> String {
    typ_args
        .values()
        .map(|t| t.borrow().to_string())
        .map(|t| if t == "()" { "unit".to_string() } else { t })
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
        Type::Variable(Variable::Unbound(var)) => typ_args.get(var).unwrap().clone(),
        Type::Variable(Variable::Link(to)) => monomorphize_typ(to, typ_args),
        Type::Primitive(_) | Type::Custom(_) => poly_typ.clone(),
    }
}

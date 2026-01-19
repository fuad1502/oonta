use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::{ApplicationExpr, Ast, Expr};
use crate::lexer::Lexer;
use crate::pass::type_inference::TypeMap;
use crate::symbol::Span;
use crate::typ::extract_fun_typs;

struct CurryTransformer<'a> {
    debug: bool,
    type_map: &'a mut TypeMap,
    lexer: &'a Lexer,
}

pub fn curry_applications(ast: &Ast, type_map: &mut TypeMap, lexer: &Lexer, debug: bool) {
    CurryTransformer {
        debug,
        type_map,
        lexer,
    }
    .curry_applications(ast);
}

impl<'a> CurryTransformer<'a> {
    fn curry_applications(&mut self, ast: &Ast) {
        for binding in &ast.binds {
            self.curry_expr(&binding.expr);
        }
    }

    fn curry_expr(&mut self, expr: &Rc<RefCell<Expr>>) {
        match &mut *expr.borrow_mut() {
            Expr::Application(application_expr) => {
                self.curry_application(application_expr);
                application_expr
                    .binds
                    .iter()
                    .for_each(|e| self.curry_expr(e));
            }
            Expr::Fun(fun_expr) => self.curry_expr(&fun_expr.body),
            Expr::Tuple(tuple_expr) => {
                tuple_expr.elements.iter().for_each(|e| self.curry_expr(e));
            }
            Expr::Construction(construction_expr) => {
                if let Some(arg) = &construction_expr.arg {
                    self.curry_expr(arg);
                }
            }
            Expr::LetIn(let_in_expr) => {
                self.curry_expr(&let_in_expr.bind.1);
                self.curry_expr(&let_in_expr.expr);
            }
            Expr::BinOp(bin_op_expr) => {
                self.curry_expr(&bin_op_expr.lhs);
                self.curry_expr(&bin_op_expr.rhs);
            }
            Expr::Conditional(cond_expr) => {
                self.curry_expr(&cond_expr.cond);
                self.curry_expr(&cond_expr.yes);
                self.curry_expr(&cond_expr.no);
            }
            Expr::PatternMatch(pattern_match_expr) => {
                self.curry_expr(&pattern_match_expr.matched);
                pattern_match_expr
                    .branches
                    .iter()
                    .for_each(|(_, e)| self.curry_expr(e));
            }
            Expr::Literal(_) | Expr::Var(_) => (),
        }
    }

    fn curry_application(&mut self, application_expr: &mut ApplicationExpr) {
        let mut fun_typs = {
            let fun_expr_ptr = &*application_expr.fun.borrow() as *const Expr;
            let fun_typ = self.type_map.get(fun_expr_ptr).unwrap();
            extract_fun_typs(fun_typ).unwrap()
        };
        if application_expr.binds.len() > (fun_typs.len() - 1) {
            self.print_debug_info(application_expr.binds.len() - (fun_typs.len() - 1));
            self.print_expr_before(application_expr);
            let mut args = application_expr.binds.clone();
            let args_reminder = args.split_off(fun_typs.len() - 1);
            let span = Span::new(
                application_expr.span.start_pos(),
                args.last().unwrap().borrow().span().end_pos(),
            );
            let inner_expr = ApplicationExpr {
                fun: application_expr.fun.clone(),
                binds: args,
                span,
            };
            let inner_expr = Rc::new(RefCell::new(Expr::Application(inner_expr)));

            let typ = fun_typs.pop().unwrap();
            self.type_map
                .insert(&*inner_expr.borrow() as *const Expr, typ);

            application_expr.fun = inner_expr;
            application_expr.binds = args_reminder;
            self.print_expr_after(application_expr);
            self.curry_application(application_expr);
        }
    }

    fn print_expr_before(&self, application_expr: &ApplicationExpr) {
        if self.debug {
            println!("> Before currying:");
            application_expr.pretty_print(self.lexer);
        }
    }

    fn print_expr_after(&self, application_expr: &ApplicationExpr) {
        if self.debug {
            println!("> After currying:");
            application_expr.pretty_print(self.lexer);
        }
    }

    fn print_debug_info(&self, extra_arguments: usize) {
        if self.debug {
            println!(
                "Found over-application (extra arguments: {extra_arguments}). Currying expression:"
            )
        }
    }
}

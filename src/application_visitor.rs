use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{ApplicationExpr, Ast, Expr},
    lexer::Lexer,
    symbol::Span,
    typ::{TypeMap, extract_fun_typs, normalize_typ},
};

struct TransformApplicationsVisitor<'a> {
    debug: bool,
    type_map: &'a mut TypeMap,
    lexer: &'a Lexer,
}

pub fn transform_applications(ast: &Ast, type_map: &mut TypeMap, lexer: &Lexer, debug: bool) {
    TransformApplicationsVisitor {
        debug,
        type_map,
        lexer,
    }
    .transform_applications(ast);
}

impl<'a> TransformApplicationsVisitor<'a> {
    fn transform_applications(&mut self, ast: &Ast) {
        for binding in &ast.binds {
            self.transform_expr(&binding.expr);
        }
    }

    fn transform_expr(&mut self, expr: &Rc<RefCell<Expr>>) {
        match &mut *expr.borrow_mut() {
            Expr::Application(application_expr) => self.transform_application(application_expr),
            Expr::Fun(fun_expr) => self.transform_expr(&fun_expr.body),
            Expr::LetIn(let_in_expr) => {
                self.transform_expr(&let_in_expr.bind.1);
                self.transform_expr(&let_in_expr.expr);
            }
            Expr::BinOp(bin_op_expr) => {
                self.transform_expr(&bin_op_expr.lhs);
                self.transform_expr(&bin_op_expr.rhs);
            }
            Expr::Conditional(cond_expr) => {
                self.transform_expr(&cond_expr.cond);
                self.transform_expr(&cond_expr.yes);
                self.transform_expr(&cond_expr.no);
            }
            Expr::Literal(_) | Expr::Var(_) => (),
        }
    }

    fn transform_application(&mut self, application_expr: &mut ApplicationExpr) {
        let mut fun_typs = {
            let fun_expr_ptr = &*application_expr.fun.borrow() as *const Expr;
            let fun_typ = normalize_typ(self.type_map.get(fun_expr_ptr).unwrap());
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
            self.transform_application(application_expr);
        }
    }

    fn print_expr_before(&self, application_expr: &ApplicationExpr) {
        if self.debug {
            println!("> Before transformation:");
            application_expr.pretty_print(self.lexer);
        }
    }

    fn print_expr_after(&self, application_expr: &ApplicationExpr) {
        if self.debug {
            println!("> After transformation:");
            application_expr.pretty_print(self.lexer);
        }
    }

    fn print_debug_info(&self, extra_arguments: usize) {
        if self.debug {
            println!(
                "Found over-application (extra arguments: {extra_arguments}). Transforming application expression:"
            )
        }
    }
}

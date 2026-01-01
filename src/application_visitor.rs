use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{ApplicationExpr, Ast, Expr},
    symbol::Span,
    typ::{Type, TypeMap, extract_fun_typs, normalize_typ},
};

pub fn transform_applications(ast: &Ast, type_map: &mut TypeMap) {
    for binding in &ast.binds {
        transform_expr(&binding.expr, type_map);
    }
}

fn transform_expr(expr: &Rc<RefCell<Expr>>, type_map: &mut TypeMap) {
    match &mut *expr.borrow_mut() {
        Expr::Application(application_expr) => transform_application(application_expr, type_map),
        Expr::Fun(fun_expr) => transform_expr(&fun_expr.body, type_map),
        Expr::LetIn(let_in_expr) => {
            transform_expr(&let_in_expr.bind.1, type_map);
            transform_expr(&let_in_expr.expr, type_map);
        }
        Expr::BinOp(bin_op_expr) => {
            transform_expr(&bin_op_expr.lhs, type_map);
            transform_expr(&bin_op_expr.rhs, type_map);
        }
        Expr::Literal(_) | Expr::Var(_) => (),
    }
}

fn transform_application(application_expr: &mut ApplicationExpr, type_map: &mut TypeMap) {
    let mut fun_typs = {
        let fun_expr_ptr = &*application_expr.fun.borrow() as *const Expr;
        let fun_typ = normalize_typ(type_map.get(fun_expr_ptr).unwrap());
        extract_fun_typs(fun_typ).unwrap()
    };
    if application_expr.binds.len() > fun_typs.len() - 1 {
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
        type_map.insert(&*inner_expr.borrow() as *const Expr, typ);

        application_expr.fun = inner_expr;
        application_expr.binds = args_reminder;
        transform_application(application_expr, type_map);
    }
}

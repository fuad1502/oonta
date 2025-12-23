use std::rc::Rc;

use crate::{
    ast::{ApplicationExpr, Ast, BinOpExpr, Bind, FunExpr, LiteralExpr, Operator, VarExpr},
    symbol::Symbol,
};

pub fn build_ast(cst_root: Symbol) -> Ast {
    let literal_expr = Rc::new(LiteralExpr::Integer(5));
    let bind_x = Bind {
        name: "x".to_string(),
        expr: literal_expr,
    };

    let binop_expr = Rc::new(BinOpExpr {
        op: Operator::Plus,
        lhs: Rc::new(VarExpr {
            id: "a".to_string(),
        }),
        rhs: Rc::new(VarExpr {
            id: "b".to_string(),
        }),
    });
    let binop_expr = Rc::new(BinOpExpr {
        op: Operator::Plus,
        lhs: binop_expr,
        rhs: Rc::new(VarExpr {
            id: "x".to_string(),
        }),
    });
    let anon_func = Rc::new(FunExpr::Anonymous(
        vec!["x".to_string()],
        vec!["a".to_string(), "b".to_string()],
        binop_expr,
    ));
    let bind_add = Bind {
        name: "add".to_string(),
        expr: anon_func,
    };

    let appl_add = Rc::new(ApplicationExpr {
        fun: Rc::new(FunExpr::Identifier("add".to_string())),
        binds: vec![
            (
                "a".to_string(),
                Rc::new(VarExpr {
                    id: "x".to_string(),
                }),
            ),
            (
                "b".to_string(),
                Rc::new(VarExpr {
                    id: "x".to_string(),
                }),
            ),
        ],
    });
    let bind_y = Bind {
        name: "y".to_string(),
        expr: appl_add,
    };

    Ast {
        binds: vec![bind_x, bind_add, bind_y],
    }
}

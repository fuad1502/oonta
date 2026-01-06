use crate::{
    ast::{
        ApplicationExpr, Ast, BinOpExpr, CondExpr, Expr, FunExpr, LetInExpr, LiteralExpr,
        TupleExpr, VarExpr,
    },
    lexer::Lexer,
    terminal_colors::{BLUE, END, YELLOW},
};

pub struct AstPrinter<'a> {
    pub lexer: &'a Lexer,
    indent: String,
}

impl<'a> AstPrinter<'a> {
    pub fn new(lexer: &'a Lexer) -> Self {
        Self {
            lexer,
            indent: String::new(),
        }
    }

    pub fn pretty_print(mut self, ast: &Ast) {
        for binding in &ast.binds {
            let name = if let Some(name) = &binding.name {
                self.lexer.str_from_span(name)
            } else {
                "()"
            };
            println!("{YELLOW}{name}{END} = ");
            self.pretty_print_expr(&binding.expr.borrow());
            println!();
            self.indent = String::new();
        }
    }

    pub fn pretty_print_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal(literal_expr) => self.pretty_print_literal_expr(literal_expr),
            Expr::Var(var_expr) => self.pretty_print_var_expr(var_expr),
            Expr::Fun(fun_expr) => self.pretty_print_fun_expr(fun_expr),
            Expr::Tuple(tuple_expr) => self.pretty_print_tuple_expr(tuple_expr),
            Expr::Application(application_expr) => {
                self.pretty_print_application_expr(application_expr)
            }
            Expr::LetIn(let_in_expr) => self.pretty_print_let_in_expr(let_in_expr),
            Expr::BinOp(bin_op_expr) => self.pretty_print_bin_op_expr(bin_op_expr),
            Expr::Conditional(cond_expr) => self.pretty_print_cond_expr(cond_expr),
        }
    }

    fn pretty_print_fun_expr(&mut self, fun_expr: &FunExpr) {
        println!("{}{BLUE}FunExpr{END}", self.indent);
        let parameters = fun_expr
            .params
            .iter()
            .map(|p| self.lexer.str_from_span(p))
            .collect::<Vec<&str>>()
            .join(", ");
        let captures = fun_expr
            .captures
            .iter()
            .map(|s| &s[..])
            .collect::<Vec<&str>>()
            .join(", ");
        let recursive = match fun_expr.recursive_bind {
            Some(_) => "yes",
            None => "no",
        };
        println!("{}├─▸ parameters: [{parameters}]", self.indent);
        println!("{}├─▸ captures: [{captures}]", self.indent);
        println!("{}├─▸ recursive: {recursive}", self.indent);
        println!("{}└─▸ body:", self.indent);
        self.indent += "    ";
        self.pretty_print_expr(&fun_expr.body.borrow());
    }

    pub fn pretty_print_tuple_expr(&mut self, tuple_expr: &TupleExpr) {
        println!("{}{BLUE}TupleExpr{END}", self.indent);
        println!("{}└─▸ elements:", self.indent);
        let orig_indent = self.indent.clone();
        for (i, element) in tuple_expr.elements.iter().enumerate() {
            self.indent = orig_indent.clone();
            if i < tuple_expr.elements.len() - 1 {
                println!("{}    ├─▸ ({i})", self.indent);
                self.indent += "    │   ";
            } else {
                println!("{}    └─▸ ({i})", self.indent);
                self.indent += "        ";
            }
            self.pretty_print_expr(&element.borrow());
        }
    }

    pub fn pretty_print_application_expr(&mut self, application_expr: &ApplicationExpr) {
        println!("{}{BLUE}ApplicationExpr{END}", self.indent);
        println!("{}├─▸ function:", self.indent);
        let orig_indent = self.indent.clone();
        self.indent += "│   ";
        self.pretty_print_expr(&application_expr.fun.borrow());
        self.indent = orig_indent.clone();
        println!("{}└─▸ binds:", self.indent);
        for (i, bind) in application_expr.binds.iter().enumerate() {
            self.indent = orig_indent.clone();
            if i < application_expr.binds.len() - 1 {
                println!("{}    ├─▸ ({i})", self.indent);
                self.indent += "    │   ";
            } else {
                println!("{}    └─▸ ({i})", self.indent);
                self.indent += "        ";
            }
            self.pretty_print_expr(&bind.borrow());
        }
    }

    fn pretty_print_bin_op_expr(&mut self, bin_op_expr: &BinOpExpr) {
        println!("{}{BLUE}BinOpExpr{END}", self.indent);
        println!("{}├─▸ operator: {}", self.indent, bin_op_expr.op);
        println!("{}├─▸ lhs:", self.indent);
        let orig_indent = self.indent.clone();
        self.indent += "│   ";
        self.pretty_print_expr(&bin_op_expr.lhs.borrow());
        self.indent = orig_indent;
        println!("{}└─▸ rhs:", self.indent);
        self.indent += "    ";
        self.pretty_print_expr(&bin_op_expr.rhs.borrow());
    }

    fn pretty_print_cond_expr(&mut self, cond_expr: &CondExpr) {
        println!("{}{BLUE}CondExpr{END}", self.indent);
        println!("{}├─▸ condition:", self.indent);
        let orig_indent = self.indent.clone();
        self.indent += "│   ";
        self.pretty_print_expr(&cond_expr.cond.borrow());
        self.indent = orig_indent.clone();
        println!("{}├─▸ then expr:", self.indent);
        self.indent += "│   ";
        self.pretty_print_expr(&cond_expr.yes.borrow());
        self.indent = orig_indent;
        println!("{}└─▸ else expr:", self.indent);
        self.indent += "    ";
        self.pretty_print_expr(&cond_expr.no.borrow());
    }

    fn pretty_print_let_in_expr(&mut self, let_in_expr: &LetInExpr) {
        let bind_name = self.lexer.str_from_span(&let_in_expr.bind.0);
        println!("{}{BLUE}LetInExpr{END}", self.indent);
        println!("{}├─▸ bind name: {bind_name}", self.indent);
        println!("{}├─▸ bind value:", self.indent);
        let orig_indent = self.indent.clone();
        self.indent += "│   ";
        self.pretty_print_expr(&let_in_expr.bind.1.borrow());
        self.indent = orig_indent;
        println!("{}└─▸ expr:", self.indent);
        self.indent += "    ";
        self.pretty_print_expr(&let_in_expr.expr.borrow());
    }

    fn pretty_print_var_expr(&mut self, var_expr: &VarExpr) {
        let var = self.lexer.str_from_span(&var_expr.id);
        println!("{}{BLUE}VarExpr{END} (\"{var}\")", self.indent);
    }

    fn pretty_print_literal_expr(&mut self, literal_expr: &LiteralExpr) {
        let value = match literal_expr {
            LiteralExpr::Integer(val, _) => val.to_string(),
            LiteralExpr::Unit(_) => "unit".to_string(),
        };
        println!("{}{BLUE}LiteralExpr{END} ({value})", self.indent,);
    }
}

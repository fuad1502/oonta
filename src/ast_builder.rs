use crate::{
    ast::{ApplicationExpr, Ast, Bind, Expr, FunExpr, LetInExpr, Operator},
    lexer::Lexer,
    symbol::{NonTerminal, Rule, Span, Symbol, Terminal, TerminalClass},
};

pub struct AstBuilder<'a> {
    lexer: &'a Lexer,
    current_closure_ctx: Option<ClosureCtx>,
}

struct ClosureCtx {
    parent: Option<Box<ClosureCtx>>,
    params: Vec<Span>,
    captures: Vec<String>,
}

impl<'a> AstBuilder<'a> {
    pub fn new(lexer: &'a Lexer) -> Self {
        Self {
            lexer,
            current_closure_ctx: None,
        }
    }

    pub fn visit(&mut self, cst_root: &Symbol) -> Ast {
        let rule = extract_rule(cst_root);
        match rule.number {
            1 => self.visit_append_ast(&rule.components),
            2 => Ast::from(self.visit_bind(&rule.components[0])),
            _ => unreachable!(),
        }
    }

    fn visit_append_ast(&mut self, components: &[Symbol]) -> Ast {
        let mut ast = self.visit(&components[0]);
        let new_bind = self.visit_bind(&components[1]);
        ast.append(new_bind);
        ast
    }

    fn visit_bind(&mut self, symbol: &Symbol) -> Bind {
        let rule = extract_rule(symbol);
        match rule.number {
            3 => self.visit_var_bind(extract_components(&rule.components[0])),
            4 => self.visit_fun_bind(extract_components(&rule.components[0])),
            _ => unreachable!(),
        }
    }

    fn visit_var_bind(&mut self, components: &[Symbol]) -> Bind {
        let name = extract_span(&components[1]).clone();
        let expr = self.visit_expr(&components[3]);
        let start_pos = extract_span(&components[0]).start_pos();
        let end_pos = expr.span().end_pos();
        let span = Span::new(start_pos, end_pos);
        Bind { name, expr, span }
    }

    fn visit_fun_bind(&mut self, components: &[Symbol]) -> Bind {
        let name = extract_span(&components[1]).clone();
        let params = self.visit_params(&components[2]);
        self.push_closure_ctx(params);
        let expr = self.visit_expr(&components[4]);
        let expr = self.new_anonymous_fun_expr(expr);
        let start_pos = extract_span(&components[0]).start_pos();
        let end_pos = expr.span().end_pos();
        let span = Span::new(start_pos, end_pos);
        Bind { name, expr, span }
    }

    fn visit_expr(&mut self, symbol: &Symbol) -> Box<Expr> {
        match symbol {
            Symbol::NonTerminal(non_terminal) => self.visit_non_terminal_expr(non_terminal),
            Symbol::Terminal(terminal) => self.visit_terminal_expr(terminal),
        }
    }

    fn visit_non_terminal_expr(&mut self, non_terminal: &NonTerminal) -> Box<Expr> {
        match non_terminal.rule.number {
            10..=16 | 27..=30 => self.visit_expr(&non_terminal.rule.components[0]),
            17 => self.visit_anonymous_fun(&non_terminal.rule.components),
            18 => self.visit_expr(&non_terminal.rule.components[1]),
            19 => self.visit_let_in_expr(&non_terminal.rule.components),
            20..=23 => self.visit_binop_expr(&non_terminal.rule.components),
            24 => self.visit_append_application(&non_terminal.rule.components),
            25 | 26 => self.visit_application(&non_terminal.rule.components),
            _ => unreachable!(),
        }
    }

    fn visit_anonymous_fun(&mut self, components: &[Symbol]) -> Box<Expr> {
        let params = self.visit_params(&components[1]);
        self.push_closure_ctx(params);
        let expr = self.visit_expr(&components[3]);
        self.new_anonymous_fun_expr(expr)
    }

    fn visit_let_in_expr(&mut self, components: &[Symbol]) -> Box<Expr> {
        // TODO: Change LetInExpr to allow multiple binds and combine nested binds into one
        let bind_name = extract_span(&components[1]).clone();
        let bind_expr = self.visit_expr(&components[3]);
        let expr = self.visit_expr(&components[5]);
        let span = Span::new(
            extract_span(&components[0]).start_pos(),
            expr.span().end_pos(),
        );
        let let_in_expr = Expr::LetIn(LetInExpr {
            bind: (bind_name, bind_expr),
            expr,
            span,
        });
        Box::new(let_in_expr)
    }

    fn visit_application(&mut self, components: &[Symbol]) -> Box<Expr> {
        let fun = self.visit_expr(&components[0]);
        let arg = self.visit_expr(&components[1]);
        self.new_application_expr(*fun, *arg)
    }

    fn visit_append_application(&mut self, components: &[Symbol]) -> Box<Expr> {
        let app = self.visit_expr(&components[0]);
        let arg = self.visit_expr(&components[1]);
        if let Expr::Application(mut app_expr) = *app {
            let span = Span::new(app_expr.span.start_pos(), arg.span().end_pos());
            app_expr.span = span;
            app_expr.binds.push(*arg);
            Box::new(Expr::Application(app_expr))
        } else {
            unreachable!()
        }
    }

    fn visit_binop_expr(&mut self, components: &[Symbol]) -> Box<Expr> {
        let lhs = self.visit_expr(&components[0]);
        let op = match extract_terminal_class(&components[1]) {
            TerminalClass::Plus => Operator::Plus,
            TerminalClass::Minus => Operator::Minus,
            TerminalClass::Star => Operator::Star,
            TerminalClass::Slash => Operator::Slash,
            _ => unreachable!(),
        };
        let rhs = self.visit_expr(&components[2]);
        let span = Span::new(lhs.span().start_pos(), rhs.span().end_pos());
        Box::new(Expr::binop(op, lhs, rhs, span))
    }

    fn visit_terminal_expr(&mut self, terminal: &Terminal) -> Box<Expr> {
        match terminal.class() {
            TerminalClass::Number => self.new_integer_expr(terminal),
            TerminalClass::Identifier => self.new_var_expr(terminal),
            _ => unreachable!(),
        }
    }

    fn visit_param(&self, symbol: &Symbol) -> Span {
        extract_span(&extract_components(symbol)[0]).clone()
    }

    fn visit_params(&self, symbol: &Symbol) -> Vec<Span> {
        let rule = extract_rule(symbol);
        match rule.number {
            7 => {
                let mut param_list = self.visit_params(&rule.components[0]);
                let param = self.visit_param(&rule.components[1]);
                param_list.push(param);
                param_list
            }
            8 => vec![self.visit_param(&rule.components[0])],
            _ => unreachable!(),
        }
    }

    fn new_anonymous_fun_expr(&mut self, body: Box<Expr>) -> Box<Expr> {
        let closure_ctx = self.pop_closure_ctx();
        let mut params = closure_ctx.params;
        let mut captures = closure_ctx.captures;
        let span = body.span().clone();
        let body = if let Expr::Fun(FunExpr::Anonymous(mut anonymous_fun_expr)) = *body {
            // TODO: Also capture if the body is a let in expr with anonymous function as its body
            params.append(&mut anonymous_fun_expr.params);
            captures.append(&mut anonymous_fun_expr.captures);
            anonymous_fun_expr.body
        } else {
            body
        };
        Box::new(Expr::anonymous_fun(params, body, captures, span))
    }

    fn new_application_expr(&mut self, fun: Expr, arg: Expr) -> Box<Expr> {
        let span = Span::new(fun.span().start_pos(), arg.span().end_pos());
        let app_expr = ApplicationExpr {
            fun: Box::new(fun),
            binds: vec![arg],
            span,
        };
        Box::new(Expr::Application(app_expr))
    }

    fn new_integer_expr(&self, terminal: &Terminal) -> Box<Expr> {
        let lexeme = self.lexer.get_lexeme(terminal);
        let span = terminal.span().clone();
        let value = lexeme.parse().unwrap();
        Box::new(Expr::integer(value, span))
    }

    fn new_var_expr(&mut self, terminal: &Terminal) -> Box<Expr> {
        let name = self.lexer.get_lexeme(terminal);
        if let Some(ctx) = &mut self.current_closure_ctx
            && !ctx.is_name_in_params(name, self.lexer)
        {
            ctx.captures.push(name.to_string());
        }
        let id = terminal.span().clone();
        Box::new(Expr::var(id))
    }

    fn push_closure_ctx(&mut self, params: Vec<Span>) {
        let parent = match self.current_closure_ctx.take() {
            Some(parent) => Some(Box::new(parent)),
            None => None,
        };
        self.current_closure_ctx = Some(ClosureCtx {
            parent,
            params,
            captures: vec![],
        });
    }

    fn pop_closure_ctx(&mut self) -> ClosureCtx {
        let mut current_closure_ctx = self.current_closure_ctx.take().unwrap();
        if let Some(parent) = current_closure_ctx.parent.take() {
            self.current_closure_ctx = Some(*parent);
        };
        current_closure_ctx
    }
}

fn extract_span(symbol: &Symbol) -> &Span {
    if let Symbol::Terminal(t) = symbol {
        t.span()
    } else {
        panic!("extract_span should only be used with Terminal")
    }
}

fn extract_terminal_class(symbol: &Symbol) -> TerminalClass {
    if let Symbol::Terminal(t) = symbol {
        t.class()
    } else {
        panic!("extract_terminal_class should only be used with Terminal")
    }
}

fn extract_components(symbol: &Symbol) -> &Vec<Symbol> {
    &extract_rule(symbol).components
}

fn extract_rule(symbol: &Symbol) -> &Rule {
    if let Symbol::NonTerminal(NonTerminal { rule, .. }) = symbol {
        rule
    } else {
        panic!("extract_rule should only be used with NonTerminal")
    }
}

impl ClosureCtx {
    fn is_name_in_params(&self, name: &str, lexer: &Lexer) -> bool {
        for param in &self.params {
            if lexer.str_from_span(param) == name {
                return true;
            }
        }
        false
    }
}

use std::rc::Rc;

use crate::{
    ast::{AnonymousFunExpr, Ast, BinOpExpr, Bind, Expr, FunExpr, LiteralExpr, Operator, VarExpr},
    lexer::Lexer,
    symbol::{NonTerminal, NonTerminalClass, Rule, Span, Symbol, Terminal, TerminalClass},
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

    pub fn visit(&mut self, cst_root: &Symbol) -> Result<Ast, String> {
        if let Symbol::NonTerminal(NonTerminal {
            class: NonTerminalClass::StmtList,
            rule,
        }) = cst_root
        {
            match rule.number {
                1 => self.visit_append_ast(&rule.components),
                2 => Ok(Ast::from(self.visit_bind(&rule.components[0])?)),
                _ => unreachable!(),
            }
        } else {
            Err("Expected StmtList as root symbol".to_string())
        }
    }

    fn visit_append_ast(&mut self, components: &[Symbol]) -> Result<Ast, String> {
        let mut ast = self.visit(&components[0])?;
        let new_bind = self.visit_bind(&components[1])?;
        ast.append(new_bind);
        Ok(ast)
    }

    fn visit_bind(&mut self, symbol: &Symbol) -> Result<Bind, String> {
        let rule = extract_rule(symbol);
        match rule.number {
            3 => self.visit_var_bind(extract_components(&rule.components[0])),
            4 => self.visit_fun_bind(extract_components(&rule.components[0])),
            _ => unreachable!(),
        }
    }

    fn visit_var_bind(&mut self, components: &[Symbol]) -> Result<Bind, String> {
        let name = extract_span(&components[1]).clone();
        let expr = self.visit_expr(&components[3])?;
        let start_pos = extract_span(&components[0]).start_pos();
        let end_pos = expr.span().end_pos();
        let span = Span::new(start_pos, end_pos);
        Ok(Bind { name, expr, span })
    }

    fn visit_fun_bind(&mut self, components: &[Symbol]) -> Result<Bind, String> {
        let name = extract_span(&components[1]).clone();
        let params = self.visit_params(&components[2])?;
        self.push_closure_ctx(params);
        let expr = self.visit_expr(&components[4])?;
        let fun_expr = self.new_anonymous_fun_expr(expr);
        let start_pos = extract_span(&components[0]).start_pos();
        let end_pos = fun_expr.span().end_pos();
        let span = Span::new(start_pos, end_pos);
        Ok(Bind {
            name,
            expr: fun_expr,
            span,
        })
    }

    fn visit_params(&self, symbol: &Symbol) -> Result<Vec<Span>, String> {
        let rule = extract_rule(symbol);
        match rule.number {
            7 => {
                let mut param_list = self.visit_params(&rule.components[0])?;
                let param = self.visit_param(&rule.components[1])?;
                param_list.push(param);
                Ok(param_list)
            }
            8 => Ok(vec![self.visit_param(&rule.components[0])?]),
            _ => unreachable!(),
        }
    }

    fn visit_param(&self, symbol: &Symbol) -> Result<Span, String> {
        Ok(extract_span(&extract_components(symbol)[0]).clone())
    }

    fn visit_expr(&mut self, symbol: &Symbol) -> Result<Rc<dyn Expr>, String> {
        match symbol {
            Symbol::NonTerminal(non_terminal) => self.visit_non_terminal_expr(non_terminal),
            Symbol::Terminal(terminal) => self.visit_terminal_expr(terminal),
        }
    }

    fn visit_non_terminal_expr(
        &mut self,
        non_terminal: &NonTerminal,
    ) -> Result<Rc<dyn Expr>, String> {
        match non_terminal.rule.number {
            10..16 | 26 => self.visit_expr(&non_terminal.rule.components[0]),
            20..23 => self.visit_binop_expr(&non_terminal.rule.components),
            _ => unreachable!(),
        }
    }

    fn visit_binop_expr(&mut self, components: &[Symbol]) -> Result<Rc<dyn Expr>, String> {
        let lhs = self.visit_expr(&components[0])?;
        let op = match extract_terminal_class(&components[1]) {
            TerminalClass::Plus => Operator::Plus,
            TerminalClass::Minus => Operator::Minus,
            TerminalClass::Star => Operator::Star,
            TerminalClass::Slash => Operator::Slash,
            _ => unreachable!(),
        };
        let rhs = self.visit_expr(&components[2])?;
        let span = Span::new(lhs.span().start_pos(), rhs.span().end_pos());
        Ok(Rc::new(BinOpExpr { op, lhs, rhs, span }))
    }

    fn visit_terminal_expr(&mut self, terminal: &Terminal) -> Result<Rc<dyn Expr>, String> {
        match terminal.class() {
            TerminalClass::Number => Ok(self.new_integer_expr(terminal)),
            TerminalClass::Identifier => Ok(self.new_var_expr(terminal)),
            _ => unreachable!(),
        }
    }

    fn push_closure_ctx(&mut self, params: Vec<Span>) {
        match self.current_closure_ctx {
            Some(_) => todo!(),
            None => {
                _ = self.current_closure_ctx.insert(ClosureCtx {
                    parent: None,
                    params,
                    captures: vec![],
                })
            }
        }
    }

    fn new_anonymous_fun_expr(&mut self, body: Rc<dyn Expr>) -> Rc<dyn Expr> {
        let closure_ctx = self.pop_closure_ctx();
        let params = closure_ctx.params;
        let captures = closure_ctx.captures;
        let span = body.span().clone();
        Rc::new(FunExpr::Anonymous(AnonymousFunExpr {
            params,
            body,
            captures,
            span,
        }))
    }

    fn pop_closure_ctx(&mut self) -> ClosureCtx {
        let mut current_closure_ctx = self.current_closure_ctx.take().unwrap();
        match current_closure_ctx.parent.take() {
            Some(parent) => _ = self.current_closure_ctx.insert(*parent),
            None => (),
        };
        current_closure_ctx
    }

    fn new_integer_expr(&self, terminal: &Terminal) -> Rc<dyn Expr> {
        let lexeme = self.lexer.get_lexeme(terminal);
        let span = terminal.span().clone();
        let value = lexeme.parse().unwrap();
        Rc::new(LiteralExpr::Integer(value, span))
    }

    fn new_var_expr(&mut self, terminal: &Terminal) -> Rc<dyn Expr> {
        let name = self.lexer.get_lexeme(terminal);
        if let Some(ctx) = &mut self.current_closure_ctx {
            if !ctx.is_name_in_params(name, self.lexer) {
                ctx.captures.push(name.to_string());
            }
        }
        let id = terminal.span().clone();
        Rc::new(VarExpr { id })
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

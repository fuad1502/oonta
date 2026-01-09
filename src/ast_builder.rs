use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{
        ApplicationExpr, Ast, Bind, CondExpr, ConstructExpr, Expr, LetInExpr, LiteralExpr,
        Operator, Pattern, PatternMatchExpr, Stmt, TupleExpr,
    },
    custom_types::{Constructor, CustomTypes, Variant},
    lexer::Lexer,
    symbol::{NonTerminal, Rule, Span, Symbol, Terminal, TerminalClass},
    typ::{Primitive, Type},
};

pub struct AstBuilder<'a> {
    lexer: &'a Lexer,
    current_closure_ctx: Option<ClosureCtx>,
    ast: Ast,
    custom_types: CustomTypes,
}

struct ClosureCtx {
    parent: Option<Box<ClosureCtx>>,
    params: Vec<Span>,
    recursive_name: Option<Span>,
    captures: Vec<String>,
}

impl<'a> AstBuilder<'a> {
    pub fn new(lexer: &'a Lexer) -> Self {
        Self {
            lexer,
            current_closure_ctx: None,
            ast: Ast::default(),
            custom_types: CustomTypes::default(),
        }
    }

    pub fn build(mut self, cst_root: &Symbol) -> (Ast, CustomTypes) {
        self.visit_stmts(cst_root);
        (self.ast, self.custom_types)
    }

    fn visit_stmts(&mut self, cst_root: &Symbol) {
        let rule = extract_rule(cst_root);
        match rule.number {
            1 => {
                self.visit_stmts(&rule.components[0]);
                self.visit_stmt(&rule.components[1]);
            }
            2 => {
                self.visit_stmt(&rule.components[0]);
            }
            _ => unreachable!(),
        }
    }

    fn visit_stmt(&mut self, symbol: &Symbol) {
        let rule = extract_rule(symbol);
        let stmt = match rule.number {
            3 => Stmt::Bind(self.visit_var_bind(extract_components(&rule.components[0]))),
            4 => Stmt::Bind(self.visit_fun_bind(extract_components(&rule.components[0]), false)),
            5 => Stmt::Bind(self.visit_fun_bind(extract_components(&rule.components[0]), true)),
            6 => Stmt::Bind(self.visit_unit_bind(extract_components(&rule.components[0]))),
            7 => Stmt::TypeDecl(self.visit_type_decl(extract_components(&rule.components[0]))),
            _ => unreachable!(),
        };
        match stmt {
            Stmt::Bind(bind) => self.ast.insert_stmt(bind),
            Stmt::TypeDecl(variant) => self.custom_types.add_variant(variant),
        }
    }

    fn visit_unit_bind(&mut self, components: &[Symbol]) -> Bind {
        let expr = self.visit_expr(&components[3]);
        Bind { name: None, expr }
    }

    fn visit_var_bind(&mut self, components: &[Symbol]) -> Bind {
        let name = extract_span(&components[1]).clone();
        let expr = self.visit_expr(&components[3]);
        Bind {
            name: Some(name),
            expr,
        }
    }

    fn visit_fun_bind(&mut self, components: &[Symbol], recursive: bool) -> Bind {
        let name_idx = if recursive { 2 } else { 1 };
        let params_idx = if recursive { 3 } else { 2 };
        let expr_idx = if recursive { 5 } else { 4 };

        let name = extract_span(&components[name_idx]).clone();
        let params = self.visit_params(&components[params_idx]);
        self.push_closure_ctx(params, Some(name.clone()));
        let expr = self.visit_expr(&components[expr_idx]);

        let recursive_bind = if recursive {
            Some(self.lexer.str_from_span(&name).to_string())
        } else {
            None
        };
        let expr = self.new_fun_expr(expr, recursive_bind);
        Bind {
            name: Some(name),
            expr,
        }
    }

    fn visit_type_decl(&mut self, components: &[Symbol]) -> Variant {
        let name = self.lexer.str_from_span(extract_span(&components[1]));
        let constructors = self.visit_constructors(&components[3]);
        Variant::new(name.to_string(), constructors)
    }

    fn visit_expr(&mut self, symbol: &Symbol) -> Rc<RefCell<Expr>> {
        match symbol {
            Symbol::NonTerminal(non_terminal) => self.visit_non_terminal_expr(non_terminal),
            Symbol::Terminal(terminal) => self.visit_terminal_expr(terminal),
        }
    }

    fn visit_non_terminal_expr(&mut self, non_terminal: &NonTerminal) -> Rc<RefCell<Expr>> {
        match non_terminal.rule.number {
            24..=33 | 65..=69 => self.visit_expr(&non_terminal.rule.components[0]),
            34 | 35 => self.visit_construction_expr(&non_terminal.rule.components),
            36 => self.visit_if_then_else_expr(&non_terminal.rule.components),
            37 => self.visit_tuple_expr(&non_terminal.rule.components),
            40 => self.visit_anonymous_fun(&non_terminal.rule.components),
            41 => self.visit_expr(&non_terminal.rule.components[1]),
            42 => self.visit_let_in_expr(&non_terminal.rule.components),
            43 => self.visit_pattern_match_expr(&non_terminal.rule.components),
            53..=61 => self.visit_binop_expr(&non_terminal.rule.components),
            62 => self.visit_append_application(&non_terminal.rule.components),
            63 | 64 => self.visit_application(&non_terminal.rule.components),
            _ => unreachable!(),
        }
    }

    fn visit_construction_expr(&mut self, components: &[Symbol]) -> Rc<RefCell<Expr>> {
        let cons = extract_span(&components[0]).clone();
        let (arg, span) = if components.len() == 2 {
            let arg = self.visit_expr(&components[1]);
            let span = Span::new(cons.start_pos(), arg.borrow().span().end_pos());
            (Some(arg), span)
        } else {
            (None, cons.clone())
        };
        let construct_expr = ConstructExpr { cons, arg, span };
        Rc::new(RefCell::new(Expr::Construction(construct_expr)))
    }

    fn visit_if_then_else_expr(&mut self, components: &[Symbol]) -> Rc<RefCell<Expr>> {
        let cond = self.visit_expr(&components[1]);
        let yes = self.visit_expr(&components[3]);
        let no = self.visit_expr(&components[5]);
        let span = Span::new(
            extract_span(&components[0]).start_pos(),
            no.borrow().span().end_pos(),
        );
        let cond_expr = Expr::Conditional(CondExpr {
            cond,
            yes,
            no,
            span,
        });
        Rc::new(RefCell::new(cond_expr))
    }

    fn visit_pattern_match_expr(&mut self, components: &[Symbol]) -> Rc<RefCell<Expr>> {
        let matched = self.visit_expr(&components[1]);
        let branches = self.visit_branches(&components[3]);
        let span = Span::new(
            extract_span(&components[0]).start_pos(),
            branches.last().unwrap().1.borrow().span().end_pos(),
        );
        let pattern_match_expr = PatternMatchExpr {
            matched,
            branches,
            span,
        };
        Rc::new(RefCell::new(Expr::PatternMatch(pattern_match_expr)))
    }

    fn visit_tuple_expr(&mut self, components: &[Symbol]) -> Rc<RefCell<Expr>> {
        let elements = self.visit_expr_list(&components[1]);
        let start_pos = extract_span(&components[0]).start_pos();
        let end_pos = extract_span(&components[2]).end_pos();
        let span = Span::new(start_pos, end_pos);
        let tuple_expr = TupleExpr { elements, span };
        Rc::new(RefCell::new(Expr::Tuple(tuple_expr)))
    }

    fn visit_anonymous_fun(&mut self, components: &[Symbol]) -> Rc<RefCell<Expr>> {
        let params = self.visit_params(&components[1]);
        self.push_closure_ctx(params, None);
        let expr = self.visit_expr(&components[3]);
        self.new_fun_expr(expr, None)
    }

    fn visit_let_in_expr(&mut self, components: &[Symbol]) -> Rc<RefCell<Expr>> {
        // TODO: Change LetInExpr to allow multiple binds and combine nested binds into one
        let bind_name = extract_span(&components[1]).clone();
        let bind_expr = self.visit_expr(&components[3]);
        let expr = self.visit_expr(&components[5]);
        let span = Span::new(
            extract_span(&components[0]).start_pos(),
            expr.borrow().span().end_pos(),
        );
        let let_in_expr = Expr::LetIn(LetInExpr {
            bind: (bind_name, bind_expr),
            expr,
            span,
        });
        Rc::new(RefCell::new(let_in_expr))
    }

    fn visit_application(&mut self, components: &[Symbol]) -> Rc<RefCell<Expr>> {
        let fun = self.visit_expr(&components[0]);
        let arg = self.visit_expr(&components[1]);
        self.new_application_expr(fun, arg)
    }

    fn visit_append_application(&mut self, components: &[Symbol]) -> Rc<RefCell<Expr>> {
        let app = self.visit_expr(&components[0]);
        let arg = self.visit_expr(&components[1]);
        if let Expr::Application(app_expr) = &mut *app.borrow_mut() {
            let span = Span::new(app_expr.span.start_pos(), arg.borrow().span().end_pos());
            app_expr.span = span;
            app_expr.binds.push(arg);
        } else {
            unreachable!()
        }
        app
    }

    fn visit_binop_expr(&mut self, components: &[Symbol]) -> Rc<RefCell<Expr>> {
        let lhs = self.visit_expr(&components[0]);
        let op = match extract_terminal_class(&components[1]) {
            TerminalClass::Plus => Operator::Plus,
            TerminalClass::Minus => Operator::Minus,
            TerminalClass::Star => Operator::Star,
            TerminalClass::Slash => Operator::Slash,
            TerminalClass::Eq => Operator::Eq,
            TerminalClass::Lte => Operator::Lte,
            TerminalClass::Lt => Operator::Lt,
            TerminalClass::Gte => Operator::Gte,
            TerminalClass::Gt => Operator::Gt,
            _ => unreachable!(),
        };
        let rhs = self.visit_expr(&components[2]);
        let span = Span::new(
            lhs.borrow().span().start_pos(),
            rhs.borrow().span().end_pos(),
        );
        Rc::new(RefCell::new(Expr::binop(op, lhs, rhs, span)))
    }

    fn visit_terminal_expr(&mut self, terminal: &Terminal) -> Rc<RefCell<Expr>> {
        match terminal.class() {
            TerminalClass::Identifier => self.new_var_expr(terminal),
            TerminalClass::Number | TerminalClass::Unit => {
                let literal_expr = self.visit_literal_expr(terminal);
                let expr = Expr::Literal(literal_expr);
                Rc::new(RefCell::new(expr))
            }
            _ => unreachable!(),
        }
    }

    fn visit_literal_expr(&mut self, terminal: &Terminal) -> LiteralExpr {
        match terminal.class() {
            TerminalClass::Number => self.new_integer_expr(terminal),
            TerminalClass::Unit => self.new_unit_expr(terminal),
            _ => unreachable!(),
        }
    }

    fn visit_constructors(&self, symbol: &Symbol) -> Vec<Constructor> {
        let rule = extract_rule(symbol);
        match rule.number {
            16 => {
                let mut constructors = self.visit_constructors(&rule.components[0]);
                constructors.push(self.visit_constructor(&rule.components[2]));
                constructors
            }
            17 => vec![self.visit_constructor(&rule.components[0])],
            _ => unreachable!(),
        }
    }

    fn visit_constructor(&self, symbol: &Symbol) -> Constructor {
        let rule = extract_rule(symbol);
        match rule.number {
            18 => {
                let name = self.lexer.str_from_span(extract_span(&rule.components[0]));
                Constructor::no_arg(name.to_string())
            }
            19 => {
                let name = self.lexer.str_from_span(extract_span(&rule.components[0]));
                let typ = self.visit_type_string(&rule.components[2]);
                Constructor::new(name.to_string(), typ)
            }
            _ => unreachable!(),
        }
    }

    fn visit_type_string(&self, symbol: &Symbol) -> Rc<RefCell<Type>> {
        let rule = extract_rule(symbol);
        match rule.number {
            20 => {
                let typs = self.visit_type_strings(&rule.components[1]);
                Rc::new(RefCell::new(Type::Tuple(typs)))
            }
            21 => {
                let id = self.lexer.str_from_span(extract_span(&rule.components[0]));
                match id {
                    "int" => Rc::new(RefCell::new(Type::Primitive(Primitive::Integer))),
                    "bool" => Rc::new(RefCell::new(Type::Primitive(Primitive::Bool))),
                    _ => Rc::new(RefCell::new(Type::Custom(id.to_string()))),
                }
            }
            _ => unreachable!(),
        }
    }

    fn visit_type_strings(&self, symbol: &Symbol) -> Vec<Rc<RefCell<Type>>> {
        let rule = extract_rule(symbol);
        match rule.number {
            22 => {
                let mut typs = self.visit_type_strings(&rule.components[0]);
                typs.push(self.visit_type_string(&rule.components[2]));
                typs
            }
            23 => vec![
                self.visit_type_string(&rule.components[0]),
                self.visit_type_string(&rule.components[2]),
            ],
            _ => unreachable!(),
        }
    }

    fn visit_param(&self, symbol: &Symbol) -> Span {
        extract_span(&extract_components(symbol)[0]).clone()
    }

    fn visit_params(&self, symbol: &Symbol) -> Vec<Span> {
        let rule = extract_rule(symbol);
        match rule.number {
            13 => {
                let mut param_list = self.visit_params(&rule.components[0]);
                let param = self.visit_param(&rule.components[1]);
                param_list.push(param);
                param_list
            }
            14 => vec![self.visit_param(&rule.components[0])],
            _ => unreachable!(),
        }
    }

    fn visit_expr_list(&mut self, symbol: &Symbol) -> Vec<Rc<RefCell<Expr>>> {
        let rule = extract_rule(symbol);
        match rule.number {
            38 => {
                let mut expr_list = self.visit_expr_list(&rule.components[0]);
                let expr = self.visit_expr(&rule.components[2]);
                expr_list.push(expr);
                expr_list
            }
            39 => {
                vec![
                    self.visit_expr(&rule.components[0]),
                    self.visit_expr(&rule.components[2]),
                ]
            }
            _ => unreachable!(),
        }
    }

    fn visit_branches(&mut self, symbol: &Symbol) -> Vec<(Pattern, Rc<RefCell<Expr>>)> {
        let rule = extract_rule(symbol);
        match rule.number {
            44 => {
                let mut branches = self.visit_branches(&rule.components[0]);
                let branch = self.visit_branch(&rule.components[2]);
                branches.push(branch);
                branches
            }
            45 => vec![self.visit_branch(&rule.components[0])],
            _ => unreachable!(),
        }
    }

    fn visit_branch(&mut self, symbol: &Symbol) -> (Pattern, Rc<RefCell<Expr>>) {
        let components = extract_components(symbol);
        let pattern = self.visit_pattern(&components[0]);
        let expr = self.visit_expr(&components[2]);
        (pattern, expr)
    }

    fn visit_pattern(&mut self, symbol: &Symbol) -> Pattern {
        let rule = extract_rule(symbol);
        match rule.number {
            47 => {
                let patterns = self.visit_patterns(&rule.components[1]);
                Pattern::Tuple(patterns)
            }
            48 => self.visit_singular_pattern(&rule.components[0]),
            _ => unreachable!(),
        }
    }

    fn visit_patterns(&mut self, symbol: &Symbol) -> Vec<Pattern> {
        let rule = extract_rule(symbol);
        match rule.number {
            49 => {
                let mut patterns = self.visit_patterns(&rule.components[0]);
                let pattern = self.visit_pattern(&rule.components[2]);
                patterns.push(pattern);
                patterns
            }
            50 => vec![
                self.visit_pattern(&rule.components[0]),
                self.visit_pattern(&rule.components[2]),
            ],
            _ => unreachable!(),
        }
    }

    fn visit_singular_pattern(&mut self, symbol: &Symbol) -> Pattern {
        let rule = extract_rule(symbol);
        match rule.number {
            51 => {
                let span = extract_span(&rule.components[0]);
                match self.lexer.str_from_span(span) {
                    "_" => Pattern::None,
                    _ => Pattern::Identifier(span.clone()),
                }
            }
            52 => {
                let literal = &rule.components[0];
                let terminal = &extract_components(literal)[0];
                Pattern::Literal(self.visit_literal_expr(extract_terminal(terminal)))
            }
            _ => unreachable!(),
        }
    }

    fn new_fun_expr(
        &mut self,
        body: Rc<RefCell<Expr>>,
        recursive_bind: Option<String>,
    ) -> Rc<RefCell<Expr>> {
        let closure_ctx = self.pop_closure_ctx();
        let mut params = closure_ctx.params;
        let mut captures = closure_ctx.captures;
        let span = body.borrow().span().clone();
        let body = if let Expr::Fun(fun_expr) = &mut *body.borrow_mut() {
            // TODO: Also capture if the body is a let in expr with anonymous function as its body
            params.append(&mut fun_expr.params);
            captures.append(&mut fun_expr.captures);
            fun_expr.body.clone()
        } else {
            body
        };
        Rc::new(RefCell::new(Expr::fun(
            params,
            body,
            captures,
            recursive_bind,
            span,
        )))
    }

    fn new_application_expr(
        &mut self,
        fun: Rc<RefCell<Expr>>,
        arg: Rc<RefCell<Expr>>,
    ) -> Rc<RefCell<Expr>> {
        let span = Span::new(
            fun.borrow().span().start_pos(),
            arg.borrow().span().end_pos(),
        );
        let app_expr = ApplicationExpr {
            fun,
            binds: vec![arg],
            span,
        };
        Rc::new(RefCell::new(Expr::Application(app_expr)))
    }

    fn new_unit_expr(&self, terminal: &Terminal) -> LiteralExpr {
        let span = terminal.span().clone();
        LiteralExpr::Unit(span)
    }

    fn new_integer_expr(&self, terminal: &Terminal) -> LiteralExpr {
        let lexeme = self.lexer.get_lexeme(terminal);
        let span = terminal.span().clone();
        let value = lexeme.parse().unwrap();
        LiteralExpr::Integer(value, span)
    }

    fn new_var_expr(&mut self, terminal: &Terminal) -> Rc<RefCell<Expr>> {
        let name = self.lexer.get_lexeme(terminal);
        if let Some(ctx) = &mut self.current_closure_ctx
            && !ctx.is_in_params(name, self.lexer)
            && !ctx.is_recursive_name(name, self.lexer)
        {
            ctx.captures.push(name.to_string());
        }
        let id = terminal.span().clone();
        Rc::new(RefCell::new(Expr::var(id)))
    }

    fn push_closure_ctx(&mut self, params: Vec<Span>, recursive_name: Option<Span>) {
        let parent = self.current_closure_ctx.take().map(Box::new);
        self.current_closure_ctx = Some(ClosureCtx {
            parent,
            params,
            recursive_name,
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
    extract_terminal(symbol).span()
}

fn extract_terminal_class(symbol: &Symbol) -> TerminalClass {
    extract_terminal(symbol).class()
}

fn extract_terminal(symbol: &Symbol) -> &Terminal {
    if let Symbol::Terminal(t) = symbol {
        t
    } else {
        panic!("extract_terminal should only be used with Terminal")
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
    fn is_in_params(&self, name: &str, lexer: &Lexer) -> bool {
        for param in &self.params {
            if lexer.str_from_span(param) == name {
                return true;
            }
        }
        false
    }

    fn is_recursive_name(&self, name: &str, lexer: &Lexer) -> bool {
        if let Some(span) = &self.recursive_name {
            let recursive_name = lexer.str_from_span(span);
            if name == recursive_name {
                return true;
            }
        }
        false
    }
}

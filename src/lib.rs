mod application_visitor;
mod ast;
mod ast_builder;
pub mod cmd;
pub mod driver;
mod ir_builder;
pub mod terminal_colors;
mod typ;

#[rustfmt::skip]
mod lexer {
    include!(concat!(env!("OUT_DIR"), "/lexer.rs"));
}
#[rustfmt::skip]
mod parser{
    include!(concat!(env!("OUT_DIR"), "/parser.rs"));
}
#[allow(unused)]
#[rustfmt::skip]
mod symbol{
    include!(concat!(env!("OUT_DIR"), "/symbol.rs"));
}

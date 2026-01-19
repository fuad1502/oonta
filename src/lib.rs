pub mod driver;

mod ast;
mod pass;
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

use std::path::PathBuf;

fn main() {
    let file = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("ocaml")
        .join("main.ml");
    match oonta::compile(&file) {
        Ok(()) => (),
        Err(e) => eprintln!("{e}"),
    }
}

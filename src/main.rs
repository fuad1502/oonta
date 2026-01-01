use std::path::PathBuf;

fn main() {
    let src_file = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("ocaml")
        .join("main.ml");
    let out_file = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test.ll");
    match oonta::compile(&src_file, &out_file) {
        Ok(()) => (),
        Err(e) => eprintln!("{e}"),
    }
}

extern crate lang_c;
extern crate lang_c_corrode;

use std::env;

fn main() {
    let text = env::args().nth(1).unwrap();
    let ast = lang_c::parser::declaration(&text, &mut lang_c::env::Env::with_gnu(true)).unwrap();
    let alloc = &lang_c_corrode::Alloc::new();
    let hir = lang_c_corrode::interpret_declaration(alloc, &mut lang_c_corrode::Env::new(), &ast, lang_c_corrode::Storage::Auto).unwrap();
    println!("{:#?}", hir);
}
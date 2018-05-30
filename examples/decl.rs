extern crate lang_c;
extern crate lang_c_corrode;

use std::env;

fn main() {
    let text = env::args().nth(1).unwrap();
    let ast = lang_c::driver::parse_preprocessed(&Default::default(), text).unwrap();
    let alloc = &lang_c_corrode::Alloc::new();
    let ir = lang_c_corrode::Unit::from_ast(alloc, &ast.unit).unwrap();
    println!("{:#?}", ir);
}

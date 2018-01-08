extern crate lang_c;
extern crate lang_c_corrode;

use std::env;

fn main() {
    let text = env::args().nth(1).unwrap();
    let ast = lang_c::driver::parse_preprocessed(&Default::default(), text).unwrap();
    let alloc = &lang_c_corrode::Alloc::new();
    let hir = lang_c_corrode::interpret_translation_unit(
        alloc,
        &mut lang_c_corrode::Env::new(),
        &ast.unit,
    ).unwrap();
    let buf = &mut String::new();
    lang_c_corrode::gen::write_translation_unit(&mut Default::default(), buf, &hir).unwrap();
    println!("{}", buf);
}

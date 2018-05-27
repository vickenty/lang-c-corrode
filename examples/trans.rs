extern crate lang_c;
extern crate lang_c_corrode;

use std::env;
use std::io::stdout;

use lang_c_corrode::fmt::{ToCode, Writer};

fn main() {
    let text = env::args().nth(1).unwrap();
    let ast = lang_c::driver::parse_preprocessed(&Default::default(), text).unwrap();
    let alloc = &lang_c_corrode::Alloc::new();
    let ir = lang_c_corrode::interpret_translation_unit(alloc, &ast.unit).unwrap();
    ir.run_passes().unwrap();
    let stdout = stdout();
    ir.to_code(&mut Writer::new(&mut stdout.lock()));
}

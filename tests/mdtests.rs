extern crate lang_c;
extern crate lang_c_corrode;
extern crate syn;

use lang_c_corrode::fmt::ToCode;

fn translate(s: &str) -> String {
    let alloc = &lang_c_corrode::Alloc::new();
    let mut buf = Vec::new();

    let parse = lang_c::driver::parse_preprocessed(&Default::default(), s.into()).unwrap();
    let unit = lang_c_corrode::Unit::from_ast(alloc, &parse.unit).unwrap();
    unit.run_passes().unwrap();
    unit.to_code(&mut lang_c_corrode::fmt::Writer::new(&mut buf));

    let s = String::from_utf8(buf).unwrap();
    syn::parse_str::<syn::File>(&s).unwrap();
    s
}

macro_rules! check {
    ($c:expr, $r:expr) => (assert_eq!(::translate($c), $r));
}

#[path="../mdtests/decl.rs"] mod decl;

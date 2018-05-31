extern crate lang_c;
extern crate lang_c_corrode;
extern crate syn;

use lang_c_corrode::fmt::ToCode;

fn translate(s: &str) -> String {
    let alloc = &lang_c_corrode::Alloc::new();
    let mut buf = Vec::new();

    let parse = lang_c::driver::parse_preprocessed(&Default::default(), s.into()).unwrap();
    let unit = lang_c_corrode::Unit::from_ast(alloc, &parse.unit).unwrap();

    #[cfg(feature = "old_trans")]
    {
        unit.run_passes().unwrap();
        unit.to_code(&mut lang_c_corrode::fmt::Writer::new(&mut buf));
    }
    #[cfg(not(feature = "old_trans"))]
    {
        let ir2 = lang_c_corrode::r::unit::Unit::from_c(&unit);
        ir2.to_code(&mut lang_c_corrode::fmt::Writer::new(&mut buf));
    }
    let s = String::from_utf8(buf).unwrap();
    if let res @ Err(_) = syn::parse_str::<syn::File>(&s) {
        eprintln!("```rust\n{}\n```", s);
        res.unwrap();
    }

    s
}

macro_rules! check {
    ($c:expr, $r:expr) => {
        assert_eq!(::translate($c), $r)
    };
}

#[path = "../mdtests/decl.rs"]
mod decl;

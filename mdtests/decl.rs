#![cfg_attr(rustfmt, rustfmt_skip)]
#[test]
fn static_int() {
    check!("static int x;\n", "#[no_mangle]\npub static mut x: c_int = (0) as c_int;\n");
}
#[test]
fn static_pointer() {
    check!("typedef int *ip;\nstatic ip x;\n", "#[no_mangle]\npub static mut x: *mut c_int = (0) as *mut c_int;\n");
}
#[test]
fn extern_var() {
    check!("extern char x;\n", "extern {\n    #[no_mangle]\n    pub static mut x: c_char;\n}\n");
}
#[test]
fn struct_simple() {
    check!("extern struct a { int b; } c;\n", "extern {\n    pub static mut c: a;\n}\n#[repr(C)]\npub struct a {\n    pub b: c_int,\n}\n");
}
#[test]
fn struct_anon() {
    check!("struct {\n    union {\n        int a;\n        float b;\n    };\n    struct {\n        int c, d;\n    };\n} v;\n", "#[no_mangle]\npub static mut v: Generated_0 = Generated_0 {\n    anon_0: Generated_1 {\n        a: (0) as c_int,\n    },\n    anon_1: Generated_2 {\n        c: (0) as c_int,\n        d: (0) as c_int,\n    },\n};\n#[repr(C)]\npub struct Generated_0 {\n    pub anon_0: Generated_1,\n    pub anon_1: Generated_2,\n}\n#[repr(C)]\npub union Generated_1 {\n    pub a: c_int,\n    pub b: c_float,\n}\n#[repr(C)]\npub struct Generated_2 {\n    pub c: c_int,\n    pub d: c_int,\n}\n");
}
#[test]
fn struct_anon_clobber() {
    check!("int Generated_0;\nstruct {\n    int anon_0;\n    struct {\n        int anon_1;\n    };\n} Generated_1;\nstruct Generated_0 *p;\n", "#[no_mangle]\npub static mut Generated_0: c_int = (0) as c_int;\n#[no_mangle]\npub static mut Generated_1: Generated_2 = Generated_2 {\n    anon_0: (0) as c_int,\n    anon_1: Generated_3 {\n        anon_1: (0) as c_int,\n    },\n};\n#[no_mangle]\npub static mut p: *mut Generated_0_ = (0) as *mut Generated_0_;\n#[repr(C)]\npub struct Generated_2 {\n    pub anon_0: c_int,\n    pub anon_1: Generated_3,\n}\n#[repr(C)]\npub struct Generated_3 {\n    pub anon_1: c_int,\n}\npub enum Generated_0_{}\n");
}
#[test]
fn literal_int() {
    check!("extern int a = 1;\n", "#[no_mangle]\npub static mut a: c_int = (1) as c_int;\n");
}
#[test]
fn literal_int_suffix() {
    check!("unsigned long long a = 1llu;\n", "#[no_mangle]\npub static mut a: c_ulonglong = (1) as c_ulonglong;\n");
}
#[test]
fn literal_float() {
    check!("double f = 0.1;\n", "#[no_mangle]\npub static mut f: c_double = (0.1) as c_double;\n");
}
#[test]
fn literal_float_suffix() {
    check!("float f = 0.1f;\n", "#[no_mangle]\npub static mut f: c_float = (0.1) as c_float;\n");
}
#[test]
fn literal_float_suffix_coerce() {
    check!("double f = 0.1f;\n", "#[no_mangle]\npub static mut f: c_double = ((0.1) as c_float) as c_double;\n");
}
#[test]
fn expr_simple() {
    check!("int f = -1 + 1;\n", "#[no_mangle]\npub static mut f: c_int = (-((1) as c_int)) + ((1) as c_int);\n");
}
#[test]
fn expr_integer_zero() {
    check!("int x = 0;\n", "#[no_mangle]\npub static mut x: c_int = (0) as c_int;\n");
}
#[test]
fn expr_pointer_zero() {
    check!("int *p = 0;\n", "#[no_mangle]\npub static mut p: *mut c_int = ((0) as c_int) as *mut c_int;\n");
}
#[test]
fn integer_literal_expand() {
    check!("char x = 1000;\n", "#[no_mangle]\npub static mut x: c_char = ((1000) as c_int) as c_char;\n");
}

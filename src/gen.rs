use super::expr;
use super::{Function, FunctionTy, Item, Struct, StructKind, Type, Unit, Variable};

use fmt::{Formatter, ToCode};

impl<'a> ToCode for Unit<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        for item in &self.items {
            item.to_code(fmt);
        }
    }
}

impl<'a> ToCode for Item<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        match *self {
            Item::Struct(s) => s.to_code(fmt),
            Item::Variable(var) => var.to_code(fmt),
            Item::Function(f) => f.to_code(fmt),
        }
    }
}

impl<'a> ToCode for Variable<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        if self.is_defined() {
            static_define(self, fmt);
        } else {
            static_extern(self, fmt);
        }
    }
}

fn static_define(var: &Variable, fmt: &mut Formatter) {
    tokln!(fmt, "#[no_mangle]");
    tokln!(fmt, "pub static mut ", var.name, ": ", var.ty.ty, " = ", var.initial, ";");
}

fn static_extern<'a, 'w>(var: &Variable, fmt: &mut Formatter) {
    tokln!(fmt, "extern {");
    block!(fmt, {
        tokln!(fmt, "pub static mut ", var.name, ": ", var.ty.ty, ";");
    });
    tokln!(fmt, "}");
}

impl<'a> ToCode for expr::Expression<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        match *self {
            expr::Expression::Constant(ref c) => c.to_code(fmt),
            expr::Expression::Unary(ref e) => e.to_code(fmt),
            expr::Expression::Binary(ref e) => e.to_code(fmt),
            expr::Expression::Struct(ref v) => v.to_code(fmt),
            expr::Expression::Cast(ref c) => c.to_code(fmt),
        }
    }
}

impl<'a> ToCode for expr::Constant<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        match *self {
            expr::Constant::Integer(ref i) => i.to_code(fmt),
            expr::Constant::Float(ref f) => f.to_code(fmt),
        }
    }
}

impl<'a> ToCode for expr::Integer<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        toks!(fmt, "(", self.base, self.number, ") as ", self.ty);
    }
}

impl ToCode for expr::IntegerBase {
    fn to_code(&self, fmt: &mut Formatter) {
        match *self {
            expr::IntegerBase::Decimal => {}
            expr::IntegerBase::Octal => toks!(fmt, "0o"),
            expr::IntegerBase::Hexademical => toks!(fmt, "0x"),
        }
    }
}

impl<'a> ToCode for expr::Float<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        toks!(fmt, "(", self.number, ") as ", self.ty);
    }
}

impl<'a> ToCode for expr::Unary<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        toks!(fmt, self.operator, "(", self.operand, ")");
    }
}

impl ToCode for expr::UnaryOperator {
    fn to_code(&self, fmt: &mut Formatter) {
        toks!(
            fmt,
            match *self {
                expr::UnaryOperator::Plus => "+",
                expr::UnaryOperator::Minus => "-",
                expr::UnaryOperator::Complement => "!",
                expr::UnaryOperator::Negate => "!",
                _ => unimplemented!(),
            }
        );
    }
}

impl<'a> ToCode for expr::Binary<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        toks!(fmt, "(", self.lhs, ") ", self.operator, " (", self.rhs, ")");
    }
}

impl ToCode for expr::BinaryOperator {
    fn to_code(&self, fmt: &mut Formatter) {
        toks!(
            fmt,
            match *self {
                expr::BinaryOperator::Multiply => "*",
                expr::BinaryOperator::Divide => "/",
                expr::BinaryOperator::Modulo => "%",
                expr::BinaryOperator::Plus => "+",
                expr::BinaryOperator::Minus => "-",
                expr::BinaryOperator::ShiftLeft => "<<",
                expr::BinaryOperator::ShiftRight => ">>",
                _ => unimplemented!(),
            }
        );
    }
}

impl<'a> ToCode for expr::StructValue<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        tokln!(fmt, self.def.rust_name, " {");
        block!(fmt, {
            if let Some(ref fields) = *self.def.fields.borrow() {
                for field in fields {
                    if let Some(ref val) = self.values.get(&field.id()) {
                        tokln!(fmt, field.rust_name, ": ", val, ",");
                    }
                }
            }
        });
        toks!(fmt, "}");
    }
}

impl<'a> ToCode for expr::Cast<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        toks!(fmt, "(", self.expr, ") as ", self.ty);
    }
}

impl<'a> ToCode for Struct<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        if self.is_complete_ty() {
            tokln!(fmt, "#[repr(C)]");
            tokln!(fmt, "pub ", self.kind, " ", self.rust_name, " {");
            block!(fmt, {
                if let Some(ref fields) = *self.fields.borrow() {
                    for field in fields {
                        tokln!(fmt, "pub ", field.rust_name, ": ", field.ty.ty, ",");
                    }
                }
            });
            tokln!(fmt, "}");
        } else {
            tokln!(fmt, "pub enum ", self.rust_name, "{}");
        }
    }
}

impl ToCode for StructKind {
    fn to_code(&self, fmt: &mut Formatter) {
        toks!(
            fmt,
            match *self {
                StructKind::Struct => "struct",
                StructKind::Union => "union",
            }
        );
    }
}

impl<'a> ToCode for Type<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        match *self {
            Type::Void => toks!(fmt, "c_void"),
            Type::Char => toks!(fmt, "c_char"),
            Type::SChar => toks!(fmt, "c_schar"),
            Type::UChar => toks!(fmt, "c_uchar"),
            Type::SInt => toks!(fmt, "c_int"),
            Type::UInt => toks!(fmt, "c_uint"),
            Type::SShort => toks!(fmt, "c_short"),
            Type::UShort => toks!(fmt, "c_ushort"),
            Type::SLong => toks!(fmt, "c_long"),
            Type::ULong => toks!(fmt, "c_ulong"),
            Type::SLongLong => toks!(fmt, "c_longlong"),
            Type::ULongLong => toks!(fmt, "c_ulonglong"),
            Type::Float => toks!(fmt, "c_float"),
            Type::Double => toks!(fmt, "c_double"),
            Type::Struct(s) => toks!(fmt, s.rust_name),
            Type::Pointer(ref ty) => toks!(fmt, "*mut ", ty.ty),
            _ => unimplemented!(),
        }
    }
}

impl<'a> ToCode for Function<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        tokln!(fmt, "extern \"C\" {");
        block!(fmt, {
            tokln!(fmt, "pub fn ", self.name, self.ty, ";");
        });
        tokln!(fmt, "}");
    }
}

impl<'a> ToCode for FunctionTy<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        toks!(fmt, "(");
        for p in &self.parameters {
            toks!(fmt, p.ty.ty, ", ");
        }
        if self.variadic {
            toks!(fmt, "...");
        }
        toks!(fmt, ")");

        match self.return_type.ty {
            Type::Void => (),
            ref ty => toks!(fmt, " -> ", ty),
        }
    }
}

#[cfg(test)]
use super::{interpret_translation_unit, Alloc};
#[cfg(test)]
use {fmt, lang_c, syn};

#[cfg(test)]
fn translate(s: &str) -> String {
    let alloc = &Alloc::new();
    let mut buf = Vec::new();

    let parse = lang_c::driver::parse_preprocessed(&Default::default(), s.into()).unwrap();
    let unit = interpret_translation_unit(alloc, &parse.unit).unwrap();
    unit.run_passes().unwrap();
    unit.to_code(&mut fmt::Writer::new(&mut buf));

    let s = String::from_utf8(buf).unwrap();
    syn::parse_str::<syn::File>(&s).unwrap();
    s
}

#[cfg(test)]
macro_rules! check {
    ($a:expr, $b:expr) => {
        assert_eq!(translate($a), $b)
    };
}

#[test]
fn test_static() {
    check!(
        "typedef int *ip; static ip x;",
        "#[no_mangle]\n\
         pub static mut x: *mut c_int = (0) as *mut c_int;\n"
    );
}

#[test]
fn test_extern() {
    check!("extern char x;", "extern {\npub static mut x: c_char;\n}\n");
}

#[test]
fn test_struct() {
    check!(
        "extern struct a { int b; } c;",
        "extern {\npub static mut c: a;\n}\n\
         #[repr(C)]\n\
         pub struct a {\npub b: c_int,\n}\n\
         "
    );
}

#[test]
fn test_anon_struct() {
    check!(
        "struct { union { int a; float b; }; struct { int c, d; }; } v;",
        "\
         #[no_mangle]\n\
         pub static mut v: Generated_0 = Generated_0 {\n\
         anon_0: Generated_1 {\na: (0) as c_int,\n},\n\
         anon_1: Generated_2 {\nc: (0) as c_int,\nd: (0) as c_int,\n},\n\
         };\n\
         #[repr(C)]\n\
         pub struct Generated_0 {\npub anon_0: Generated_1,\npub anon_1: Generated_2,\n}\n\
         #[repr(C)]\n\
         pub union Generated_1 {\npub a: c_int,\npub b: c_float,\n}\n\
         #[repr(C)]\n\
         pub struct Generated_2 {\npub c: c_int,\npub d: c_int,\n}\n\
         "
    );
}

#[test]
fn test_anon_struct_clobber() {
    check!(
        "int Generated_0; struct { int anon_0; struct { int anon_1; }; } Generated_1; struct Generated_0 *p;",
        "#[no_mangle]\n\
         pub static mut Generated_0: c_int = (0) as c_int;\n\
         #[no_mangle]\n\
         pub static mut Generated_1: Generated_2 = Generated_2 {\n\
         anon_0: (0) as c_int,\n\
         anon_1: Generated_3 {\nanon_1: (0) as c_int,\n},\n\
         };\n\
         #[no_mangle]\n\
         pub static mut p: *mut Generated_0_ = (0) as *mut Generated_0_;\n\
         #[repr(C)]\npub struct Generated_2 {\npub anon_0: c_int,\npub anon_1: Generated_3,\n}\n\
         #[repr(C)]\npub struct Generated_3 {\npub anon_1: c_int,\n}\n\
         pub enum Generated_0_{}\n\
         "
    )
}

#[test]
fn test_int_init() {
    check!(
        "extern int a = 1;",
        "#[no_mangle]\n\
         pub static mut a: c_int = (1) as c_int;\n"
    );
    check!(
        "unsigned long long a = 1llu;",
        "#[no_mangle]\n\
         pub static mut a: c_ulonglong = (1) as c_ulonglong;\n"
    );
}

#[test]
fn test_int_const_expand() {
    use std::mem::size_of;
    use std::os::raw::{c_int, c_long, c_uint};

    if size_of::<c_int>() < size_of::<c_long>() {
        let num = c_int::max_value() as c_uint + 1;
        let c_src = format!("int a = {};", num);
        let r_src = format!(
            "#[no_mangle]\npub static mut a: c_int = (({}) as c_long) as c_int;\n",
            num
        );
        check!(&c_src[..], &r_src[..]);
    }
}

#[test]
fn float_const() {
    check!(
        "double f = 0.1;",
        "#[no_mangle]\npub static mut f: c_double = (0.1) as c_double;\n"
    );

    check!(
        "float f = 0.1f;",
        "#[no_mangle]\npub static mut f: c_float = (0.1) as c_float;\n"
    );

    check!(
        "double f = 0.1f;",
        "#[no_mangle]\npub static mut f: c_double = ((0.1) as c_float) as c_double;\n"
    );
}

#[test]
fn epxr() {
    check!(
        "int f = -1 + 1;",
        "#[no_mangle]\npub static mut f: c_int = (-((1) as c_int)) + ((1) as c_int);\n"
    );
}

#[test]
fn int_zero() {
    check!(
        "int x = 0;",
        "#[no_mangle]\npub static mut x: c_int = (0) as c_int;\n"
    );
    check!(
        "int *p = 0;",
        "#[no_mangle]\npub static mut p: *mut c_int = ((0) as c_int) as *mut c_int;\n"
    );
}

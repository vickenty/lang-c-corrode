use std::io;
use std::io::Write;
use std::fmt;

use super::expr;
use super::{Field, Function, FunctionTy, Item, Ref, Struct, StructKind, Type, Unit, Variable};

pub type Result = io::Result<()>;

pub struct Env<'w> {
    output: &'w mut (io::Write + 'w),
}

impl<'a, 'w> io::Write for Env<'w> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.output.write(buf)
    }
    fn flush(&mut self) -> io::Result<()> {
        self.output.flush()
    }
    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        self.output.write_all(buf)
    }
    fn write_fmt(&mut self, fmt: fmt::Arguments) -> io::Result<()> {
        self.output.write_fmt(fmt)
    }
}

impl<'a, 'w> Env<'w> {
    pub fn new(output: &'w mut io::Write) -> Env<'w> {
        Env { output: output }
    }
}

pub fn write_translation_unit<'a, 'w>(env: &mut Env<'w>, unit: &Unit<'a>) -> Result {
    for item in &unit.items {
        write_item(env, item)?;
    }

    Ok(())
}

pub fn write_item<'a, 'w>(env: &mut Env<'w>, item: &Item<'a>) -> Result {
    match *item {
        Item::Struct(s) => write_struct(env, s),
        Item::Variable(var) => write_variable(env, var),
        Item::Function(f) => write_function(env, f),
    }
}

pub fn write_variable<'a, 'w>(env: &mut Env<'w>, var: Ref<'a, Variable<'a>>) -> Result {
    if var.is_defined() {
        write_static_define(env, var)
    } else {
        write_static_extern(env, var)
    }
}

fn write_static_define<'a, 'w>(env: &mut Env<'w>, var: Ref<'a, Variable<'a>>) -> Result {
    writeln!(env.output, "#[no_mangle]")?;
    write!(env, "pub static mut {}: ", var.name)?;
    write_type_ref(env, &var.ty.ty)?;
    write!(env, " = ")?;
    write_expr(
        env,
        var.initial.borrow().as_ref().expect("uninitialized static"),
    )?;
    writeln!(env.output, ";")
}

fn write_static_extern<'a, 'w>(env: &mut Env<'w>, var: Ref<'a, Variable<'a>>) -> Result {
    write!(env, "extern {{ pub static mut {}: ", var.name)?;
    write_type_ref(env, &var.ty.ty)?;
    writeln!(env.output, "; }}")
}

pub fn write_expr<'a, 'w>(env: &mut Env<'w>, expr: &expr::Expression<'a>) -> Result {
    match *expr {
        expr::Expression::Constant(ref c) => write_const(env, c),
        expr::Expression::Unary(ref e) => write_expr_unary(env, e),
        expr::Expression::Binary(ref e) => write_expr_binary(env, e),
        expr::Expression::Struct(ref v) => write_expr_struct_val(env, v),
        expr::Expression::Cast(ref c) => write_expr_cast(env, c),
    }
}

pub fn write_const<'a, 'w>(env: &mut Env<'w>, c: &expr::Constant<'a>) -> Result {
    match *c {
        expr::Constant::Integer(ref i) => {
            write_cast_expr(env, &i.ty, |env| {
                match i.base {
                    expr::IntegerBase::Octal => write!(env, "0o")?,
                    expr::IntegerBase::Hexademical => write!(env, "0x")?,
                    expr::IntegerBase::Decimal => (),
                }
                write!(env, "{}", i.number)
            })?;
        }

        expr::Constant::Float(ref f) => {
            write_cast_expr(env, &f.ty, |env| write!(env, "{}", f.number))?;
        }
    }
    Ok(())
}

fn write_cast_expr<'a, 'w, F>(env: &mut Env<'w>, ty: &Type<'a>, mut f: F) -> Result
where
    F: FnMut(&mut Env<'w>) -> Result,
{
    write!(env, "(")?;
    f(env)?;
    write!(env, ") as ")?;
    write_type_ref(env, ty)?;
    Ok(())
}

fn write_expr_unary<'a, 'w>(env: &mut Env<'w>, e: &expr::Unary<'a>) -> Result {
    let op = match e.operator {
        expr::UnaryOperator::Minus => "-",
        expr::UnaryOperator::Plus => "+",
        expr::UnaryOperator::Complement => "!",
        expr::UnaryOperator::Negate => "!",
        _ => unimplemented!(),
    };

    write!(env, "{}(", op)?;
    write_expr(env, &e.operand)?;
    write!(env, ")")
}

fn write_expr_binary<'a, 'w>(env: &mut Env<'w>, e: &expr::Binary<'a>) -> Result {
    let op = match e.operator {
        expr::BinaryOperator::Multiply => "*",
        expr::BinaryOperator::Divide => "/",
        expr::BinaryOperator::Modulo => "%",
        expr::BinaryOperator::Plus => "+",
        expr::BinaryOperator::Minus => "-",
        expr::BinaryOperator::ShiftLeft => "<<",
        expr::BinaryOperator::ShiftRight => ">>",
        _ => unimplemented!(),
    };

    write!(env, "(")?;
    write_expr(env, &e.lhs)?;
    write!(env, ") {} (", op)?;
    write_expr(env, &e.rhs)?;
    write!(env, ")")
}

fn write_expr_struct_val<'a, 'w>(env: &mut Env<'w>, v: &expr::StructValue<'a>) -> Result {
    write_struct_tag(env, v.def)?;
    write_struct_fields(env, false, v.def, &mut |env, f| {
        write_expr(
            env,
            v.values.get(&f.id()).expect("missing value for a field"),
        )
    })
}

fn write_struct_tag<'a, 'w>(env: &mut Env<'w>, s: Ref<'a, Struct<'a>>) -> Result {
    write!(
        env,
        "{}",
        s.rust_name
            .borrow()
            .as_ref()
            .expect("struct without rust_name")
    )
}

pub fn write_struct<'a, 'w>(env: &mut Env<'w>, s: Ref<'a, Struct<'a>>) -> Result {
    if s.is_complete_ty() {
        write_struct_def(env, s)
    } else {
        write_struct_opaque(env, s)
    }
}

pub fn write_struct_opaque<'a, 'w>(env: &mut Env<'w>, s: Ref<'a, Struct<'a>>) -> Result {
    write!(env, "pub enum ")?;
    write_struct_tag(env, s)?;
    writeln!(env.output, "{{}}")
}

pub fn write_struct_def<'a, 'w>(env: &mut Env<'w>, s: Ref<'a, Struct<'a>>) -> Result {
    let kind = match s.kind {
        StructKind::Struct => "struct",
        StructKind::Union => "union",
    };

    writeln!(env.output, "#[repr(C)]")?;

    write!(env, "pub {} ", kind)?;
    write_struct_tag(env, s)?;

    write_struct_fields(env, true, s, &mut |env, field| {
        write_type_ref(env, &field.ty.ty)
    })?;

    writeln!(env.output)
}

fn write_struct_field<'a, 'w>(
    env: &mut Env<'w>,
    is_def: bool,
    field: Ref<'a, Field<'a>>,
    f: &mut FnMut(&mut Env<'w>, Ref<'a, Field<'a>>) -> Result,
) -> Result {
    if is_def {
        write!(env, "pub ")?;
    }

    write!(
        env,
        "{}: ",
        field
            .rust_name
            .borrow()
            .as_ref()
            .expect("field without rust_name")
    )?;

    f(env, field)?;
    writeln!(env.output, ",")
}

fn write_struct_fields<'a, 'w>(
    env: &mut Env<'w>,
    is_def: bool,
    s: Ref<'a, Struct<'a>>,
    f: &mut FnMut(&mut Env<'w>, Ref<'a, Field<'a>>) -> Result,
) -> Result {
    writeln!(env.output, " {{")?;

    if let Some(ref fields) = *s.fields.borrow() {
        for field in fields {
            write_struct_field(env, is_def, *field, f)?;

            if !is_def && s.kind == StructKind::Union {
                break;
            }
        }
    }

    write!(env, "}}")?;
    Ok(())
}

fn write_expr_cast<'a, 'w>(env: &mut Env<'w>, cast: &expr::Cast<'a>) -> Result {
    write_cast_expr(env, &cast.ty, |env| write_expr(env, &cast.expr))
}

pub fn write_type_ref<'a, 'w>(env: &mut Env<'w>, ty: &Type<'a>) -> Result {
    match *ty {
        Type::Void => write!(env, "c_void"),
        Type::Char => write!(env, "c_char"),
        Type::SChar => write!(env, "c_schar"),
        Type::UChar => write!(env, "c_uchar"),
        Type::SInt => write!(env, "c_int"),
        Type::UInt => write!(env, "c_uint"),
        Type::SShort => write!(env, "c_short"),
        Type::UShort => write!(env, "c_ushort"),
        Type::SLong => write!(env, "c_long"),
        Type::ULong => write!(env, "c_ulong"),
        Type::SLongLong => write!(env, "c_longlong"),
        Type::ULongLong => write!(env, "c_ulonglong"),
        Type::Float => write!(env, "c_float"),
        Type::Double => write!(env, "c_double"),
        Type::Struct(s) => write_struct_tag(env, s),
        Type::Pointer(ref ty) => {
            write!(env, "*mut ")?;
            write_type_ref(env, &ty.ty)
        }
        _ => unimplemented!(),
    }
}

fn write_function<'a, 'w>(env: &mut Env<'w>, f: Ref<'a, Function<'a>>) -> Result {
    write!(env, "extern \"C\" {{\n")?;
    write!(env, "pub fn {}", f.name)?;

    write_fn_type(env, &f.ty)?;

    write!(env, ";\n}}\n")
}

fn write_fn_type<'a, 'w>(env: &mut Env<'w>, f: &FunctionTy<'a>) -> Result {
    write!(env, "(")?;
    for p in &f.parameters {
        write_type_ref(env, &p.ty.ty)?;
        write!(env, ", ")?;
    }
    if f.variadic {
        write!(env, "...")?;
    }
    write!(env, ")")?;

    match f.return_type.ty {
        Type::Void => (),
        ref ty => {
            write!(env, " -> ")?;
            write_type_ref(env, ty)?;
        }
    }

    Ok(())
}

#[cfg(test)]
use {lang_c, syn};
#[cfg(test)]
use super::{interpret_translation_unit, Alloc};

#[cfg(test)]
fn translate(s: &str) -> String {
    let alloc = &Alloc::new();
    let mut buf = Vec::new();
    let parse = lang_c::driver::parse_preprocessed(&Default::default(), s.into()).unwrap();
    let ir = interpret_translation_unit(alloc, &parse.unit).unwrap();
    ir.run_passes().unwrap();
    write_translation_unit(&mut Env::new(&mut buf), &ir).unwrap();
    let s = String::from_utf8(buf).unwrap();
    syn::parse_str::<syn::File>(&s).unwrap();
    s
}

#[cfg(test)]
macro_rules! check {
    ($a:expr, $b:expr) => (assert_eq!(translate($a), $b));
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
    check!("extern char x;", "extern { pub static mut x: c_char; }\n");
}

#[test]
fn test_struct() {
    check!(
        "extern struct a { int b; } c;",
        "extern { pub static mut c: a; }\n\
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
    use std::os::raw::{c_int, c_long, c_uint};
    use std::mem::size_of;

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

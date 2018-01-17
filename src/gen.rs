use std::io;
use std::ops::RangeFrom;
use std::collections::hash_map::{Entry, HashMap};

use super::expr;
use super::{Declaration, Field, Function, QualType, Ref, Struct, StructKind, Type, Unit, Variable};

pub type Result = io::Result<()>;

#[derive(Default)]
pub struct Env<'a> {
    backlog: Vec<Item<'a>>,
    def_name: HashMap<usize, String>,
    def_name_next: usize,
}

impl<'a> Env<'a> {
    fn gen_name_for(&mut self, id: usize) -> &str {
        match self.def_name.entry(id) {
            Entry::Occupied(e) => e.into_mut(),
            Entry::Vacant(e) => {
                let name = format!("Generated_{}", self.def_name_next);
                self.def_name_next += 1;
                e.insert(name)
            }
        }
    }
}

pub struct Item<'a> {
    mode: ItemMode,
    kind: ItemKind<'a>,
}

#[derive(Clone, Copy)]
pub enum ItemMode {
    Opaque,
    Translate,
}

pub enum ItemKind<'a> {
    Variable(Ref<'a, Variable<'a>>),
    Function(Ref<'a, Function<'a>>),
    Struct(Ref<'a, Struct<'a>>),
}

pub fn write_translation_unit<'a>(
    env: &mut Env<'a>,
    dst: &mut io::Write,
    unit: &Unit<'a>,
) -> Result {
    for decl in &unit.declarations {
        match *decl {
            Declaration::Variable(var) => env.backlog.push(Item {
                mode: match var.defined.get() {
                    true => ItemMode::Translate,
                    false => ItemMode::Opaque,
                },
                kind: ItemKind::Variable(var),
            }),
            Declaration::FunctionDeclaration(_) => unimplemented!(),
        }
    }

    while let Some(item) = env.backlog.pop() {
        match item.kind {
            ItemKind::Struct(s) => write_struct(env, dst, item.mode, s)?,
            ItemKind::Variable(var) => write_variable(env, dst, item.mode, var)?,
            ItemKind::Function(_) => unimplemented!(),
        }
    }

    Ok(())
}

pub fn write_variable<'a>(
    env: &mut Env<'a>,
    dst: &mut io::Write,
    mode: ItemMode,
    var: Ref<'a, Variable<'a>>,
) -> Result {
    match mode {
        ItemMode::Translate => {
            writeln!(dst, "#[no_mangle]")?;
            write!(dst, "pub static mut {}: ", var.name)?;
            write_type_ref(env, dst, &var.ty.ty)?;
            write!(dst, " = ")?;
            match *var.initial.borrow() {
                Some(ref expr) => write_expr_as_ty(env, dst, expr, &var.ty.ty)?,
                None => write_zero_const(env, dst, &var.ty)?,
            }
            writeln!(dst, ";")?;
        }
        ItemMode::Opaque => {
            write!(dst, "extern {{ pub static mut {}: ", var.name)?;
            write_type_ref(env, dst, &var.ty.ty)?;
            writeln!(dst, "; }}")?;
        }
    }
    Ok(())
}

pub fn write_zero_const<'a>(env: &mut Env<'a>, dst: &mut io::Write, ty: &QualType<'a>) -> Result {
    match ty.ty {
        Type::Void => unimplemented!(),
        Type::Char
        | Type::SChar
        | Type::UChar
        | Type::SInt
        | Type::UInt
        | Type::SShort
        | Type::UShort
        | Type::SLong
        | Type::ULong
        | Type::SLongLong
        | Type::ULongLong
        | Type::Bool => write!(dst, "0"),
        Type::Float | Type::Double => write!(dst, "0.0"),
        Type::Struct(s) => {
            write_struct_tag(env, dst, s)?;
            write_struct_fields(env, dst, false, s, &mut |env, dst, field| {
                write_zero_const(env, dst, &field.ty)
            })
        }
        Type::Pointer(_) => write!(dst, "0 as *mut _"),
        _ => unimplemented!(),
    }
}

pub fn write_expr_as_ty<'a>(
    env: &mut Env<'a>,
    dst: &mut io::Write,
    expr: &expr::Expression<'a>,
    ty: &Type<'a>,
) -> Result {
    if expr.ty() != ty {
        write_cast_expr(env, dst, ty, |env, dst| write_expr(env, dst, expr))
    } else {
        write_expr(env, dst, expr)
    }
}

pub fn write_expr<'a>(
    env: &mut Env<'a>,
    dst: &mut io::Write,
    expr: &expr::Expression<'a>,
) -> Result {
    match *expr {
        expr::Expression::Constant(ref c) => write_const(env, dst, c),
    }
}

pub fn write_const<'a>(env: &mut Env<'a>, dst: &mut io::Write, c: &expr::Constant<'a>) -> Result {
    match *c {
        expr::Constant::Integer(ref i) => {
            write_cast_expr(env, dst, &i.ty, |_, dst| {
                match i.base {
                    expr::IntegerBase::Octal => write!(dst, "0o")?,
                    expr::IntegerBase::Hexademical => write!(dst, "0x")?,
                    expr::IntegerBase::Decimal => (),
                }
                write!(dst, "{}", i.number)
            })?;
        }

        expr::Constant::Float(ref f) => {
            write_cast_expr(env, dst, &f.ty, |_, dst| write!(dst, "{}", f.number))?;
        }
    }
    Ok(())
}

fn write_cast_expr<'a, F>(env: &mut Env<'a>, dst: &mut io::Write, ty: &Type<'a>, mut f: F) -> Result
where
    F: FnMut(&mut Env<'a>, &mut io::Write) -> Result,
{
    write!(dst, "(")?;
    f(env, dst)?;
    write!(dst, ") as ")?;
    write_type_ref(env, dst, ty)?;
    Ok(())
}

fn write_struct_tag<'a>(env: &mut Env<'a>, dst: &mut io::Write, s: Ref<'a, Struct<'a>>) -> Result {
    match s.tag {
        Some(ref tag) => write!(dst, "{}", tag),
        None => write!(dst, "{}", env.gen_name_for(s.id())),
    }
}

pub fn write_struct<'a>(
    env: &mut Env<'a>,
    dst: &mut io::Write,
    mode: ItemMode,
    s: Ref<'a, Struct<'a>>,
) -> Result {
    match mode {
        ItemMode::Opaque => write_struct_opaque(env, dst, s),
        ItemMode::Translate => write_struct_def(env, dst, s),
    }
}

pub fn write_struct_opaque<'a>(
    env: &mut Env<'a>,
    dst: &mut io::Write,
    s: Ref<'a, Struct<'a>>,
) -> Result {
    write!(dst, "pub enum ")?;
    write_struct_tag(env, dst, s)?;
    writeln!(dst, "{{}}")
}

pub fn write_struct_def<'a>(
    env: &mut Env<'a>,
    dst: &mut io::Write,
    s: Ref<'a, Struct<'a>>,
) -> Result {
    let kind = match s.kind {
        StructKind::Struct => "struct",
        StructKind::Union => "union",
    };

    writeln!(dst, "#[repr(C)]")?;

    write!(dst, "pub {} ", kind)?;
    write_struct_tag(env, dst, s)?;

    write_struct_fields(env, dst, true, s, &mut |env, dst, field| {
        write_type_ref(env, dst, &field.ty.ty)
    })?;

    writeln!(dst)
}

fn write_struct_field<'a>(
    seq: &mut RangeFrom<usize>,
    env: &mut Env<'a>,
    dst: &mut io::Write,
    is_def: bool,
    field: &Field<'a>,
    f: &mut FnMut(&mut Env<'a>, &mut io::Write, &Field<'a>) -> Result,
) -> Result {
    if is_def {
        write!(dst, "pub ")?;
    }

    match field.name {
        Some(ref name) => write!(dst, "{}", name),
        None => write!(dst, "anon_{}", seq.next().unwrap()),
    }?;

    write!(dst, ": ")?;
    f(env, dst, field)?;
    writeln!(dst, ",")
}

fn write_struct_fields<'a>(
    env: &mut Env<'a>,
    dst: &mut io::Write,
    is_def: bool,
    s: Ref<'a, Struct<'a>>,
    f: &mut FnMut(&mut Env<'a>, &mut io::Write, &Field<'a>) -> Result,
) -> Result {
    let seq = &mut (0..);

    writeln!(dst, " {{")?;

    if let Some(ref fields) = *s.fields.borrow() {
        for field in fields {
            write_struct_field(seq, env, dst, is_def, &*field, f)?;

            if !is_def && s.kind == StructKind::Union {
                break;
            }
        }
    }

    write!(dst, "}}")?;
    Ok(())
}

pub fn write_type_ref<'a>(env: &mut Env<'a>, dst: &mut io::Write, ty: &Type<'a>) -> Result {
    match *ty {
        Type::Void => write!(dst, "c_void"),
        Type::Char => write!(dst, "c_char"),
        Type::SChar => write!(dst, "c_schar"),
        Type::UChar => write!(dst, "c_uchar"),
        Type::SInt => write!(dst, "c_int"),
        Type::UInt => write!(dst, "c_uint"),
        Type::SShort => write!(dst, "c_short"),
        Type::UShort => write!(dst, "c_ushort"),
        Type::SLong => write!(dst, "c_long"),
        Type::ULong => write!(dst, "c_ulong"),
        Type::SLongLong => write!(dst, "c_longlong"),
        Type::ULongLong => write!(dst, "c_ulonglong"),
        Type::Float => write!(dst, "c_float"),
        Type::Double => write!(dst, "c_double"),
        Type::Struct(s) => {
            env.backlog.push(Item {
                mode: ItemMode::Translate,
                kind: ItemKind::Struct(s),
            });
            write_struct_tag(env, dst, s)
        }
        Type::Pointer(ref ty) => {
            write!(dst, "*mut ")?;
            write_type_ref(env, dst, &ty.ty)
        }
        _ => unimplemented!(),
    }
}

#[cfg(test)]
use lang_c;
#[cfg(test)]
use super::{interpret_translation_unit, Alloc};

#[cfg(test)]
fn translate(s: &str) -> String {
    let alloc = &Alloc::new();
    let mut buf = Vec::new();
    let parse = lang_c::driver::parse_preprocessed(&Default::default(), s.into()).unwrap();
    let ir = interpret_translation_unit(alloc, &mut super::Env::new(), &parse.unit).unwrap();
    write_translation_unit(&mut Default::default(), &mut buf, &ir).unwrap();
    String::from_utf8(buf).unwrap()
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
         pub static mut x: *mut c_int = 0 as *mut _;\n"
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
         anon_0: Generated_1 {\na: 0,\n},\n\
         anon_1: Generated_2 {\nc: 0,\nd: 0,\n},\n\
         };\n\
         #[repr(C)]\n\
         pub struct Generated_0 {\npub anon_0: Generated_1,\npub anon_1: Generated_2,\n}\n\
         #[repr(C)]\n\
         pub struct Generated_2 {\npub c: c_int,\npub d: c_int,\n}\n\
         #[repr(C)]\n\
         pub union Generated_1 {\npub a: c_int,\npub b: c_float,\n}\n\
         "
    );
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

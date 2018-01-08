use std::fmt::{self, Write};

use super::{Declaration, Field, Function, QualType, Ref, Struct, StructKind, Type, Variable};

#[derive(Default)]
pub struct Env<'a> {
    backlog: Vec<Item<'a>>,
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
    dst: &mut Write,
    decl_list: &[Declaration<'a>],
) -> fmt::Result {
    for decl in decl_list {
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
    dst: &mut Write,
    mode: ItemMode,
    var: Ref<'a, Variable<'a>>,
) -> fmt::Result {
    match mode {
        ItemMode::Translate => {
            writeln!(dst, "#[no_mangle]")?;
            write!(dst, "pub static mut {}: ", var.name)?;
            write_type_ref(env, dst, &var.ty)?;
            writeln!(dst, " = std::mem::zeroed();")?;
        }
        ItemMode::Opaque => {
            write!(dst, "extern {{ pub static mut {}: ", var.name)?;
            write_type_ref(env, dst, &var.ty)?;
            writeln!(dst, "; }}")?;
        }
    }
    Ok(())
}

fn write_struct_tag<'a>(
    _env: &mut Env<'a>,
    dst: &mut fmt::Write,
    s: Ref<'a, Struct<'a>>,
) -> fmt::Result {
    match s.tag {
        Some(ref tag) => write!(dst, "{}", tag),
        None => write!(dst, "Gen{:x}", s.id()),
    }
}

pub fn write_struct<'a>(
    env: &mut Env<'a>,
    dst: &mut fmt::Write,
    mode: ItemMode,
    s: Ref<'a, Struct<'a>>,
) -> fmt::Result {
    match mode {
        ItemMode::Opaque => write_struct_opaque(env, dst, s),
        ItemMode::Translate => write_struct_def(env, dst, s),
    }
}

pub fn write_struct_opaque<'a>(
    env: &mut Env<'a>,
    dst: &mut fmt::Write,
    s: Ref<'a, Struct<'a>>,
) -> fmt::Result {
    write!(dst, "pub enum ")?;
    write_struct_tag(env, dst, s)?;
    writeln!(dst, "{{}}")
}

pub fn write_struct_def<'a>(
    env: &mut Env<'a>,
    dst: &mut fmt::Write,
    s: Ref<'a, Struct<'a>>,
) -> fmt::Result {
    let kind = match s.kind {
        StructKind::Struct => "struct",
        StructKind::Union => "union",
    };

    writeln!(dst, "#[repr(C)]")?;

    write!(dst, "pub {} ", kind)?;
    write_struct_tag(env, dst, s)?;
    writeln!(dst, " {{")?;

    let mut seq_name = String::new();
    if let Some(ref fields) = *s.fields.borrow() {
        for (seq, field) in fields.iter().enumerate() {
            let name = match field.name {
                Some(ref name) => name,
                None => {
                    seq_name.clear();
                    write!(&mut seq_name, "anon_{}", seq)?;
                    &seq_name
                }
            };
            write_struct_field(env, dst, name, field)?;
        }
    }
    writeln!(dst, "}}")?;

    Ok(())
}

pub fn write_struct_field<'a>(
    env: &mut Env<'a>,
    dst: &mut fmt::Write,
    name: &str,
    f: &Field<'a>,
) -> fmt::Result {
    write!(dst, "    pub {}: ", name)?;
    write_type_ref(env, dst, &f.ty)?;
    writeln!(dst, ",")?;

    Ok(())
}

pub fn write_type_ref<'b, 'a: 'b>(
    env: &mut Env<'a>,
    dst: &mut fmt::Write,
    ty: &'b QualType<'a>,
) -> fmt::Result {
    match ty.ty {
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
        Type::Struct(s) => {
            env.backlog.push(Item {
                mode: ItemMode::Translate,
                kind: ItemKind::Struct(s),
            });
            write_struct_tag(env, dst, s)
        }
        Type::Pointer(ref ty) => {
            write!(dst, "*mut ")?;
            write_type_ref(env, dst, ty)
        }
        _ => unimplemented!(),
    }
}

#[cfg(test)]
use lang_c;
#[cfg(test)]
use super::{interpret_translation_unit, Alloc};

#[test]
fn test_static() {
    let alloc = &Alloc::new();
    let parse = lang_c::driver::parse_preprocessed(
        &Default::default(),
        "typedef int *ip; static ip x;".into(),
    ).unwrap();
    let buf = &mut String::new();
    let hir = interpret_translation_unit(alloc, &mut super::Env::new(), &parse.unit).unwrap();
    write_translation_unit(&mut Default::default(), buf, &hir).unwrap();
    assert_eq!(
        &buf[..],
        "#[no_mangle]\n\
         pub static mut x: *mut c_int = std::mem::zeroed();\n"
    );
}

#[test]
fn test_extern() {
    let alloc = &Alloc::new();
    let parse =
        lang_c::driver::parse_preprocessed(&Default::default(), "extern char x;".into()).unwrap();
    let buf = &mut String::new();
    let hir = interpret_translation_unit(alloc, &mut super::Env::new(), &parse.unit).unwrap();
    write_translation_unit(&mut Default::default(), buf, &hir).unwrap();
    assert_eq!(&buf[..], "extern { pub static mut x: c_char; }\n");
}

#[test]
fn test_struct() {
    let alloc = &Alloc::new();
    let parse = lang_c::driver::parse_preprocessed(
        &Default::default(),
        "extern struct a { int b; } c;".into(),
    ).unwrap();
    let buf = &mut String::new();
    let hir = interpret_translation_unit(alloc, &mut super::Env::new(), &parse.unit).unwrap();
    write_translation_unit(&mut Default::default(), buf, &hir).unwrap();
    assert_eq!(
        &buf[..],
        "\
         extern { pub static mut c: a; }\n\
         #[repr(C)]\n\
         pub struct a {\n    pub b: c_int,\n}\n\
         "
    );
}

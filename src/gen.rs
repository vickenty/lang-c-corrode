use std::fmt::{self, Write};

use super::{Field, QualType, Ref, Struct, StructKind, Type, Declaration, Variable, Linkage};

#[derive(Default)]
pub struct Env<'a> {
    backlog: Vec<Item<'a>>,
}

pub struct Item<'a> {
    mode: ItemMode,
    kind: ItemKind<'a>,
}

pub enum ItemMode {
    Opaque,
    Translate,
}

pub enum ItemKind<'a> {
    Type(QualType<'a>),
    Struct(Ref<'a, Struct<'a>>),
}

pub fn write_translation_unit<'a>(
    env: &mut Env<'a>,
    dst: &mut Write,
    decl_list: &[Declaration<'a>],
) -> fmt::Result {
    for decl in decl_list {
        match *decl {
            Declaration::Variable(ref var) => write_variable(env, dst, var)?,
            Declaration::FunctionDeclaration(_) => unimplemented!(),
        }
    }

    Ok(())
}

pub fn write_variable<'a>(
    env: &mut Env<'a>,
    dst: &mut Write,
    var: &Variable<'a>
) -> fmt::Result {
    match var.linkage {
        Linkage::Internal => {
            write!(dst, "pub static {}: ", var.name)?;
            write_type_ref(env, dst, &var.ty)?;
            writeln!(dst, " = std::mem::zeroed();")?;
        }
        Linkage::External => {
            write!(dst, "extern {{ pub static {}: ", var.name)?;
            write_type_ref(env, dst, &var.ty)?;
            writeln!(dst, "; }}")?;
        }
    }
    Ok(())
}

pub fn write_struct<'a>(
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
    match s.tag {
        Some(ref tag) => write!(dst, "{}", tag)?,
        None => write!(dst, "Gen{:x}", s.id())?,
    }

    writeln!(dst, " {{")?;

    let mut seq_name = String::new();
    if let Some(ref fields) = *s.fields.borrow() {
        for (seq, field) in fields.iter().enumerate() {
            let name = match field.name {
                Some(ref name) => name,
                None => {
                    seq_name.clear();
                    write!(&mut seq_name, "anon_{}", seq);
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
            match s.tag {
            Some(ref tag) => write!(dst, "{}", tag),
            None => write!(dst, "Gen{:x}", s.id()),
        }}
        Type::Pointer(ref ty) => {
            env.backlog.push(Item {
                mode: ItemMode::Opaque,
                kind: ItemKind::Type(Clone::clone(ty)),
            });
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
fn test_struct() {
    let alloc = &Alloc::new();
    let env = &mut super::Env::new();
    let res = lang_c::driver::parse_preprocessed(
        &Default::default(),
        "typedef int *ip; union x { ip x; };".into(),
    ).unwrap();
    let _ = interpret_translation_unit(alloc, env, &res.unit).unwrap();
    let sty = env.lookup_struct("x", false).unwrap().unwrap();
    let buf = &mut String::new();
    write_struct(&mut Env::default(), buf, sty).unwrap();
    assert_eq!(
        &buf[..],
        "#[repr(C)]\npub union x {\n    pub x: *mut c_int,\n}\n"
    );
}

#[test]
fn test_static() {
    let alloc = &Alloc::new();
    let parse = lang_c::driver::parse_preprocessed(&Default::default(), "typedef int *ip; static ip x;".into()).unwrap();
    let buf = &mut String::new();
    let hir = interpret_translation_unit(alloc, &mut super::Env::new(), &parse.unit).unwrap();
    write_translation_unit(&mut Default::default(), buf, &hir).unwrap();
    assert_eq!(&buf[..], "pub static x: *mut c_int = std::mem::zeroed();\n");
}

#[test]
fn test_extern() {
    let alloc = &Alloc::new();
    let parse = lang_c::driver::parse_preprocessed(&Default::default(), "extern char x;".into()).unwrap();
    let buf = &mut String::new();
    let hir = interpret_translation_unit(alloc, &mut super::Env::new(), &parse.unit).unwrap();
    write_translation_unit(&mut Default::default(), buf, &hir).unwrap();
    assert_eq!(&buf[..], "extern { pub static x: c_char; }\n");
}

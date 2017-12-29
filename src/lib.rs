extern crate dynamic_arena;
extern crate lang_c;

use std::cell::RefCell;
use std::collections::HashMap;

use lang_c::ast;
use lang_c::span::Node;

pub type Error = &'static str;

pub struct Alloc<'a>(dynamic_arena::DynamicArena<'a>);

impl<'a> Alloc<'a> {
    pub fn new() -> Alloc<'a> {
        Alloc(dynamic_arena::DynamicArena::new_bounded())
    }
    fn new_variable(&'a self, v: Variable<'a>) -> &'a Variable<'a> {
        self.0.alloc(v)
    }
    fn new_struct(&'a self, v: Struct<'a>) -> &'a Struct<'a> {
        self.0.alloc(v)
    }
}

pub struct Scope<'a> {
    names: HashMap<String, NameDef<'a>>,
    tags: HashMap<String, TagDef<'a>>,
}

impl<'a> Scope<'a> {
    fn new() -> Scope<'a> {
        Scope {
            names: HashMap::new(),
            tags: HashMap::new(),
        }
    }

    fn add_typedef(&mut self, name: &str, qty: QualType<'a>) {
        self.names.insert(name.into(), NameDef::Typedef(qty));
    }

    fn add_variable(&mut self, name: &str, var: &'a Variable<'a>) {
        self.names.insert(name.into(), NameDef::Variable(var));
    }

    fn lookup_name(&self, name: &str) -> Option<&NameDef<'a>> {
        self.names.get(name)
    }

    fn add_struct(&mut self, tag: &str, s: &'a Struct<'a>) {
        self.tags.insert(tag.into(), TagDef::Struct(s));
    }

    fn lookup_tag(&self, tag: &str) -> Option<&TagDef<'a>> {
        self.tags.get(tag)
    }
}

pub struct Env<'a> {
    scopes: Vec<Scope<'a>>,
}

impl<'a> Env<'a> {
    pub fn new() -> Env<'a> {
        Env {
            scopes: vec![Scope::new()],
        }
    }

    fn top(&mut self) -> &mut Scope<'a> {
        self.scopes.last_mut().expect("empty scope stack")
    }

    fn add_typedef(&mut self, name: &str, qty: QualType<'a>) {
        self.top().add_typedef(name, qty);
    }

    fn add_variable(&mut self, name: &str, var: &'a Variable<'a>) {
        self.top().add_variable(name, var);
    }

    fn add_struct(&mut self, tag: &str, s: &'a Struct<'a>) {
        self.top().add_struct(tag, s);
    }

    fn lookup_name(&self, name: &str) -> Option<&NameDef<'a>> {
        for scope in self.scopes.iter().rev() {
            if let Some(def) = scope.lookup_name(name) {
                return Some(def);
            }
        }
        None
    }

    fn lookup_typedef(&self, name: &str) -> Result<&QualType<'a>, Error> {
        match self.lookup_name(name) {
            Some(&NameDef::Typedef(ref qty)) => Ok(qty),
            Some(_) => Err("not a type"),
            None => Err("unknown type"),
        }
    }

    fn lookup_tag(&self, tag: &str, top_only: bool) -> Option<&TagDef<'a>> {
        for scope in self.scopes.iter().rev() {
            if let Some(def) = scope.lookup_tag(tag) {
                return Some(def);
            }
            if top_only {
                break;
            }
        }
        None
    }

    fn lookup_struct(&self, tag: &str, top_only: bool) -> Option<Result<&'a Struct<'a>, Error>> {
        match self.lookup_tag(tag, top_only) {
            Some(&TagDef::Struct(s)) => Some(Ok(s)),
            Some(_) => Some(Err("not a struct or a unino")),
            None => None,
        }
    }
}

enum NameDef<'a> {
    Variable(&'a Variable<'a>),
    Typedef(QualType<'a>),
}

enum TagDef<'a> {
    Struct(&'a Struct<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration<'a> {
    Variable(&'a Variable<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Variable<'a> {
    storage: Storage,
    name: String,
    ty: QualType<'a>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Storage {
    Auto,
    Register,
    Static,
    Extern,
}

#[derive(Debug, PartialEq, Clone)]
pub struct QualType<'a> {
    ty: Type<'a>,
    volatile: bool,
    constant: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type<'a> {
    Void,
    Char,
    SChar,
    UChar,
    SInt,
    UInt,
    SShort,
    UShort,
    SLong,
    ULong,
    SLongLong,
    ULongLong,
    Float,
    Double,
    Bool,
    Pointer(Box<QualType<'a>>),
    Struct(&'a Struct<'a>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Sign {
    None,
    Signed,
    Unsigned,
}

#[derive(Debug, PartialEq)]
enum TypeKind<'a> {
    Void,
    Char,
    Int,
    Float,
    Double,
    Bool,
    Typedef(String),
    Struct(&'a Struct<'a>),
}

struct TypeBuilder<'a> {
    kind: Option<TypeKind<'a>>,
    sign: Sign,
    short: usize,
    long: usize,
    volatile: bool,
    constant: bool,
}

impl<'a> TypeBuilder<'a> {
    fn new() -> TypeBuilder<'a> {
        TypeBuilder {
            kind: None,
            sign: Sign::None,
            short: 0,
            long: 0,
            volatile: false,
            constant: false,
        }
    }

    fn visit_qualifier(&mut self, qual: &ast::TypeQualifier) -> Result<(), Error> {
        match *qual {
            ast::TypeQualifier::Volatile => self.volatile = true,
            ast::TypeQualifier::Const => self.constant = true,
            ast::TypeQualifier::Atomic => return Err("atomic qualifier is not supported"),
            ast::TypeQualifier::Restrict => (),
        }

        Ok(())
    }

    fn build_qual_type(&self, alloc: &'a Alloc<'a>, ty: Type<'a>) -> Result<QualType<'a>, Error> {
        Ok(QualType {
            ty: ty,
            volatile: self.volatile,
            constant: self.constant,
        })
    }

    fn visit_specifier(
        &mut self,
        alloc: &'a Alloc<'a>,
        env: &mut Env<'a>,
        spec: &ast::TypeSpecifier,
    ) -> Result<(), Error> {
        let mut new_kind = None;
        let mut new_sign = Sign::None;

        match *spec {
            ast::TypeSpecifier::Void => new_kind = Some(TypeKind::Void),
            ast::TypeSpecifier::Char => new_kind = Some(TypeKind::Char),
            ast::TypeSpecifier::Int => new_kind = Some(TypeKind::Int),
            ast::TypeSpecifier::Float => new_kind = Some(TypeKind::Float),
            ast::TypeSpecifier::Double => new_kind = Some(TypeKind::Double),
            ast::TypeSpecifier::Bool => new_kind = Some(TypeKind::Bool),
            ast::TypeSpecifier::Short if self.long == 0 => self.short += 1,
            ast::TypeSpecifier::Short => return Err("invalid use of short"),
            ast::TypeSpecifier::Long if self.short == 0 => self.long += 1,
            ast::TypeSpecifier::Long => return Err("invalid use of long"),
            ast::TypeSpecifier::Signed => new_sign = Sign::Signed,
            ast::TypeSpecifier::Unsigned => new_sign = Sign::Unsigned,
            ast::TypeSpecifier::TypedefName(ref id) => {
                new_kind = Some(TypeKind::Typedef(id.node.name.clone()))
            }
            ast::TypeSpecifier::Struct(ref s) => {
                new_kind = Some(TypeKind::Struct(interpret_struct_type(alloc, env, s)?))
            }
            _ => unimplemented!(),
        }

        if new_kind != None {
            if self.kind == None {
                self.kind = new_kind;
            } else {
                return Err("two or more data types in a declaration");
            }
        }

        if new_sign != Sign::None {
            if self.sign == Sign::None {
                self.sign = new_sign;
            } else {
                return Err("two or more sign specifiers in a declaration");
            }
        }

        Ok(())
    }

    fn build(mut self, alloc: &'a Alloc<'a>, env: &Env<'a>) -> Result<QualType<'a>, Error> {
        let kind = self.kind.take().unwrap_or(TypeKind::Int);

        let ty = match (kind, self.short, self.long, self.sign) {
            (TypeKind::Void, 0, 0, Sign::None) => Type::Void,

            (TypeKind::Char, 0, 0, Sign::None) => Type::Char,
            (TypeKind::Char, 0, 0, Sign::Signed) => Type::SChar,
            (TypeKind::Char, 0, 0, Sign::Unsigned) => Type::UChar,

            (TypeKind::Int, 0, 0, Sign::None) => Type::SInt,
            (TypeKind::Int, 0, 0, Sign::Signed) => Type::SInt,
            (TypeKind::Int, 0, 0, Sign::Unsigned) => Type::UInt,

            (TypeKind::Int, 1, 0, Sign::None) => Type::SShort,
            (TypeKind::Int, 1, 0, Sign::Signed) => Type::SShort,
            (TypeKind::Int, 1, 0, Sign::Unsigned) => Type::UShort,

            (TypeKind::Int, 0, 1, Sign::None) => Type::SLong,
            (TypeKind::Int, 0, 1, Sign::Signed) => Type::SLong,
            (TypeKind::Int, 0, 1, Sign::Unsigned) => Type::ULong,

            (TypeKind::Int, 0, 2, Sign::None) => Type::SLongLong,
            (TypeKind::Int, 0, 2, Sign::Signed) => Type::SLongLong,
            (TypeKind::Int, 0, 2, Sign::Unsigned) => Type::ULongLong,

            (TypeKind::Float, 0, 0, Sign::None) => Type::Float,
            (TypeKind::Double, 0, 0, Sign::None) => Type::Double,

            (TypeKind::Bool, 0, 0, Sign::None) => Type::Bool,

            (TypeKind::Typedef(ref name), 0, 0, Sign::None) => {
                let mut qty = Clone::clone(env.lookup_typedef(name)?);
                qty.volatile |= self.volatile;
                qty.constant |= self.constant;
                return Ok(qty);
            }

            (_, _, _, Sign::Signed) => return Err("invalid use of signed"),
            (_, _, _, Sign::Unsigned) => return Err("invalid use of unsigned"),
            (_, s, 0, _) if s > 0 => return Err("invalid use of short"),
            (_, 0, l, _) if l > 0 => return Err("invalid use of long"),

            spec => panic!("unhandled type spec: {:?}", spec),
        };

        self.build_qual_type(alloc, ty)
    }
}

fn derive_type<'a>(
    alloc: &'a Alloc<'a>,
    mut qty: QualType<'a>,
    derived: &[Node<ast::DerivedDeclarator>],
) -> Result<QualType<'a>, Error> {
    for dd in derived {
        match dd.node {
            ast::DerivedDeclarator::Pointer(ref s) => qty = derive_pointer(alloc, qty, s)?,
            ast::DerivedDeclarator::Array(ref s) => unimplemented!(),
            ast::DerivedDeclarator::Function(ref s) => unimplemented!(),
            ast::DerivedDeclarator::KRFunction(_) => {
                return Err("K&R-style function definition not allowed here")
            }
        }
    }

    Ok(qty)
}

fn derive_pointer<'a>(
    alloc: &'a Alloc<'a>,
    qty: QualType<'a>,
    quals: &[Node<ast::PointerQualifier>],
) -> Result<QualType<'a>, Error> {
    let mut builder = TypeBuilder::new();
    for qual in quals {
        match qual.node {
            ast::PointerQualifier::TypeQualifier(ref s) => builder.visit_qualifier(&s.node)?,
            ast::PointerQualifier::Extension(_) => (),
        }
    }
    Ok(builder.build_qual_type(alloc, Type::Pointer(Box::new(qty)))?)
}

pub fn interpret_declaration<'a>(
    alloc: &'a Alloc<'a>,
    env: &mut Env<'a>,
    decl: &Node<ast::Declaration>,
    default_storage: Storage,
) -> Result<Vec<Declaration<'a>>, Error> {
    let mut builder = TypeBuilder::new();
    let mut storage = None;
    let mut inline = false;
    let mut noreturn = false;
    let mut is_typedef = false;

    for spec in &decl.node.specifiers {
        let mut new_storage = None;
        match spec.node {
            ast::DeclarationSpecifier::StorageClass(ref s) => match s.node {
                ast::StorageClassSpecifier::Auto => new_storage = Some(Storage::Auto),
                ast::StorageClassSpecifier::Extern => new_storage = Some(Storage::Extern),
                ast::StorageClassSpecifier::Static => new_storage = Some(Storage::Static),
                ast::StorageClassSpecifier::Register => new_storage = Some(Storage::Register),
                ast::StorageClassSpecifier::ThreadLocal => {
                    return Err("_Thread_local is not supported")
                }
                ast::StorageClassSpecifier::Typedef => is_typedef = true,
            },
            ast::DeclarationSpecifier::TypeSpecifier(ref s) => {
                builder.visit_specifier(alloc, env, &s.node)?
            }
            ast::DeclarationSpecifier::TypeQualifier(ref s) => builder.visit_qualifier(&s.node)?,
            ast::DeclarationSpecifier::Function(ref s) => match s.node {
                ast::FunctionSpecifier::Inline => inline = true,
                ast::FunctionSpecifier::Noreturn => noreturn = true,
            },
            ast::DeclarationSpecifier::Alignment(_) => {
                return Err("alignment specifiers not supported")
            }
            ast::DeclarationSpecifier::Extension(_) => (), // ignored for now
        }

        if new_storage.is_some() {
            if storage.is_none() {
                storage = new_storage;
            } else {
                return Err("multiple storage specifiers in a declaration");
            }
        }
    }

    let base_qty = builder.build(alloc, env)?;

    let mut res = vec![];

    for init_declarator in &decl.node.declarators {
        let declr = &init_declarator.node.declarator.node;
        let qty = derive_type(alloc, base_qty.clone(), &declr.derived)?;

        match declr.kind.node {
            ast::DeclaratorKind::Abstract => (),
            ast::DeclaratorKind::Identifier(ref id) => {
                if is_typedef {
                    env.add_typedef(&id.node.name, qty);
                } else {
                    let var = alloc.new_variable(Variable {
                        storage: storage.unwrap_or(default_storage),
                        name: id.node.name.clone(),
                        ty: qty,
                    });
                    env.add_variable(&id.node.name, var);
                    res.push(Declaration::Variable(var));
                }
            }
            ast::DeclaratorKind::Declarator(_) => unimplemented!(),
        }
    }

    Ok(res)
}

#[cfg(test)]
fn interpret_specifiers<'a, 'b, T>(alloc: &'a Alloc<'a>, specifiers: T) -> Result<Type<'a>, Error>
where
    T: IntoIterator<Item = &'b ast::TypeSpecifier>,
{
    let mut builder = TypeBuilder::new();
    let env = &mut Env::new();

    for spec in specifiers {
        builder.visit_specifier(alloc, env, spec)?;
    }
    let qty = builder.build(alloc, env)?;
    Ok(qty.ty.clone())
}

#[test]
fn test_specs() {
    use lang_c::ast::TypeSpecifier::*;
    let alloc = &Alloc::new();
    assert_eq!(interpret_specifiers(alloc, &[Void]), Ok(Type::Void));
    assert_eq!(interpret_specifiers(alloc, &[Int]), Ok(Type::SInt));
    assert_eq!(
        interpret_specifiers(alloc, &[Unsigned, Int]),
        Ok(Type::UInt)
    );
    assert_eq!(
        interpret_specifiers(alloc, &[Int, Unsigned]),
        Ok(Type::UInt)
    );
    assert_eq!(
        interpret_specifiers(alloc, &[Long, Long]),
        Ok(Type::SLongLong)
    );
    assert_eq!(
        interpret_specifiers(alloc, &[Long, Unsigned, Long, Int]),
        Ok(Type::ULongLong)
    );
    assert_eq!(interpret_specifiers(alloc, &[Char]), Ok(Type::Char));
    assert_eq!(
        interpret_specifiers(alloc, &[Signed, Char]),
        Ok(Type::SChar)
    );
    assert_eq!(
        interpret_specifiers(alloc, &[Char, Long]),
        Err("invalid use of long")
    );
    assert_eq!(
        interpret_specifiers(alloc, &[Char, Void]),
        Err("two or more data types in a declaration")
    );
    assert_eq!(
        interpret_specifiers(alloc, &[Short, Long]),
        Err("invalid use of long")
    );
    assert_eq!(
        interpret_specifiers(alloc, &[Signed, Void]),
        Err("invalid use of signed")
    );
}

#[cfg(test)]
fn interpret_decl_str<'a>(
    parse_env: &mut lang_c::env::Env,
    alloc: &'a Alloc<'a>,
    env: &mut Env<'a>,
    decl_str: &str,
) -> Result<Vec<Declaration<'a>>, Error> {
    let decl = lang_c::parser::declaration(decl_str, parse_env).expect("syntax error");
    interpret_declaration(alloc, env, &decl, Storage::Static)
}

#[test]
fn test_decl() {
    let parse_env = &mut lang_c::env::Env::with_gnu(true);
    let alloc = &Alloc::new();
    let env = &mut Env::new();
    assert_eq!(
        interpret_decl_str(parse_env, alloc, env, "extern int x, * const y;"),
        Ok(vec![
            Declaration::Variable(alloc.new_variable(Variable {
                storage: Storage::Extern,
                name: "x".into(),
                ty: QualType {
                    volatile: false,
                    constant: false,
                    ty: Type::SInt,
                },
            })),
            Declaration::Variable(alloc.new_variable(Variable {
                storage: Storage::Extern,
                name: "y".into(),
                ty: QualType {
                    volatile: false,
                    constant: true,
                    ty: Type::Pointer(Box::new(QualType {
                        volatile: false,
                        constant: false,
                        ty: Type::SInt,
                    })),
                },
            })),
        ])
    );
}

#[test]
fn test_typedef() {
    let parse_env = &mut lang_c::env::Env::with_gnu(true);
    let alloc = &Alloc::new();
    let env = &mut Env::new();
    assert_eq!(
        interpret_decl_str(parse_env, alloc, env, "volatile int typedef a, *b;"),
        Ok(vec![])
    );
    assert_eq!(
        interpret_decl_str(parse_env, alloc, env, "a c;"),
        Ok(vec![
            Declaration::Variable(alloc.new_variable(Variable {
                storage: Storage::Static,
                name: "c".into(),
                ty: QualType {
                    volatile: true,
                    constant: false,
                    ty: Type::SInt,
                },
            })),
        ])
    );
    assert_eq!(
        interpret_decl_str(parse_env, alloc, env, "const b d;"),
        Ok(vec![
            Declaration::Variable(alloc.new_variable(Variable {
                storage: Storage::Static,
                name: "d".into(),
                ty: QualType {
                    volatile: false,
                    constant: true,
                    ty: Type::Pointer(Box::new(QualType {
                        volatile: true,
                        constant: false,
                        ty: Type::SInt,
                    })),
                },
            })),
        ])
    );
}

#[derive(Debug, PartialEq, Clone)]
pub struct Struct<'a> {
    kind: StructKind,
    name: Option<String>,
    fields: RefCell<Option<Vec<&'a Field<'a>>>>,
}

impl<'a> Struct<'a> {
    fn new(kind: StructKind, name: Option<String>) -> Struct<'a> {
        Struct {
            kind: kind,
            name: name,
            fields: RefCell::new(None),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum StructKind {
    Struct,
    Union,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Field<'a> {
    name: Option<String>,
    ty: QualType<'a>,
    bits: Option<usize>,
}

fn interpret_struct_type<'a>(
    alloc: &'a Alloc<'a>,
    env: &mut Env<'a>,
    sty: &Node<ast::StructType>,
) -> Result<&'a Struct<'a>, Error> {
    let kind = match sty.node.kind.node {
        ast::StructKind::Struct => StructKind::Struct,
        ast::StructKind::Union => StructKind::Union,
    };
    let name = sty.node.identifier.as_ref().map(|i| &i.node.name[..]);
    let decls = &sty.node.declarations;

    // A struct without a definition may refer to something from outer scope. A struct with a
    // declration may complement a previous forward declaration in the current scope or define a
    // new one.
    let lookup = name.and_then(|tag| env.lookup_struct(tag, !decls.is_empty()));
    let s = match lookup {
        Some(Err(e)) => return Err(e),
        Some(Ok(s)) if s.kind != kind => return Err("wrong kind of tag"),
        Some(Ok(s)) => s,
        None => {
            let s = alloc.new_struct(Struct::new(kind, name.map(ToOwned::to_owned)));
            if let Some(tag) = name {
                env.add_struct(tag, s);
            }
            s
        }
    };

    if !decls.is_empty() {
        let mut fields = s.fields.borrow_mut();
        if fields.is_some() {
            return Err("duplicate struct definition");
        }
        let new_fields = decls
            .iter()
            .map(|fd| interpret_field_decl(alloc, env, fd))
            .collect::<Result<_, _>>()?;
        *fields = Some(new_fields);
    }

    Ok(s)
}

fn interpret_field_decl<'a>(
    alloc: &'a Alloc<'a>,
    env: &mut Env<'a>,
    field_def: &Node<ast::StructDeclaration>,
) -> Result<&'a Field<'a>, Error> {
    unimplemented!();
}

#[test]
fn test_struct() {
    let parse_env = &mut lang_c::env::Env::with_gnu(true);
    let alloc = &Alloc::new();
    let env = &mut Env::new();

    assert_eq!(
        interpret_decl_str(parse_env, alloc, env, "struct x { struct x *next; } *head;"),
        Ok(vec![])
    );
}

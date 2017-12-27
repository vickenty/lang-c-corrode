extern crate dynamic_arena;
extern crate lang_c;

use std::cell::Cell;
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
    fn new_qual_type(&'a self, v: QualType<'a>) -> &'a QualType<'a> {
        self.0.alloc(v)
    }
}

pub struct Scope<'a> {
    names: HashMap<String, NameDef<'a>>,
}

impl<'a> Scope<'a> {
    fn new() -> Scope<'a> {
        Scope {
            names: HashMap::new(),
        }
    }

    fn add_typedef(&mut self, name: &str, qty: &'a QualType<'a>) {
        self.names.insert(name.into(), NameDef::Typedef(qty));
    }

    fn add_variable(&mut self, name: &str, var: &'a Variable<'a>) {
        self.names.insert(name.into(), NameDef::Variable(var));
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

    fn add_typedef(&mut self, name: &str, qty: &'a QualType<'a>) {
        self.top().add_typedef(name, qty);
    }

    fn add_variable(&mut self, name: &str, var: &'a Variable<'a>) {
        self.top().add_variable(name, var);
    }
}

enum NameDef<'a> {
    Variable(&'a Variable<'a>),
    Typedef(&'a QualType<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Declaration<'a> {
    Variable(&'a Variable<'a>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Variable<'a> {
    storage: Storage,
    name: String,
    ty: &'a QualType<'a>,
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
    ty: Cell<Type<'a>>,
    volatile: bool,
    constant: bool,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Type<'a> {
    Incomplete,
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
    Pointer(&'a QualType<'a>),
}

#[derive(Debug, PartialEq)]
enum Sign {
    None,
    Signed,
    Unsigned,
}

#[derive(Debug, PartialEq)]
enum TypeKind {
    None,
    Void,
    Char,
    Int,
    Float,
    Double,
    Bool,
}

struct QualTypeBuilder {
    volatile: bool,
    constant: bool,
}

struct TypeBuilder {
    kind: TypeKind,
    sign: Sign,
    short: usize,
    long: usize,
}

impl QualTypeBuilder {
    fn new() -> QualTypeBuilder {
        QualTypeBuilder {
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

    fn build<'a>(self, alloc: &'a Alloc<'a>, ty: Type<'a>) -> Result<&'a QualType<'a>, Error> {
        Ok(alloc.new_qual_type(QualType {
            ty: Cell::new(ty),
            volatile: self.volatile,
            constant: self.constant,
        }))
    }
}

impl TypeBuilder {
    fn new() -> TypeBuilder {
        TypeBuilder {
            kind: TypeKind::None,
            sign: Sign::None,
            short: 0,
            long: 0,
        }
    }

    fn visit_specifier(&mut self, spec: &ast::TypeSpecifier) -> Result<(), Error> {
        let mut new_kind = TypeKind::None;
        let mut new_sign = Sign::None;

        match *spec {
            ast::TypeSpecifier::Void => new_kind = TypeKind::Void,
            ast::TypeSpecifier::Char => new_kind = TypeKind::Char,
            ast::TypeSpecifier::Int => new_kind = TypeKind::Int,
            ast::TypeSpecifier::Float => new_kind = TypeKind::Float,
            ast::TypeSpecifier::Double => new_kind = TypeKind::Double,
            ast::TypeSpecifier::Bool => new_kind = TypeKind::Bool,
            ast::TypeSpecifier::Short if self.long == 0 => self.short += 1,
            ast::TypeSpecifier::Short => return Err("invalid use of short"),
            ast::TypeSpecifier::Long if self.short == 0 => self.long += 1,
            ast::TypeSpecifier::Long => return Err("invalid use of long"),
            ast::TypeSpecifier::Signed => new_sign = Sign::Signed,
            ast::TypeSpecifier::Unsigned => new_sign = Sign::Unsigned,
            _ => unimplemented!(),
        }

        if new_kind != TypeKind::None {
            if self.kind == TypeKind::None {
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

    fn build<'a>(self) -> Result<Type<'a>, Error> {
        let kind = match self.kind {
            TypeKind::None => TypeKind::Int,
            kind => kind,
        };

        let ty = match (kind, self.short, self.long, self.sign) {
            (TypeKind::None, _, _, _) => unreachable!(),

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

            (_, _, _, Sign::Signed) => return Err("invalid use of signed"),
            (_, _, _, Sign::Unsigned) => return Err("invalid use of unsigned"),
            (_, s, 0, _) if s > 0 => return Err("invalid use of short"),
            (_, 0, l, _) if l > 0 => return Err("invalid use of long"),

            spec => panic!("unhandled type spec: {:?}", spec),
        };

        Ok(ty)
    }
}

fn derive_type<'a>(
    alloc: &'a Alloc<'a>,
    mut qty: &'a QualType<'a>,
    derived: &[Node<ast::DerivedDeclarator>],
) -> Result<&'a QualType<'a>, Error> {
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
    qty: &'a QualType<'a>,
    quals: &[Node<ast::PointerQualifier>],
) -> Result<&'a QualType<'a>, Error> {
    let mut builder = QualTypeBuilder::new();
    for qual in quals {
        match qual.node {
            ast::PointerQualifier::TypeQualifier(ref s) => builder.visit_qualifier(&s.node)?,
            ast::PointerQualifier::Extension(_) => (),
        }
    }
    Ok(builder.build(alloc, Type::Pointer(qty))?)
}

pub fn interpret_declaration<'a>(
    alloc: &'a Alloc<'a>,
    env: &mut Env<'a>,
    decl: &Node<ast::Declaration>,
    default_storage: Storage,
) -> Result<Vec<Declaration<'a>>, Error> {
    let mut type_builder = TypeBuilder::new();
    let mut qual_builder = QualTypeBuilder::new();
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
                type_builder.visit_specifier(&s.node)?
            }
            ast::DeclarationSpecifier::TypeQualifier(ref s) => {
                qual_builder.visit_qualifier(&s.node)?
            }
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

    let base_ty = type_builder.build()?;
    let base_qty = qual_builder.build(alloc, base_ty)?;

    let mut res = vec![];

    for init_declarator in &decl.node.declarators {
        let declr = &init_declarator.node.declarator.node;
        let qty = derive_type(alloc, base_qty, &declr.derived)?;

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
fn interpret_specifiers<'a, 'b, T>(specifiers: T) -> Result<Type<'a>, Error>
where
    T: IntoIterator<Item = &'b ast::TypeSpecifier>,
{
    let mut builder = TypeBuilder::new();

    for spec in specifiers {
        builder.visit_specifier(spec)?;
    }
    let ty = builder.build()?;
    Ok(ty)
}

#[test]
fn test_specs() {
    use lang_c::ast::TypeSpecifier::*;
    assert_eq!(interpret_specifiers(&[Void]), Ok(Type::Void));
    assert_eq!(interpret_specifiers(&[Int]), Ok(Type::SInt));
    assert_eq!(interpret_specifiers(&[Unsigned, Int]), Ok(Type::UInt));
    assert_eq!(interpret_specifiers(&[Int, Unsigned]), Ok(Type::UInt));
    assert_eq!(interpret_specifiers(&[Long, Long]), Ok(Type::SLongLong));
    assert_eq!(
        interpret_specifiers(&[Long, Unsigned, Long, Int]),
        Ok(Type::ULongLong)
    );
    assert_eq!(interpret_specifiers(&[Char]), Ok(Type::Char));
    assert_eq!(interpret_specifiers(&[Signed, Char]), Ok(Type::SChar));
    assert_eq!(
        interpret_specifiers(&[Char, Long]),
        Err("invalid use of long")
    );
    assert_eq!(
        interpret_specifiers(&[Char, Void]),
        Err("two or more data types in a declaration")
    );
    assert_eq!(
        interpret_specifiers(&[Short, Long]),
        Err("invalid use of long")
    );
    assert_eq!(
        interpret_specifiers(&[Signed, Void]),
        Err("invalid use of signed")
    );
}

#[cfg(test)]
fn interpret_decl_str<'a>(
    alloc: &'a Alloc<'a>,
    decl_str: &str,
) -> Result<Vec<Declaration<'a>>, Error> {
    use lang_c::{env, parser};
    let decl = parser::declaration(decl_str, &mut env::Env::with_gnu(true)).expect("syntax error");
    interpret_declaration(alloc, &mut Env::new(), &decl, Storage::Static)
}

#[test]
fn test_decl() {
    use lang_c::parser::declaration;
    let alloc = &Alloc::new();
    assert_eq!(
        interpret_decl_str(alloc, "extern int x, * const y;"),
        Ok(vec![
            Declaration::Variable(alloc.new_variable(Variable {
                storage: Storage::Extern,
                name: "x".into(),
                ty: alloc.new_qual_type(QualType {
                    volatile: false,
                    constant: false,
                    ty: Cell::new(Type::SInt),
                }),
            })),
            Declaration::Variable(alloc.new_variable(Variable {
                storage: Storage::Extern,
                name: "y".into(),
                ty: alloc.new_qual_type(QualType {
                    volatile: false,
                    constant: true,
                    ty: Cell::new(Type::Pointer(alloc.new_qual_type(QualType {
                        volatile: false,
                        constant: false,
                        ty: Cell::new(Type::SInt),
                    }))),
                }),
            })),
        ])
    );
}

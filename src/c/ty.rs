use std::cell::RefCell;
use std::collections::HashSet;

use c::Env;
use {ast, Alloc, Error, Node, Ref};

#[derive(Debug, PartialEq, Clone)]
pub struct QualType<'a> {
    pub ty: Type<'a>,
    pub volatile: bool,
    pub constant: bool,
}

impl<'a> QualType<'a> {
    pub fn merge(&self, _other: &QualType<'a>) {
        // nothing to do yet
    }
}

#[derive(PartialEq, Clone, Debug)]
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
    Struct(Ref<'a, Struct<'a>>),
    Function(Box<FunctionTy<'a>>),
}

impl<'a> Type<'a> {
    pub fn eq(a: &Self, b: &Self) -> bool {
        match (a, b) {
            (&Type::Void, &Type::Void) => true,
            (&Type::Char, &Type::Char) => true,
            (&Type::SChar, &Type::SChar) => true,
            (&Type::UChar, &Type::UChar) => true,
            (&Type::SInt, &Type::SInt) => true,
            (&Type::UInt, &Type::UInt) => true,
            (&Type::SShort, &Type::SShort) => true,
            (&Type::UShort, &Type::UShort) => true,
            (&Type::SLong, &Type::SLong) => true,
            (&Type::ULong, &Type::ULong) => true,
            (&Type::SLongLong, &Type::SLongLong) => true,
            (&Type::ULongLong, &Type::ULongLong) => true,
            (&Type::Float, &Type::Float) => true,
            (&Type::Double, &Type::Double) => true,
            (&Type::Bool, &Type::Bool) => true,
            (&Type::Pointer(ref a), &Type::Pointer(ref b)) => a == b,
            (&Type::Struct(ref a), &Type::Struct(ref b)) => Ref::eq(a, b),
            (&Type::Function(ref a), &Type::Function(ref b)) => a == b,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionTy<'a> {
    pub return_type: QualType<'a>,
    pub parameters: Vec<Parameter<'a>>,
    pub variadic: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Parameter<'a> {
    pub name: Option<String>,
    pub ty: QualType<'a>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Struct<'a> {
    pub kind: StructKind,
    pub tag: Option<String>,
    pub fields: RefCell<Option<Vec<Ref<'a, Field<'a>>>>>,
    pub rust_name: RefCell<Option<String>>,
}

impl<'a> Struct<'a> {
    pub fn new(kind: StructKind, tag: Option<String>) -> Struct<'a> {
        Struct {
            kind: kind,
            tag: tag,
            fields: RefCell::new(None),
            rust_name: RefCell::new(None),
        }
    }

    pub fn is_complete_ty(&self) -> bool {
        self.fields.borrow().is_some()
    }

    pub fn postprocess(&self, env: &mut Env) {
        self.postprocess_name(env);
        self.postprocess_fields();
    }

    fn postprocess_name(&self, env: &mut Env) {
        if let Some(mut name) = self.tag.clone() {
            env.mangle_name(&mut name);
            *self.rust_name.borrow_mut() = Some(name);
        } else {
            *self.rust_name.borrow_mut() = Some(env.generate_name());
        }
    }

    fn postprocess_fields(&self) {
        let fields = self.fields.borrow();
        let fields = match *fields {
            Some(ref f) => f,
            None => return,
        };

        let field_names = fields
            .iter()
            .filter_map(|f| f.name.as_ref())
            .collect::<HashSet<_>>();

        let mut seq = 0..;

        for f in fields {
            if f.name.is_some() {
                *f.rust_name.borrow_mut() = f.name.clone();
            } else {
                loop {
                    let name = format!("anon_{}", seq.next().unwrap());
                    if !field_names.contains(&name) {
                        *f.rust_name.borrow_mut() = Some(name);
                        break;
                    }
                }
            }
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
    pub name: Option<String>,
    pub rust_name: RefCell<Option<String>>,
    pub ty: QualType<'a>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Sign {
    None,
    Signed,
    Unsigned,
}

#[derive(Debug)]
enum TypeKind<'a> {
    Void,
    Char,
    Int,
    Float,
    Double,
    Bool,
    Typedef(String),
    Struct(Ref<'a, Struct<'a>>),
}

pub struct TypeBuilder<'a> {
    kind: Option<TypeKind<'a>>,
    sign: Sign,
    short: usize,
    long: usize,
    volatile: bool,
    constant: bool,
}

impl<'a> TypeBuilder<'a> {
    pub fn new() -> TypeBuilder<'a> {
        TypeBuilder {
            kind: None,
            sign: Sign::None,
            short: 0,
            long: 0,
            volatile: false,
            constant: false,
        }
    }

    pub fn visit_qualifier(&mut self, qual: &ast::TypeQualifier) -> Result<(), Error> {
        match *qual {
            ast::TypeQualifier::Volatile => self.volatile = true,
            ast::TypeQualifier::Const => self.constant = true,
            ast::TypeQualifier::Atomic => return Err("atomic qualifier is not supported"),
            ast::TypeQualifier::Restrict => (),
        }

        Ok(())
    }

    fn build_qual_type(&self, ty: Type<'a>) -> Result<QualType<'a>, Error> {
        Ok(QualType {
            ty: ty,
            volatile: self.volatile,
            constant: self.constant,
        })
    }

    pub fn visit_specifier(
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

        if new_kind.is_some() {
            if self.kind.is_none() {
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

    pub fn build(mut self, env: &Env<'a>) -> Result<QualType<'a>, Error> {
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
                let mut qty = env.lookup_typedef(name)?.clone();
                qty.volatile |= self.volatile;
                qty.constant |= self.constant;
                return Ok(qty);
            }

            (TypeKind::Struct(sty), 0, 0, Sign::None) => Type::Struct(sty),

            (_, _, _, Sign::Signed) => return Err("invalid use of signed"),
            (_, _, _, Sign::Unsigned) => return Err("invalid use of unsigned"),
            (_, s, 0, _) if s > 0 => return Err("invalid use of short"),
            (_, 0, l, _) if l > 0 => return Err("invalid use of long"),

            spec => panic!("unhandled type spec: {:?}", spec),
        };

        self.build_qual_type(ty)
    }
}

fn derive_type<'a>(
    alloc: &'a Alloc<'a>,
    env: &mut Env<'a>,
    mut qty: QualType<'a>,
    derived: &[Node<ast::DerivedDeclarator>],
) -> Result<QualType<'a>, Error> {
    for dd in derived {
        match dd.node {
            ast::DerivedDeclarator::Pointer(ref s) => qty = derive_pointer(qty, s)?,
            ast::DerivedDeclarator::Array(_) => unimplemented!(),
            ast::DerivedDeclarator::Function(ref fd) => {
                qty = derive_func_type(alloc, env, qty, fd)?;
            }
            ast::DerivedDeclarator::KRFunction(ref fs) => {
                qty = derive_func_type_kr(qty, fs)?;
            }
        }
    }

    Ok(qty)
}

fn derive_pointer<'a>(
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
    Ok(builder.build_qual_type(Type::Pointer(Box::new(qty)))?)
}

fn interpret_struct_type<'a>(
    alloc: &'a Alloc<'a>,
    env: &mut Env<'a>,
    sty: &Node<ast::StructType>,
) -> Result<Ref<'a, Struct<'a>>, Error> {
    let kind = match sty.node.kind.node {
        ast::StructKind::Struct => StructKind::Struct,
        ast::StructKind::Union => StructKind::Union,
    };
    let tag = sty.node.identifier.as_ref().map(|i| &i.node.name[..]);
    let decls = &sty.node.declarations;

    // A struct without a definition may refer to something from outer scope. A struct with a
    // declration may complement a previous forward declaration in the current scope or define a
    // new one.
    let lookup = tag.and_then(|tag| env.lookup_struct(tag, !decls.is_empty()));
    let s = match lookup {
        Some(Err(e)) => return Err(e),
        Some(Ok(s)) if s.kind != kind => return Err("wrong kind of tag"),
        Some(Ok(s)) => s,
        None => {
            let s = alloc.new_struct(Struct::new(kind, tag.map(ToOwned::to_owned)));
            env.add_struct(s);
            s
        }
    };

    if !decls.is_empty() {
        let mut fields = s.fields.borrow_mut();
        if fields.is_some() {
            return Err("duplicate struct definition");
        }

        let mut new_fields = Vec::new();

        for decl in decls {
            match decl.node {
                ast::StructDeclaration::Field(ref f) => {
                    interpret_field_decl(alloc, env, f, &mut new_fields)?
                }
                ast::StructDeclaration::StaticAssert(_) => (),
            }
        }

        *fields = Some(new_fields);
    }

    Ok(s)
}

fn interpret_field_decl<'a>(
    alloc: &'a Alloc<'a>,
    env: &mut Env<'a>,
    field_def: &Node<ast::StructField>,
    output: &mut Vec<Ref<'a, Field<'a>>>,
) -> Result<(), Error> {
    let mut builder = TypeBuilder::new();

    for s in &field_def.node.specifiers {
        match s.node {
            ast::SpecifierQualifier::TypeQualifier(ref q) => builder.visit_qualifier(&q.node)?,
            ast::SpecifierQualifier::TypeSpecifier(ref s) => {
                builder.visit_specifier(alloc, env, &s.node)?
            }
        }
    }

    let base_qty = builder.build(env)?;

    if field_def.node.declarators.is_empty() {
        if let Type::Struct(Ref(&Struct { tag: None, .. })) = base_qty.ty {
            output.push(alloc.new_field(Field {
                name: None,
                ty: base_qty,
                rust_name: None.into(),
            }));
        }
        return Ok(());
    }

    for sdeclr in &field_def.node.declarators {
        if sdeclr.node.bit_width.is_some() {
            unimplemented!();
        }
        if let Some(ref declr) = sdeclr.node.declarator {
            let (qty, name) = derive_declarator(alloc, env, base_qty.clone(), declr)?;
            output.push(alloc.new_field(Field {
                name: name,
                ty: qty,
                rust_name: None.into(),
            }));
        }
    }

    Ok(())
}

fn derive_func_type<'a>(
    alloc: &'a Alloc<'a>,
    env: &mut Env<'a>,
    ret_ty: QualType<'a>,
    fd: &Node<ast::FunctionDeclarator>,
) -> Result<QualType<'a>, Error> {
    let params = fd
        .node
        .parameters
        .iter()
        .map(|pd| interpret_parameter(alloc, env, pd))
        .collect::<Result<_, _>>()?;

    Ok(QualType {
        constant: false,
        volatile: false,
        ty: Type::Function(Box::new(FunctionTy {
            return_type: ret_ty,
            parameters: params,
            variadic: fd.node.ellipsis == ast::Ellipsis::Some,
        })),
    })
}

fn derive_func_type_kr<'a>(
    ret_ty: QualType<'a>,
    fs: &Vec<Node<ast::Identifier>>,
) -> Result<QualType<'a>, Error> {
    let default_ty = QualType {
        volatile: false,
        constant: false,
        ty: Type::SInt,
    };

    let params = fs
        .iter()
        .map(|id| Parameter {
            name: Some(id.node.name.clone()),
            ty: default_ty.clone(),
        })
        .collect();

    Ok(QualType {
        volatile: false,
        constant: false,
        ty: Type::Function(Box::new(FunctionTy {
            return_type: ret_ty,
            parameters: params,
            variadic: false,
        })),
    })
}

pub fn derive_declarator<'a>(
    alloc: &'a Alloc<'a>,
    env: &mut Env<'a>,
    base_qty: QualType<'a>,
    declr: &Node<ast::Declarator>,
) -> Result<(QualType<'a>, Option<String>), Error> {
    let qty = derive_type(alloc, env, base_qty.clone(), &declr.node.derived)?;

    match declr.node.kind.node {
        ast::DeclaratorKind::Abstract => Ok((qty, None)),
        ast::DeclaratorKind::Identifier(ref id) => Ok((qty, Some(id.node.name.clone()))),
        ast::DeclaratorKind::Declarator(ref declr) => derive_declarator(alloc, env, qty, declr),
    }
}

fn interpret_parameter<'a>(
    alloc: &'a Alloc<'a>,
    env: &mut Env<'a>,
    pd: &Node<ast::ParameterDeclaration>,
) -> Result<Parameter<'a>, Error> {
    let mut builder = TypeBuilder::new();

    for spec in &pd.node.specifiers {
        match spec.node {
            ast::DeclarationSpecifier::StorageClass(ref sc) => {
                if sc.node != ast::StorageClassSpecifier::Register {
                    return Err("storage class spcifier not allowed in a parameter declaration");
                }
            }
            ast::DeclarationSpecifier::Function(_) => {
                return Err("function specifier not allowed in parameter declaration")
            }
            ast::DeclarationSpecifier::Alignment(_) => {
                return Err("alignment specifier not allowed in parameter declaration")
            }
            ast::DeclarationSpecifier::TypeQualifier(ref q) => builder.visit_qualifier(&q.node)?,
            ast::DeclarationSpecifier::TypeSpecifier(ref s) => {
                builder.visit_specifier(alloc, env, &s.node)?
            }
            ast::DeclarationSpecifier::Extension(_) => (),
        }
    }

    let base_qty = builder.build(env)?;

    Ok(match pd.node.declarator {
        Some(ref d) => {
            let (qty, name) = derive_declarator(alloc, env, base_qty, d)?;
            Parameter {
                name: name,
                ty: qty,
            }
        }
        None => Parameter {
            name: None,
            ty: base_qty,
        },
    })
}

#[cfg(test)]
impl<'a> From<Type<'a>> for Box<QualType<'a>> {
    fn from(ty: Type<'a>) -> Box<QualType<'a>> {
        Box::new(ty.into())
    }
}
#[cfg(test)]
impl<'a> From<Type<'a>> for QualType<'a> {
    fn from(ty: Type<'a>) -> QualType<'a> {
        QualType {
            ty: ty,
            volatile: false,
            constant: false,
        }
    }
}
#[cfg(test)]
impl<'a> From<FunctionTy<'a>> for Type<'a> {
    fn from(ft: FunctionTy<'a>) -> Type<'a> {
        Type::Function(Box::new(ft))
    }
}
#[cfg(test)]
impl<'a> From<FunctionTy<'a>> for QualType<'a> {
    fn from(ft: FunctionTy<'a>) -> QualType<'a> {
        QualType::from(Type::from(ft))
    }
}
#[cfg(test)]
impl<'a> From<FunctionTy<'a>> for Box<QualType<'a>> {
    fn from(ft: FunctionTy<'a>) -> Box<QualType<'a>> {
        Box::new(ft.into())
    }
}

#[test]
fn test_type_builder() {
    fn build<'a, 'b, T>(alloc: &'a Alloc<'a>, specifiers: T) -> Result<Type<'a>, Error>
    where
        T: IntoIterator<Item = &'b ast::TypeSpecifier>,
    {
        let mut builder = TypeBuilder::new();
        let env = &mut Env::new();

        for spec in specifiers {
            builder.visit_specifier(alloc, env, spec)?;
        }
        let qty = builder.build(env)?;
        Ok(qty.ty.clone())
    }

    use lang_c::ast::TypeSpecifier::*;
    let alloc = &Alloc::new();
    assert_eq!(build(alloc, &[Void]), Ok(Type::Void));
    assert_eq!(build(alloc, &[Int]), Ok(Type::SInt));
    assert_eq!(build(alloc, &[Unsigned, Int]), Ok(Type::UInt));
    assert_eq!(build(alloc, &[Int, Unsigned]), Ok(Type::UInt));
    assert_eq!(build(alloc, &[Long, Long]), Ok(Type::SLongLong));
    assert_eq!(
        build(alloc, &[Long, Unsigned, Long, Int]),
        Ok(Type::ULongLong)
    );
    assert_eq!(build(alloc, &[Char]), Ok(Type::Char));
    assert_eq!(build(alloc, &[Signed, Char]), Ok(Type::SChar));
    assert_eq!(build(alloc, &[Char, Long]), Err("invalid use of long"));
    assert_eq!(
        build(alloc, &[Char, Void]),
        Err("two or more data types in a declaration")
    );
    assert_eq!(build(alloc, &[Short, Long]), Err("invalid use of long"));
    assert_eq!(build(alloc, &[Signed, Void]), Err("invalid use of signed"));
}

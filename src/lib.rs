extern crate dynamic_arena;
extern crate lang_c;
#[cfg(test)]
extern crate syn;

use std::mem;
use std::ops::RangeFrom;
use std::fmt;
use std::cell::{Cell, RefCell};
use std::collections::{HashMap, HashSet};

use lang_c::ast;
use lang_c::span::Node;

pub mod gen;
pub mod expr;

pub type Error = &'static str;

pub struct Alloc<'a>(dynamic_arena::DynamicArena<'a>);

impl<'a> Alloc<'a> {
    pub fn new() -> Alloc<'a> {
        Alloc(dynamic_arena::DynamicArena::new_bounded())
    }
    fn new_variable(&'a self, v: Variable<'a>) -> Ref<'a, Variable<'a>> {
        Ref(self.0.alloc(v))
    }
    fn new_struct(&'a self, v: Struct<'a>) -> Ref<'a, Struct<'a>> {
        Ref(self.0.alloc(v))
    }
    fn new_field(&'a self, v: Field<'a>) -> Ref<'a, Field<'a>> {
        Ref(self.0.alloc(v))
    }
    fn new_function(&'a self, v: Function<'a>) -> Ref<'a, Function<'a>> {
        Ref(self.0.alloc(v))
    }
}

#[derive(PartialEq)]
pub struct Ref<'a, T: 'a>(&'a T);

pub type RefId = usize;

impl<'a, T: PartialEq> Ref<'a, T> {
    pub fn same_as(&self, other: &Self) -> bool {
        self.0 as *const _ == other.0 as *const _
    }

    pub fn id(&self) -> RefId {
        self.0 as *const _ as usize
    }
}

impl<'a, T: fmt::Debug> fmt::Debug for Ref<'a, T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        thread_local! {
            static SEEN: RefCell<HashMap<usize, (usize, bool)>> = RefCell::new(HashMap::new());
        }

        let addr = self.0 as *const _ as usize;
        SEEN.with(|seen_map| {
            let seen = seen_map.borrow().get(&addr).cloned();
            match seen {
                Some((id, _)) => {
                    seen_map.borrow_mut().get_mut(&addr).unwrap().1 = true;
                    write!(fmt, "\\{}", id)
                }
                None => {
                    let id = seen_map.borrow().len();
                    seen_map.borrow_mut().insert(addr, (id, false));
                    let repr = match fmt.alternate() {
                        true => format!("{:#?}", self.0),
                        false => format!("{:?}", self.0),
                    };
                    let used = seen_map
                        .borrow()
                        .get(&addr)
                        .map(|&(_, used)| used)
                        .unwrap_or(false);
                    if used {
                        write!(fmt, "<{}> {}", id, repr)?;
                    } else {
                        write!(fmt, "{}", repr)?;
                    }
                    seen_map.borrow_mut().remove(&addr);
                    Ok(())
                }
            }
        })
    }
}

impl<'a, T> Clone for Ref<'a, T> {
    fn clone(&self) -> Ref<'a, T> {
        Ref(self.0)
    }
}

impl<'a, T> Copy for Ref<'a, T> {}

impl<'a, T> std::ops::Deref for Ref<'a, T> {
    type Target = T;
    fn deref(&self) -> &T {
        &*self.0
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

    fn add_variable(&mut self, name: &str, var: Ref<'a, Variable<'a>>) {
        self.names.insert(name.into(), NameDef::Variable(var));
    }

    fn add_function(&mut self, name: &str, fun: Ref<'a, Function<'a>>) {
        self.names.insert(name.into(), NameDef::Function(fun));
    }

    fn lookup_name(&self, name: &str) -> Option<&NameDef<'a>> {
        self.names.get(name)
    }

    fn add_struct(&mut self, tag: &str, s: Ref<'a, Struct<'a>>) {
        self.tags.insert(tag.into(), TagDef::Struct(s));
    }

    fn lookup_tag(&self, tag: &str) -> Option<&TagDef<'a>> {
        self.tags.get(tag)
    }
}

pub struct Env<'a> {
    scopes: Vec<Scope<'a>>,
    struct_defs: Vec<Ref<'a, Struct<'a>>>,
    name_seq: RangeFrom<usize>,
}

impl<'a> Env<'a> {
    pub fn new() -> Env<'a> {
        Env {
            scopes: vec![Scope::new()],
            struct_defs: Vec::new(),
            name_seq: 0..,
        }
    }

    fn top(&mut self) -> &mut Scope<'a> {
        self.scopes.last_mut().expect("empty scope stack")
    }

    fn add_typedef(&mut self, name: &str, qty: QualType<'a>) {
        self.top().add_typedef(name, qty);
    }

    fn add_variable(&mut self, name: &str, var: Ref<'a, Variable<'a>>) {
        self.top().add_variable(name, var);
    }

    fn add_function(&mut self, name: &str, fun: Ref<'a, Function<'a>>) {
        self.top().add_function(name, fun);
    }

    fn add_struct(&mut self, s: Ref<'a, Struct<'a>>) {
        self.struct_defs.push(s);

        if let Some(ref tag) = s.tag {
            self.top().add_struct(tag, s);
        }
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

    fn lookup_variable(&self, name: &str) -> Result<Option<Ref<'a, Variable<'a>>>, Error> {
        match self.lookup_name(name) {
            Some(&NameDef::Variable(var)) => Ok(Some(var)),
            Some(_) => Err("not a varaible"),
            None => Ok(None),
        }
    }

    fn lookup_function(&self, name: &str) -> Result<Option<Ref<'a, Function<'a>>>, Error> {
        match self.lookup_name(name) {
            Some(&NameDef::Function(fun)) => Ok(Some(fun)),
            Some(_) => Err("not a function"),
            None => Ok(None),
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

    fn lookup_struct(
        &self,
        tag: &str,
        top_only: bool,
    ) -> Option<Result<Ref<'a, Struct<'a>>, Error>> {
        match self.lookup_tag(tag, top_only) {
            Some(&TagDef::Struct(s)) => Some(Ok(s)),
            Some(_) => Some(Err("not a struct or a union")),
            None => None,
        }
    }

    fn mangle_name(&self, name: &mut String) {
        while self.lookup_name(&name).is_some() {
            name.push('_');
        }
    }

    fn generate_name(&mut self) -> String {
        loop {
            let name = format!("Generated_{}", self.name_seq.next().unwrap());
            if self.lookup_name(&name).is_none() {
                return name;
            }
        }
    }

    fn finalize(mut self, items: &mut Vec<Item<'a>>) {
        let defs = mem::replace(&mut self.struct_defs, Vec::new());

        for s in defs {
            s.postprocess(&mut self);
            items.push(Item::Struct(s));
        }
    }
}

enum NameDef<'a> {
    Variable(Ref<'a, Variable<'a>>),
    Typedef(QualType<'a>),
    Function(Ref<'a, Function<'a>>),
}

enum TagDef<'a> {
    Struct(Ref<'a, Struct<'a>>),
}

#[derive(Debug)]
pub struct Unit<'a> {
    pub items: Vec<Item<'a>>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Item<'a> {
    Variable(Ref<'a, Variable<'a>>),
    Function(Ref<'a, Function<'a>>),
    Struct(Ref<'a, Struct<'a>>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Variable<'a> {
    linkage: Linkage,
    name: String,
    defined: Cell<bool>,
    ty: QualType<'a>,
    initial: RefCell<Option<Box<expr::Expression<'a>>>>,
}

impl<'a> Variable<'a> {
    pub fn is_defined(&self) -> bool {
        self.defined.get()
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Linkage {
    None,
    Internal,
    External,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function<'a> {
    name: String,
    linkage: Linkage,
    ty: Box<FunctionTy<'a>>,
    noreturn: Cell<bool>,
    inline: Cell<bool>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct QualType<'a> {
    ty: Type<'a>,
    volatile: bool,
    constant: bool,
}

impl<'a> QualType<'a> {
    fn merge(&self, _other: &QualType<'a>) {
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

    fn build_qual_type(&self, ty: Type<'a>) -> Result<QualType<'a>, Error> {
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

    fn build(mut self, env: &Env<'a>) -> Result<QualType<'a>, Error> {
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

pub fn interpret_declaration<'a>(
    alloc: &'a Alloc<'a>,
    env: &mut Env<'a>,
    file_scope: bool,
    decl: &Node<ast::Declaration>,
) -> Result<Vec<Item<'a>>, Error> {
    let mut builder = TypeBuilder::new();
    let mut storage = None;
    let mut inline = false;
    let mut noreturn = false;
    let mut is_typedef = false;

    for spec in &decl.node.specifiers {
        let mut new_storage = None;
        match spec.node {
            ast::DeclarationSpecifier::StorageClass(ref s) => match s.node {
                ast::StorageClassSpecifier::ThreadLocal => {
                    return Err("_Thread_local is not supported")
                }
                ast::StorageClassSpecifier::Typedef => is_typedef = true,
                ref other => new_storage = Some(other),
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

    let linkage = match (file_scope, storage) {
        (true, Some(&ast::StorageClassSpecifier::Static)) => Linkage::Internal,
        (true, _) => Linkage::External,
        (false, _) => Linkage::None,
    };

    let inline = inline && storage.is_none();

    let base_qty = builder.build(env)?;

    let mut res = vec![];

    for init_declarator in &decl.node.declarators {
        let (qty, name) = derive_declarator(
            alloc,
            env,
            base_qty.clone(),
            &init_declarator.node.declarator,
        )?;

        let name = name.expect("declaration with abstract declartor");

        let value = match init_declarator.node.initializer.as_ref() {
            Some(v) => Some(expr::Expression::from_initializer(&v.node)?),
            None => None,
        };

        match (&qty.ty, is_typedef, value) {
            (_, true, None) => env.add_typedef(&name, qty.clone()),
            (_, true, Some(_)) => return Err("typedef is initialized"),

            (&Type::Function(ref fun_ty), false, None) => {
                let fun = match env.lookup_function(&name)? {
                    Some(fun) => {
                        fun.inline.set(fun.inline.get() && inline);
                        fun.noreturn.set(fun.noreturn.get() || noreturn);
                        fun
                    }
                    None => {
                        let fun = alloc.new_function(Function {
                            linkage: linkage,
                            name: name.clone(),
                            ty: fun_ty.clone(),
                            inline: inline.into(),
                            noreturn: noreturn.into(),
                        });
                        env.add_function(&name, fun);
                        fun
                    }
                };

                res.push(Item::Function(fun));
            }

            (&Type::Function(_), false, Some(_)) => return Err("function is initialized"),

            (_, false, value) => {
                let is_extern = storage == Some(&ast::StorageClassSpecifier::Extern);

                let var = match is_extern || file_scope {
                    true => env.lookup_variable(&name)?,
                    false => None,
                };

                let var = match var {
                    Some(var) => {
                        var.ty.merge(&qty);
                        var
                    }
                    None => {
                        let var = alloc.new_variable(Variable {
                            linkage: linkage,
                            defined: false.into(),
                            name: name.clone(),
                            ty: qty.clone(),
                            initial: None.into(),
                        });
                        env.add_variable(&name, var);
                        var
                    }
                };

                var.defined
                    .set(var.defined.get() || !is_extern || value.is_some());
                *var.initial.borrow_mut() = value;

                res.push(Item::Variable(var));
            }
        }
    }

    Ok(res)
}

fn derive_declarator<'a>(
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
    let qty = builder.build(env)?;
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
fn interpret_decl_str<'a>(alloc: &'a Alloc<'a>, decl_str: &str) -> Result<Vec<Item<'a>>, Error> {
    let conf = &Default::default();
    let parse =
        lang_c::driver::parse_preprocessed(conf, decl_str.to_owned()).expect("syntax error");
    interpret_translation_unit(alloc, &parse.unit).map(|u| u.items)
}

#[test]
fn test_decl() {
    let alloc = &Alloc::new();

    assert_eq!(
        interpret_decl_str(alloc, "extern int x, * const y;"),
        Ok(vec![
            Item::Variable(alloc.new_variable(Variable {
                linkage: Linkage::External,
                defined: false.into(),
                initial: None.into(),
                name: "x".into(),
                ty: QualType {
                    volatile: false,
                    constant: false,
                    ty: Type::SInt,
                },
            })),
            Item::Variable(alloc.new_variable(Variable {
                linkage: Linkage::External,
                defined: false.into(),
                initial: None.into(),
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
fn test_external_def() {
    let alloc = &Alloc::new();

    let x = alloc.new_variable(Variable {
        linkage: Linkage::Internal,
        defined: true.into(),
        initial: None.into(),
        name: "x".into(),
        ty: QualType {
            volatile: false,
            constant: false,
            ty: Type::SInt,
        },
    });

    let y = alloc.new_variable(Variable {
        linkage: Linkage::External,
        defined: false.into(),
        initial: None.into(),
        name: "y".into(),
        ty: QualType {
            volatile: false,
            constant: false,
            ty: Type::SInt,
        },
    });

    let z = alloc.new_variable(Variable {
        linkage: Linkage::External,
        defined: true.into(),
        initial: None.into(),
        name: "z".into(),
        ty: QualType {
            volatile: false,
            constant: false,
            ty: Type::SInt,
        },
    });

    assert_eq!(
        interpret_decl_str(
            alloc,
            "static int x; extern int x; \
             extern int y; extern int y; \
             extern int z; int z;"
        ),
        Ok(vec![
            Item::Variable(x),
            Item::Variable(x),
            Item::Variable(y),
            Item::Variable(y),
            Item::Variable(z),
            Item::Variable(z),
        ])
    );
}

#[test]
fn test_extern_init() {
    let alloc = &Alloc::new();
    assert_eq!(
        interpret_decl_str(alloc, "extern int a = 1;"),
        Ok(vec![
            Item::Variable(
                alloc.new_variable(Variable {
                    name: "a".into(),
                    linkage: Linkage::External,
                    defined: true.into(),
                    initial: Some(
                        expr::Expression::Constant(
                            expr::Constant::Integer(expr::Integer {
                                base: expr::IntegerBase::Decimal,
                                ty: Type::SInt,
                                number: "1".into(),
                            }).into(),
                        ).into(),
                    ).into(),
                    ty: QualType {
                        volatile: false,
                        constant: false,
                        ty: Type::SInt,
                    },
                }),
            ),
        ])
    );
}

#[test]
fn test_typedef() {
    let alloc = &Alloc::new();
    assert_eq!(
        interpret_decl_str(alloc, "volatile int typedef a, *b; a c; const b d;"),
        Ok(vec![
            Item::Variable(alloc.new_variable(Variable {
                linkage: Linkage::External,
                defined: true.into(),
                initial: None.into(),
                name: "c".into(),
                ty: QualType {
                    volatile: true,
                    constant: false,
                    ty: Type::SInt,
                },
            })),
            Item::Variable(alloc.new_variable(Variable {
                linkage: Linkage::External,
                defined: true.into(),
                initial: None.into(),
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

#[test]
fn test_typedef_fn() {
    let alloc = &Alloc::new();
    assert_eq!(
        interpret_decl_str(alloc, "typedef int foo(); foo a;"),
        Ok(vec![
            Item::Function(alloc.new_function(Function {
                linkage: Linkage::External,
                name: "a".into(),
                inline: false.into(),
                noreturn: false.into(),
                ty: Box::new(FunctionTy {
                    return_type: Type::SInt.into(),
                    parameters: Vec::new(),
                    variadic: false,
                }),
            })),
        ])
    )
}

#[derive(PartialEq, Clone, Debug)]
pub struct Struct<'a> {
    kind: StructKind,
    tag: Option<String>,
    fields: RefCell<Option<Vec<Ref<'a, Field<'a>>>>>,
    rust_name: RefCell<Option<String>>,
}

impl<'a> Struct<'a> {
    fn new(kind: StructKind, tag: Option<String>) -> Struct<'a> {
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

    fn postprocess(&self, env: &mut Env) {
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
    name: Option<String>,
    rust_name: RefCell<Option<String>>,
    ty: QualType<'a>,
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

#[test]
fn test_struct() {
    let alloc = &Alloc::new();

    let decls = interpret_decl_str(alloc, "struct x { struct x (*next); } head;").unwrap();
    assert!(decls.len() == 2);
    let decl = decls.get(0).unwrap();
    let head = match *decl {
        Item::Variable(v) => v,
        _ => panic!("not a variable"),
    };

    assert_eq!(head.name, "head");

    let head_sty = match head.ty.ty {
        Type::Struct(s) => s,
        _ => panic!("head has wrong type: {:#?}", head.ty),
    };

    let fields_ref = head_sty.fields.borrow();
    let fields = fields_ref.as_ref().unwrap();
    assert_eq!(fields.len(), 1);

    let next = fields.get(0).unwrap();
    match next.ty.ty {
        Type::Pointer(ref qty) => match qty.ty {
            Type::Struct(next_sty) => assert_eq!(next_sty.same_as(&head_sty), true),
            _ => panic!("next has wrong type: {:#?}", next.ty),
        },
        _ => panic!("next has wrong type: {:#?}", next.ty),
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionTy<'a> {
    return_type: QualType<'a>,
    parameters: Vec<Parameter<'a>>,
    variadic: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Parameter<'a> {
    name: Option<String>,
    ty: QualType<'a>,
}

fn derive_func_type<'a>(
    alloc: &'a Alloc<'a>,
    env: &mut Env<'a>,
    ret_ty: QualType<'a>,
    fd: &Node<ast::FunctionDeclarator>,
) -> Result<QualType<'a>, Error> {
    let params = fd.node
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

fn derive_func_type_kr<'a>(
    ret_ty: QualType<'a>,
    fs: &Vec<Node<ast::Identifier>>,
) -> Result<QualType<'a>, Error> {
    let default_ty = QualType {
        volatile: false,
        constant: false,
        ty: Type::SInt,
    };

    let params = fs.iter()
        .map(|id| {
            Parameter {
                name: Some(id.node.name.clone()),
                ty: default_ty.clone(),
            }
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

#[test]
fn test_function_ptr() {
    let alloc = &Alloc::new();
    assert_eq!(
        interpret_decl_str(alloc, "int (*p)(int, ...);"),
        Ok(vec![
            Item::Variable(
                alloc.new_variable(Variable {
                    name: "p".into(),
                    defined: true.into(),
                    linkage: Linkage::External,
                    initial: None.into(),
                    ty: Type::Pointer(
                        FunctionTy {
                            variadic: true,
                            return_type: Type::SInt.into(),
                            parameters: vec![
                                Parameter {
                                    name: None,
                                    ty: Type::SInt.into(),
                                },
                            ],
                        }.into(),
                    ).into(),
                }),
            ),
        ])
    );
}

pub fn interpret_translation_unit<'a>(
    alloc: &'a Alloc<'a>,
    unit: &ast::TranslationUnit,
) -> Result<Unit<'a>, Error> {
    let mut env = Env::new();
    let mut items = Vec::new();

    for ed in &unit.0 {
        match ed.node {
            ast::ExternalDeclaration::Declaration(ref decl) => {
                items.extend(interpret_declaration(alloc, &mut env, true, decl)?)
            }
            _ => unimplemented!(),
        }
    }

    env.finalize(&mut items);

    add_initializers(&items)?;

    Ok(Unit { items: items })
}

fn add_initializers<'a>(items: &[Item<'a>]) -> Result<(), Error> {
    for item in items {
        match *item {
            Item::Variable(ref v) => {
                let mut init = v.initial.borrow_mut();
                if init.is_none() {
                    *init = Some(expr::Expression::new_zero(v.ty.ty.clone())?)
                }
            }
            _ => (),
        }
    }

    Ok(())
}

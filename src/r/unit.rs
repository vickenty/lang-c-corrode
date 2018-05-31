use c::ty::Type as CType;
use c::unit::Item as CItem;
use c::unit::Unit as CUnit;
use c::unit::Variable as CVariable;
use fmt;

pub struct Unit {
    items: Vec<Item>,
}

impl Unit {
    pub fn from_c(unit: &CUnit) -> Unit {
        Unit {
            items: unit.items.iter().map(|i| Item::from_c(i)).collect(),
        }
    }
}

impl fmt::ToCode for Unit {
    fn to_code(&self, fmt: &mut fmt::Formatter) {
        for item in &self.items {
            item.to_code(fmt);
        }
    }
}

pub enum Item {
    Static(Static),
    Extern(Extern),
}

impl Item {
    pub fn from_c(item: &CItem) -> Item {
        match item {
            CItem::Variable(var) => {
                let def = Static::from_c(&var, var.is_defined());
                if var.is_defined() {
                    Item::Static(def)
                } else {
                    Item::Extern(Extern::Static(def))
                }
            }
            _ => unimplemented!(),
        }
    }
}

impl fmt::ToCode for Item {
    fn to_code(&self, fmt: &mut fmt::Formatter) {
        match *self {
            Item::Static(ref s) => s.to_code(fmt),
            Item::Extern(ref e) => e.to_code(fmt),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Static {
    name: String,
    ty: Type,
    initial: Option<Expr>,
}

impl Static {
    pub fn from_c(var: &CVariable, initialize: bool) -> Static {
        let ty = Type::from_c(&var.ty.ty);
        let initial = match *var.initial.borrow() {
            Some(_) => unimplemented!(),
            None if initialize => Some(Expr::new_zero(&ty)),
            None => None,
        };

        Static {
            name: var.name.clone(),
            ty: ty,
            initial: initial,
        }
    }
}

impl fmt::ToCode for Static {
    fn to_code(&self, fmt: &mut fmt::Formatter) {
        tokln!(fmt, "#[no_mangle]");
        toks!(fmt, "pub static mut ", self.name, ": ", self.ty.name());
        if let Some(ref init) = self.initial {
            toks!(fmt, " = ", init);
        }
        tokln!(fmt, ";");
    }
}

#[derive(Debug, Clone)]
pub enum Extern {
    Static(Static),
}

impl Extern {}

impl fmt::ToCode for Extern {
    fn to_code(&self, fmt: &mut fmt::Formatter) {
        tokln!(fmt, "extern {");
        block!(fmt, {
            match *self {
                Extern::Static(ref s) => s.to_code(fmt),
            }
        });
        tokln!(fmt, "}");
    }
}

#[derive(Debug, Clone)]
pub enum Type {
    Void,
    Int(&'static str),
    Float(&'static str),
    Bool,
    Pointer(Box<Type>),
}

impl Type {
    pub fn from_c(ty: &CType) -> Type {
        match *ty {
            CType::Void => Type::Void,
            CType::Char => Type::Int("c_char"),
            CType::SChar => Type::Int("c_schar"),
            CType::UChar => Type::Int("c_uchar"),
            CType::SInt => Type::Int("c_int"),
            CType::UInt => Type::Int("c_uint"),
            CType::SShort => Type::Int("c_short"),
            CType::UShort => Type::Int("c_ushort"),
            CType::SLong => Type::Int("c_long"),
            CType::ULong => Type::Int("c_ulong"),
            CType::SLongLong => Type::Int("c_longlong"),
            CType::ULongLong => Type::Int("c_ulonglong"),
            CType::Float => Type::Float("c_float"),
            CType::Double => Type::Float("c_double"),
            CType::Bool => Type::Bool,
            CType::Pointer(ref qty) => Type::Pointer(Box::new(Type::from_c(&qty.ty))),
            _ => unimplemented!(),
        }
    }

    pub fn name(&self) -> TypeName {
        TypeName(self)
    }
}

pub struct TypeName<'a>(&'a Type);

impl<'a> fmt::ToCode for TypeName<'a> {
    fn to_code(&self, fmt: &mut fmt::Formatter) {
        match *self.0 {
            Type::Void => "c_void".to_code(fmt),
            Type::Int(name) => name.to_code(fmt),
            Type::Float(name) => name.to_code(fmt),
            Type::Bool => "c_bool".to_code(fmt),
            Type::Pointer(ref ty) => toks!(fmt, "*mut ", ty.name()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Integer(Integer),
    Cast(Box<Expr>, Type),
}

impl Expr {
    pub fn new_zero(ty: &Type) -> Expr {
        match *ty {
            Type::Int(_) => Expr::Integer(Integer::new_zero()),
            Type::Pointer(_) => {
                Expr::Cast(Box::new(Expr::Integer(Integer::new_zero())), ty.clone())
            }
            _ => unimplemented!(),
        }
    }
}

impl fmt::ToCode for Expr {
    fn to_code(&self, fmt: &mut fmt::Formatter) {
        match *self {
            Expr::Integer(ref i) => i.to_code(fmt),
            Expr::Cast(ref expr, ref ty) => toks!(fmt, "(", expr, ") as ", ty.name()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Integer {
    value: Box<str>,
}

impl Integer {
    fn new_zero() -> Integer {
        Integer { value: "0".into() }
    }
}

impl fmt::ToCode for Integer {
    fn to_code(&self, fmt: &mut fmt::Formatter) {
        toks!(fmt, self.value);
    }
}

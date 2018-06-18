use super::Expression;
use {c, fmt};

pub struct Unit {
    items: Vec<Item>,
}

impl Unit {
    pub fn from_c(unit: &c::Unit) -> Unit {
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
    pub fn from_c(item: &c::Item) -> Item {
        match item {
            c::Item::Variable(var) => {
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
    initial: Option<Expression>,
}

impl Static {
    pub fn from_c(var: &c::Variable, initialize: bool) -> Static {
        let ty = Type::from_c(&var.ty.ty);

        let initial = match *var.initial.borrow() {
            Some(ref val) => Some(Expression::from_c(&val)),
            None if initialize => Some(Expression::new_zero(&ty)),
            None => None,
        };

        // C11 6.5.16.1 §2
        let initial = initial.map(|e| e.cast(&ty));

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

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Auto,
    Void,
    Int(&'static str),
    Float(&'static str),
    Bool,
    Pointer(Box<Type>),
}

impl Type {
    pub fn from_c(ty: &c::Type) -> Type {
        match *ty {
            c::Type::Void => Type::Void,
            c::Type::Char => Type::Int("c_char"),
            c::Type::SChar => Type::Int("c_schar"),
            c::Type::UChar => Type::Int("c_uchar"),
            c::Type::SInt => Type::Int("c_int"),
            c::Type::UInt => Type::Int("c_uint"),
            c::Type::SShort => Type::Int("c_short"),
            c::Type::UShort => Type::Int("c_ushort"),
            c::Type::SLong => Type::Int("c_long"),
            c::Type::ULong => Type::Int("c_ulong"),
            c::Type::SLongLong => Type::Int("c_longlong"),
            c::Type::ULongLong => Type::Int("c_ulonglong"),
            c::Type::Float => Type::Float("c_float"),
            c::Type::Double => Type::Float("c_double"),
            c::Type::Bool => Type::Bool,
            c::Type::Pointer(ref qty) => Type::Pointer(Box::new(Type::from_c(&qty.ty))),
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
            Type::Auto => "_".to_code(fmt),
            Type::Void => "c_void".to_code(fmt),
            Type::Int(name) => name.to_code(fmt),
            Type::Float(name) => name.to_code(fmt),
            Type::Bool => "c_bool".to_code(fmt),
            Type::Pointer(ref ty) => toks!(fmt, "*mut ", ty.name()),
        }
    }
}

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
}

impl Item {
    pub fn from_c(item: &CItem) -> Item {
        match item {
            CItem::Variable(var) if var.is_defined() => Item::Static(Static::from_c(&var)),
            _ => unimplemented!(),
        }
    }
}

impl fmt::ToCode for Item {
    fn to_code(&self, fmt: &mut fmt::Formatter) {
        match *self {
            Item::Static(ref s) => s.to_code(fmt),
        }
    }
}

pub struct Static {
    name: String,
    ty: Type,
    initial: Expr,
}

impl fmt::ToCode for Static {
    fn to_code(&self, fmt: &mut fmt::Formatter) {
        tokln!(fmt, "#[no_mangle]");
        tokln!(
            fmt,
            "pub static mut ",
            self.name,
            ": ",
            self.ty.name(),
            " = ",
            self.initial,
            ";"
        );
    }
}

impl Static {
    pub fn from_c(var: &CVariable) -> Static {
        let ty = Type::from_c(&var.ty.ty);
        let initial = match *var.initial.borrow() {
            Some(_) => unimplemented!(),
            None => Expr::new_zero(&ty),
        };

        Static {
            name: var.name.clone(),
            ty: ty,
            initial: initial,
        }
    }
}

pub enum Type {
    CInt,
}

pub struct TypeName<'a>(&'a Type);

impl Type {
    pub fn from_c(ty: &CType) -> Type {
        match *ty {
            CType::SInt => Type::CInt,
            _ => unimplemented!(),
        }
    }

    pub fn name(&self) -> TypeName {
        TypeName(self)
    }
}

impl<'a> fmt::ToCode for TypeName<'a> {
    fn to_code(&self, fmt: &mut fmt::Formatter) {
        match *self.0 {
            Type::CInt => toks!(fmt, "c_int"),
        }
    }
}

pub enum Expr {
    Integer(Integer),
}

impl Expr {
    pub fn new_zero(ty: &Type) -> Expr {
        match *ty {
            Type::CInt => Expr::Integer(Integer::new_zero()),
        }
    }
}

impl fmt::ToCode for Expr {
    fn to_code(&self, fmt: &mut fmt::Formatter) {
        match *self {
            Expr::Integer(ref i) => i.to_code(fmt),
        }
    }
}

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

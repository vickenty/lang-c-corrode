use c::IntegerBase;
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
    initial: Option<Expr>,
}

impl Static {
    pub fn from_c(var: &c::Variable, initialize: bool) -> Static {
        let ty = Type::from_c(&var.ty.ty);

        let initial = match *var.initial.borrow() {
            Some(ref val) => Some(Expr::from_c(&val)),
            None if initialize => Some(Expr::new_zero(&ty)),
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

#[derive(Debug, Clone)]
pub enum Expr {
    Integer(Integer),
    Float(Float),
    Cast(Box<Expr>, Type),
    Unary(&'static str, Box<Expr>),
    Infix(&'static str, Box<Expr>, Box<Expr>),
}

impl Expr {
    fn new_zero(ty: &Type) -> Expr {
        match *ty {
            Type::Int(_) | Type::Pointer(_) => Expr::Integer(Integer::new_zero()).cast(ty),
            Type::Float(_) => Expr::Float(Float::new_zero()).cast(ty),
            _ => unimplemented!(),
        }
    }

    fn from_c<'a>(val: &c::Expression<'a>) -> Expr {
        match *val {
            c::Expression::Constant(ref c) => Expr::from_const(&c),
            c::Expression::Binary(ref op) => Expr::from_binop(op),
            c::Expression::Unary(ref op) => Expr::from_unop(op),
            _ => unimplemented!(),
        }
    }

    fn from_const(c: &c::Constant) -> Expr {
        let expr = match *c {
            c::Constant::Integer(ref i) => Expr::Integer(Integer::from_c(i)),
            c::Constant::Float(ref f) => Expr::Float(Float::from_c(f)),
        };

        // Assumes that `literal as ty` has the same effect as explicitly
        // typing literal with the suffix that corresponds to ty.
        Expr::Cast(Box::new(expr), Type::from_c(&c.ty()))
    }

    fn from_unop(o: &c::Unary) -> Expr {
        let arg = Box::new(Expr::from_c(&o.operand));
        match o.operator {
            c::UnaryOperator::Minus => Expr::Unary("-", arg),
            _ => unimplemented!(),
        }
    }

    fn from_binop(o: &c::Binary) -> Expr {
        let lhs = Box::new(Expr::from_c(&o.lhs));
        let rhs = Box::new(Expr::from_c(&o.rhs));

        let op = match o.operator {
            c::BinaryOperator::Multiply => "*",
            c::BinaryOperator::Divide => "/",
            c::BinaryOperator::Modulo => "%",
            c::BinaryOperator::Plus => "+",
            c::BinaryOperator::Minus => "-",
            c::BinaryOperator::ShiftLeft => "<<",
            c::BinaryOperator::ShiftRight => ">>",
            c::BinaryOperator::BitwiseAnd => "&",
            c::BinaryOperator::BitwiseXor => "^",
            c::BinaryOperator::BitwiseOr => "|",
            _ => unimplemented!(),
        };

        Expr::Infix(op, lhs, rhs)
    }

    fn cast(self, ty: &Type) -> Expr {
        if self.ty() != *ty {
            Expr::Cast(Box::new(self), ty.clone())
        } else {
            self
        }
    }

    fn ty(&self) -> Type {
        match *self {
            Expr::Integer(_) => Type::Auto,
            Expr::Float(_) => Type::Auto,
            Expr::Cast(_, ref ty) => ty.clone(),
            Expr::Unary(_, ref arg) => arg.ty(),
            Expr::Infix(_, ref lhs, _) => lhs.ty(),
        }
    }
}

impl fmt::ToCode for Expr {
    fn to_code(&self, fmt: &mut fmt::Formatter) {
        match *self {
            Expr::Integer(ref i) => i.to_code(fmt),
            Expr::Float(ref f) => f.to_code(fmt),
            Expr::Cast(ref expr, ref ty) => toks!(fmt, "(", expr, ") as ", ty.name()),
            Expr::Unary(op, ref arg) => toks!(fmt, op, "(", arg, ")"),
            Expr::Infix(op, ref lhs, ref rhs) => toks!(fmt, "(", lhs, ") ", op, " (", rhs, ")"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Integer {
    base: IntegerBase,
    value: Box<str>,
}

impl Integer {
    fn new_zero() -> Integer {
        Integer {
            base: IntegerBase::Decimal,
            value: "0".into(),
        }
    }

    fn from_c(i: &c::Integer) -> Integer {
        Integer {
            base: i.base.clone(),
            value: i.number.clone(),
        }
    }
}

impl fmt::ToCode for Integer {
    fn to_code(&self, fmt: &mut fmt::Formatter) {
        match self.base {
            IntegerBase::Decimal => (),
            IntegerBase::Hexademical => "0x".to_code(fmt),
            IntegerBase::Octal => "Oo".to_code(fmt),
        }
        self.value.to_code(fmt);
    }
}

#[derive(Debug, Clone)]
pub struct Float {
    number: Box<str>,
}

impl Float {
    fn new_zero() -> Float {
        Float {
            number: "0".into(),
        }
    }

    fn from_c(f: &c::Float) -> Float {
        Float {
            number: f.number.clone(),
        }
    }
}

impl fmt::ToCode for Float {
    fn to_code(&self, fmt: &mut fmt::Formatter) {
        self.number.to_code(fmt);
    }
}

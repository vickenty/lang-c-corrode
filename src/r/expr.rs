use c;
use c::IntegerBase;
use super::Type;
use fmt;

#[derive(Debug, Clone)]
pub enum Expr {
    Integer(Integer),
    Float(Float),
    Cast(Box<Expr>, Type),
    Unary(&'static str, Box<Expr>),
    Infix(&'static str, Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn new_zero(ty: &Type) -> Expr {
        match *ty {
            Type::Int(_) | Type::Pointer(_) => Expr::Integer(Integer::new_zero()).cast(ty),
            Type::Float(_) => Expr::Float(Float::new_zero()).cast(ty),
            _ => unimplemented!(),
        }
    }

    pub fn from_c<'a>(val: &c::Expression<'a>) -> Expr {
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

    pub fn cast(self, ty: &Type) -> Expr {
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

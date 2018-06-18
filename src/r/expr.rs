use super::Type;
use c;
use c::IntegerBase;
use fmt;

#[derive(Debug, Clone)]
pub enum Expression {
    Integer(Integer),
    Float(Float),
    Cast(Box<Expression>, Type),
    Unary(&'static str, Box<Expression>),
    Infix(&'static str, Box<Expression>, Box<Expression>),
}

impl Expression {
    pub fn new_zero(ty: &Type) -> Expression {
        match *ty {
            Type::Int(_) | Type::Pointer(_) => Expression::Integer(Integer::new_zero()).cast(ty),
            Type::Float(_) => Expression::Float(Float::new_zero()).cast(ty),
            _ => unimplemented!(),
        }
    }

    pub fn from_c<'a>(val: &c::Expression<'a>) -> Expression {
        match *val {
            c::Expression::Constant(ref c) => Expression::from_const(&c),
            c::Expression::Binary(ref op) => Expression::from_binop(op),
            c::Expression::Unary(ref op) => Expression::from_unop(op),
            _ => unimplemented!(),
        }
    }

    fn from_const(c: &c::Constant) -> Expression {
        let expr = match *c {
            c::Constant::Integer(ref i) => Expression::Integer(Integer::from_c(i)),
            c::Constant::Float(ref f) => Expression::Float(Float::from_c(f)),
        };

        // Assumes that `literal as ty` has the same effect as explicitly
        // typing literal with the suffix that corresponds to ty.
        Expression::Cast(Box::new(expr), Type::from_c(&c.ty()))
    }

    fn from_unop(o: &c::Unary) -> Expression {
        let arg = Box::new(Expression::from_c(&o.operand));
        match o.operator {
            c::UnaryOperator::Minus => Expression::Unary("-", arg),
            _ => unimplemented!(),
        }
    }

    fn from_binop(o: &c::Binary) -> Expression {
        let lhs = Box::new(Expression::from_c(&o.lhs));
        let rhs = Box::new(Expression::from_c(&o.rhs));

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

        Expression::Infix(op, lhs, rhs)
    }

    pub fn cast(self, ty: &Type) -> Expression {
        if self.ty() != *ty {
            Expression::Cast(Box::new(self), ty.clone())
        } else {
            self
        }
    }

    fn ty(&self) -> Type {
        match *self {
            Expression::Integer(_) => Type::Auto,
            Expression::Float(_) => Type::Auto,
            Expression::Cast(_, ref ty) => ty.clone(),
            Expression::Unary(_, ref arg) => arg.ty(),
            Expression::Infix(_, ref lhs, _) => lhs.ty(),
        }
    }
}

impl fmt::ToCode for Expression {
    fn to_code(&self, fmt: &mut fmt::Formatter) {
        match *self {
            Expression::Integer(ref i) => i.to_code(fmt),
            Expression::Float(ref f) => f.to_code(fmt),
            Expression::Cast(ref expr, ref ty) => toks!(fmt, "(", expr, ") as ", ty.name()),
            Expression::Unary(op, ref arg) => toks!(fmt, op, "(", arg, ")"),
            Expression::Infix(op, ref lhs, ref rhs) => {
                toks!(fmt, "(", lhs, ") ", op, " (", rhs, ")")
            }
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
        Float { number: "0".into() }
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

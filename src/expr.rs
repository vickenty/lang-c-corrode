use ast;
use {Error, Type};

pub use ast::IntegerBase;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression<'a> {
    Constant(Box<Constant<'a>>),
}

impl<'a> Expression<'a> {
    pub fn from_ast(expr: &ast::Expression) -> Result<Box<Expression<'a>>, Error> {
        Ok(Box::new(match *expr {
            ast::Expression::Constant(ref c) => {
                Expression::Constant(Constant::from_ast(&c.node)?.into())
            }
            _ => unimplemented!(),
        }))
    }

    pub fn from_initializer(init: &ast::Initializer) -> Result<Box<Self>, Error> {
        match *init {
            ast::Initializer::Expression(ref expr) => Self::from_ast(&expr.node),
            ast::Initializer::List(_) => unimplemented!(),
        }
    }

    pub fn ty(&self) -> &Type<'a> {
        match *self {
            Expression::Constant(ref c) => c.ty(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Constant<'a> {
    Integer(Integer<'a>),
    Float(Float<'a>),
}

impl<'a> Constant<'a> {
    pub fn from_ast(c: &ast::Constant) -> Result<Constant<'a>, Error> {
        Ok(match *c {
            ast::Constant::Integer(ref int) => Constant::Integer(Integer::from_ast(int)?.into()),
            ast::Constant::Float(ref flt) => Constant::Float(Float::from_ast(flt)?.into()),
            _ => unimplemented!(),
        })
    }

    fn ty(&self) -> &Type<'a> {
        match *self {
            Constant::Integer(ref i) => &i.ty,
            Constant::Float(ref f) => &f.ty,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Integer<'a> {
    pub ty: Type<'a>,
    pub base: ast::IntegerBase,
    pub number: Box<str>,
}

impl<'a> Integer<'a> {
    fn guess_type(i: &ast::Integer) -> Result<Type<'a>, Error> {
        use std::os::raw;

        let radix = match i.base {
            ast::IntegerBase::Decimal => 10,
            ast::IntegerBase::Hexademical => 16,
            ast::IntegerBase::Octal => 8,
        };

        macro_rules! try_size {
            ($min:ident, $raw_s:ident, $raw_u:ident, $ty_s:ident, $ty_u:ident) => (
                if i.suffix.size as usize <= ast::IntegerSize::$min as usize {
                    if !i.suffix.unsigned {
                        if let Ok(_) = raw::$raw_s::from_str_radix(&*i.number, radix) {
                            return Ok(Type::$ty_s);
                        }
                    }
                    if i.suffix.unsigned || radix != 10 {
                        if let Ok(_) = raw::$raw_u::from_str_radix(&*i.number, radix) {
                            return Ok(Type::$ty_u);
                        }
                    }
                }
            )
        }

        try_size!(Int, c_int, c_uint, SInt, UInt);
        try_size!(Long, c_long, c_ulong, SLong, ULong);
        try_size!(LongLong, c_longlong, c_ulonglong, SLongLong, ULongLong);

        Err("invalid integer constant")
    }

    fn from_ast(i: &ast::Integer) -> Result<Integer<'a>, Error> {
        if i.suffix.imaginary {
            return Err("unsupported complex integer literal");
        }

        Ok(Integer {
            ty: Integer::guess_type(i)?,
            base: i.base.clone(),
            number: i.number.clone(),
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Float<'a> {
    pub ty: Type<'a>,
    pub number: Box<str>,
}

impl<'a> Float<'a> {
    fn from_ast(f: &ast::Float) -> Result<Float<'a>, Error> {
        match f.base {
            ast::FloatBase::Decimal => (),
            ast::FloatBase::Hexademical => return Err("unsupported hexademical float literal"),
        };

        let ty = match f.suffix.format {
            ast::FloatFormat::Float => Type::Float,
            ast::FloatFormat::Double => Type::Double,
            ast::FloatFormat::LongDouble => return Err("unsupported long double float literal"),
            ast::FloatFormat::TS18661Format(_) => return Err("unsupported TS18661 float literal"),
        };

        Ok(Float {
            ty: ty,
            number: f.number.clone(),
        })
    }
}

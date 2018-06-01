use std::collections::HashMap;

use ast;
use c::{Struct, StructKind, Type};
use {Error, Ref, RefId};

pub use ast::BinaryOperator;
pub use ast::IntegerBase;
pub use ast::UnaryOperator;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression<'a> {
    Constant(Box<Constant<'a>>),
    Unary(Box<Unary<'a>>),
    Binary(Box<Binary<'a>>),
    Struct(Box<StructValue<'a>>),
    Cast(Box<Cast<'a>>),
}

impl<'a> Expression<'a> {
    pub fn new_zero(ty: Type<'a>) -> Result<Box<Expression<'a>>, Error> {
        Ok(Box::new(match ty {
            Type::Void => return Err("cannot create value of type void"),
            Type::Char
            | Type::SChar
            | Type::UChar
            | Type::SInt
            | Type::UInt
            | Type::SShort
            | Type::UShort
            | Type::SLong
            | Type::ULong
            | Type::SLongLong
            | Type::ULongLong
            | Type::Bool => {
                Expression::Constant(Box::new(Constant::Integer(Integer::new_zero(ty))))
            }
            Type::Float | Type::Double => {
                Expression::Constant(Box::new(Constant::Float(Float::new_zero(ty))))
            }
            Type::Pointer(_) => {
                Expression::Constant(Box::new(Constant::Integer(Integer::new_zero(ty.clone()))))
            }
            Type::Struct(sty) => Expression::Struct(Box::new(StructValue::new_zero(sty)?)),
            _ => unimplemented!(),
        }))
    }

    pub fn new_cast(expr: Expression<'a>, ty: Type<'a>) -> Result<Box<Expression<'a>>, Error> {
        Ok(Box::new(Expression::Cast(Box::new(Cast::new(expr, ty)?))))
    }

    pub fn from_ast(expr: &ast::Expression) -> Result<Box<Expression<'a>>, Error> {
        Ok(Box::new(match *expr {
            ast::Expression::Constant(ref c) => {
                Expression::Constant(Constant::from_ast(&c.node)?.into())
            }
            ast::Expression::UnaryOperator(ref e) => {
                Expression::Unary(Unary::from_ast(&e.node)?.into())
            }
            ast::Expression::BinaryOperator(ref e) => {
                Expression::Binary(Binary::from_ast(&e.node)?.into())
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

    pub fn ty(&self) -> Type<'a> {
        match *self {
            Expression::Constant(ref c) => c.ty(),
            Expression::Unary(ref e) => e.ty(),
            Expression::Binary(ref e) => e.ty(),
            Expression::Struct(ref v) => Type::Struct(v.def),
            Expression::Cast(ref c) => c.ty.clone(),
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

    fn ty(&self) -> Type<'a> {
        match *self {
            Constant::Integer(ref i) => i.ty.clone(),
            Constant::Float(ref f) => f.ty.clone(),
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
    fn new_zero(ty: Type<'a>) -> Integer<'a> {
        Integer {
            ty: ty,
            base: ast::IntegerBase::Decimal,
            number: "0".into(),
        }
    }

    fn guess_type(i: &ast::Integer) -> Result<Type<'a>, Error> {
        use std::os::raw;

        let radix = match i.base {
            ast::IntegerBase::Decimal => 10,
            ast::IntegerBase::Hexademical => 16,
            ast::IntegerBase::Octal => 8,
        };

        macro_rules! try_size {
            ($min:ident, $raw_s:ident, $raw_u:ident, $ty_s:ident, $ty_u:ident) => {
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
            };
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
    fn new_zero(ty: Type<'a>) -> Float<'a> {
        Float {
            ty: ty,
            number: "0.0".into(),
        }
    }

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

#[derive(Debug, PartialEq, Clone)]
pub struct Unary<'a> {
    pub operator: UnaryOperator,
    pub operand: Box<Expression<'a>>,
    ty: Type<'a>,
}

impl<'a> Unary<'a> {
    fn from_ast(e: &ast::UnaryOperatorExpression) -> Result<Unary<'a>, Error> {
        let operand = Expression::from_ast(&e.operand.node)?;
        let ty = operand.ty().clone();

        Ok(Unary {
            operator: e.operator.node.clone(),
            operand: operand,
            ty: ty,
        })
    }

    fn ty(&self) -> Type<'a> {
        self.ty.clone()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Binary<'a> {
    pub operator: BinaryOperator,
    pub lhs: Box<Expression<'a>>,
    pub rhs: Box<Expression<'a>>,
    ty: Type<'a>,
}

impl<'a> Binary<'a> {
    fn from_ast(e: &ast::BinaryOperatorExpression) -> Result<Binary<'a>, Error> {
        let lhs = Expression::from_ast(&e.lhs.node)?;
        let rhs = Expression::from_ast(&e.rhs.node)?;
        let ty = binop_type(&lhs.ty(), &rhs.ty())?;

        Ok(Binary {
            operator: e.operator.node.clone(),
            lhs: lhs,
            rhs: rhs,
            ty: ty,
        })
    }

    fn ty(&self) -> Type<'a> {
        self.ty.clone()
    }
}

fn binop_type<'a>(lhs: &Type<'a>, rhs: &Type<'a>) -> Result<Type<'a>, Error> {
    if lhs == rhs {
        Ok(lhs.clone())
    } else {
        unimplemented!()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructValue<'a> {
    pub def: Ref<'a, Struct<'a>>,
    pub values: HashMap<RefId, Box<Expression<'a>>>,
}

impl<'a> StructValue<'a> {
    fn new_zero(def: Ref<'a, Struct<'a>>) -> Result<StructValue<'a>, Error> {
        let mut values = HashMap::new();

        let fields = def.fields.borrow();
        let fields = match *fields {
            Some(ref fields) => fields,
            None => return Err("can't create value for incomplete type"),
        };

        for field in fields {
            values.insert(Ref::id(field), Expression::new_zero(field.ty.ty.clone())?);

            if def.kind == StructKind::Union {
                break;
            }
        }

        Ok(StructValue {
            def: def,
            values: values,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cast<'a> {
    pub expr: Expression<'a>,
    pub ty: Type<'a>,
}

impl<'a> Cast<'a> {
    fn new(expr: Expression<'a>, ty: Type<'a>) -> Result<Cast<'a>, Error> {
        Ok(Cast { expr: expr, ty: ty })
    }
}

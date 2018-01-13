use ast;
use Error;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Constant(Box<Constant>),
}

impl Expression {
    pub fn from_ast(expr: &ast::Expression) -> Result<Box<Self>, Error> {
        Ok(match *expr {
            ast::Expression::Constant(ref c) => {
                Expression::Constant(Constant::from_ast(&c.node)?.into())
            }
            _ => unimplemented!(),
        }.into())
    }
    
    pub fn from_initializer(init: &ast::Initializer) -> Result<Box<Self>, Error> {
        match *init {
            ast::Initializer::Expression(ref expr) => Self::from_ast(&expr.node),
            ast::Initializer::List(_) => unimplemented!(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Constant {
    Integer(usize),
    Float(f64),
}

impl Constant {
    pub fn from_ast(c: &ast::Constant) -> Result<Constant, Error> {
        Ok(match *c {
            ast::Constant::Integer(ast::Integer::Decimal(ref val)) => {
                Constant::Integer(val.parse().map_err(|_| "invalid constant")?)
            }
            ast::Constant::Integer(ast::Integer::Octal(ref val)) => {
                Constant::Integer(val.parse().map_err(|_| "invalid constant")?)
            }
            _ => unimplemented!(),
        })
    }
}

use r;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Constant(Box<Constant>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Constant {
    Integer(Integer),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Integer {
    base: IntegerBase,
    digits: Box<str>,
    ty: r::ty::Integer,
}

#[derive(Debug, PartialEq, Clone)]
pub enum IntegerBase {
    Decimal,
    Octal,
    Hexademical,
}

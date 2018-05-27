//! Intermediate representation of the Rust language

pub mod expr;
pub mod gen;
pub mod ty;

pub struct Id<T: AsRef<str>>(pub T);

pub struct Str<T: AsRef<str>>(pub T);

extern crate dynamic_arena;
extern crate lang_c;
#[cfg(test)]
extern crate syn;

use lang_c::ast;
pub use lang_c::span::Node;

#[macro_use]
pub mod fmt;

pub mod alloc;
pub mod gen;

pub mod c;
pub mod r;

pub use alloc::*;
pub use c::env::*;
pub use c::expr;
pub use c::ty::*;
pub use c::unit::*;

pub type Error = &'static str;

use c::{expr, Function, FunctionTy, Item, Struct, StructKind, Type, Unit, Variable};

use fmt::{Formatter, ToCode};
use Ref;

impl<'a> ToCode for Unit<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        for item in &self.items {
            item.to_code(fmt);
        }
    }
}

impl<'a> ToCode for Item<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        match *self {
            Item::Struct(s) => s.to_code(fmt),
            Item::Variable(var) => var.to_code(fmt),
            Item::Function(f) => f.to_code(fmt),
        }
    }
}

impl<'a> ToCode for Variable<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        if self.is_defined() {
            static_define(self, fmt);
        } else {
            static_extern(self, fmt);
        }
    }
}

#[cfg_attr(rustfmt, rustfmt_skip)]
fn static_define(var: &Variable, fmt: &mut Formatter) {
    tokln!(fmt, "#[no_mangle]");
    tokln!(fmt, "pub static mut ", var.name, ": ", var.ty.ty, " = ", var.initial, ";");
}

fn static_extern<'a, 'w>(var: &Variable, fmt: &mut Formatter) {
    tokln!(fmt, "extern {");
    block!(fmt, {
        tokln!(fmt, "pub static mut ", var.name, ": ", var.ty.ty, ";");
    });
    tokln!(fmt, "}");
}

impl<'a> ToCode for expr::Expression<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        match *self {
            expr::Expression::Constant(ref c) => c.to_code(fmt),
            expr::Expression::Unary(ref e) => e.to_code(fmt),
            expr::Expression::Binary(ref e) => e.to_code(fmt),
            expr::Expression::Struct(ref v) => v.to_code(fmt),
            expr::Expression::Cast(ref c) => c.to_code(fmt),
        }
    }
}

impl<'a> ToCode for expr::Constant<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        match *self {
            expr::Constant::Integer(ref i) => i.to_code(fmt),
            expr::Constant::Float(ref f) => f.to_code(fmt),
        }
    }
}

impl<'a> ToCode for expr::Integer<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        toks!(fmt, "(", self.base, self.number, ") as ", self.ty);
    }
}

impl ToCode for expr::IntegerBase {
    fn to_code(&self, fmt: &mut Formatter) {
        match *self {
            expr::IntegerBase::Decimal => {}
            expr::IntegerBase::Octal => toks!(fmt, "0o"),
            expr::IntegerBase::Hexademical => toks!(fmt, "0x"),
        }
    }
}

impl<'a> ToCode for expr::Float<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        toks!(fmt, "(", self.number, ") as ", self.ty);
    }
}

impl<'a> ToCode for expr::Unary<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        toks!(fmt, self.operator, "(", self.operand, ")");
    }
}

impl ToCode for expr::UnaryOperator {
    fn to_code(&self, fmt: &mut Formatter) {
        toks!(
            fmt,
            match *self {
                expr::UnaryOperator::Plus => "+",
                expr::UnaryOperator::Minus => "-",
                expr::UnaryOperator::Complement => "!",
                expr::UnaryOperator::Negate => "!",
                _ => unimplemented!(),
            }
        );
    }
}

impl<'a> ToCode for expr::Binary<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        toks!(fmt, "(", self.lhs, ") ", self.operator, " (", self.rhs, ")");
    }
}

impl ToCode for expr::BinaryOperator {
    fn to_code(&self, fmt: &mut Formatter) {
        toks!(
            fmt,
            match *self {
                expr::BinaryOperator::Multiply => "*",
                expr::BinaryOperator::Divide => "/",
                expr::BinaryOperator::Modulo => "%",
                expr::BinaryOperator::Plus => "+",
                expr::BinaryOperator::Minus => "-",
                expr::BinaryOperator::ShiftLeft => "<<",
                expr::BinaryOperator::ShiftRight => ">>",
                _ => unimplemented!(),
            }
        );
    }
}

impl<'a> ToCode for expr::StructValue<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        tokln!(fmt, self.def.rust_name, " {");
        block!(fmt, {
            if let Some(ref fields) = *self.def.fields.borrow() {
                for field in fields {
                    if let Some(ref val) = self.values.get(&Ref::id(&field)) {
                        tokln!(fmt, field.rust_name, ": ", val, ",");
                    }
                }
            }
        });
        toks!(fmt, "}");
    }
}

impl<'a> ToCode for expr::Cast<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        toks!(fmt, "(", self.expr, ") as ", self.ty);
    }
}

impl<'a> ToCode for Struct<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        if self.is_complete_ty() {
            tokln!(fmt, "#[repr(C)]");
            tokln!(fmt, "pub ", self.kind, " ", self.rust_name, " {");
            block!(fmt, {
                if let Some(ref fields) = *self.fields.borrow() {
                    for field in fields {
                        tokln!(fmt, "pub ", field.rust_name, ": ", field.ty.ty, ",");
                    }
                }
            });
            tokln!(fmt, "}");
        } else {
            tokln!(fmt, "pub enum ", self.rust_name, "{}");
        }
    }
}

impl ToCode for StructKind {
    fn to_code(&self, fmt: &mut Formatter) {
        toks!(
            fmt,
            match *self {
                StructKind::Struct => "struct",
                StructKind::Union => "union",
            }
        );
    }
}

impl<'a> ToCode for Type<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        match *self {
            Type::Void => toks!(fmt, "c_void"),
            Type::Char => toks!(fmt, "c_char"),
            Type::SChar => toks!(fmt, "c_schar"),
            Type::UChar => toks!(fmt, "c_uchar"),
            Type::SInt => toks!(fmt, "c_int"),
            Type::UInt => toks!(fmt, "c_uint"),
            Type::SShort => toks!(fmt, "c_short"),
            Type::UShort => toks!(fmt, "c_ushort"),
            Type::SLong => toks!(fmt, "c_long"),
            Type::ULong => toks!(fmt, "c_ulong"),
            Type::SLongLong => toks!(fmt, "c_longlong"),
            Type::ULongLong => toks!(fmt, "c_ulonglong"),
            Type::Float => toks!(fmt, "c_float"),
            Type::Double => toks!(fmt, "c_double"),
            Type::Struct(s) => toks!(fmt, s.rust_name),
            Type::Pointer(ref ty) => toks!(fmt, "*mut ", ty.ty),
            _ => unimplemented!(),
        }
    }
}

impl<'a> ToCode for Function<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        tokln!(fmt, "extern \"C\" {");
        block!(fmt, {
            tokln!(fmt, "pub fn ", self.name, self.ty, ";");
        });
        tokln!(fmt, "}");
    }
}

impl<'a> ToCode for FunctionTy<'a> {
    fn to_code(&self, fmt: &mut Formatter) {
        toks!(fmt, "(");
        for p in &self.parameters {
            toks!(fmt, p.ty.ty, ", ");
        }
        if self.variadic {
            toks!(fmt, "...");
        }
        toks!(fmt, ")");

        match self.return_type.ty {
            Type::Void => (),
            ref ty => toks!(fmt, " -> ", ty),
        }
    }
}

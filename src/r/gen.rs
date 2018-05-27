use fmt::{Formatter, ToCode};
use r;

impl ToCode for r::ty::Integer {
    fn to_code(&self, fmt: &mut Formatter) {
        match *self {
            r::ty::Integer::Char => fmt.token("c_char"),
            r::ty::Integer::SChar => fmt.token("c_schar"),
            r::ty::Integer::UChar => fmt.token("c_uchar"),
            r::ty::Integer::Short => fmt.token("c_short"),
            r::ty::Integer::UShort => fmt.token("c_ushort"),
            r::ty::Integer::Int => fmt.token("c_int"),
            r::ty::Integer::UInt => fmt.token("c_uint"),
            r::ty::Integer::Long => fmt.token("c_long"),
            r::ty::Integer::ULong => fmt.token("c_ulong"),
            r::ty::Integer::LongLong => fmt.token("c_longlong"),
            r::ty::Integer::ULongLong => fmt.token("c_ulonglong"),
        }
    }
}

impl<T: AsRef<str>> ToCode for r::Id<T> {
    fn to_code(&self, fmt: &mut Formatter) {
        fmt.token(self.0.as_ref());
    }
}

impl<T: AsRef<str>> ToCode for r::Str<T> {
    fn to_code(&self, fmt: &mut Formatter) {
        fmt.string(self.0.as_ref());
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Integer(Integer),
}

/// C Integer types provided by `std::os::raw` module.
#[derive(Debug, PartialEq, Clone)]
pub enum Integer {
    Char,
    SChar,
    UChar,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
    LongLong,
    ULongLong,
}

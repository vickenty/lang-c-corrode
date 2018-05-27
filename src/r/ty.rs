#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Integer(Integer),
}

#[derive(Debug, PartialEq, Clone)]
/// C Integer types provided by `std::os::raw` module.
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

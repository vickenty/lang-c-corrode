use std::cell::RefCell;
use std::io;

pub trait Formatter {
    fn token(&mut self, token: &str);
    fn string(&mut self, string: &str);
    fn new_line(&mut self);
    fn add_offset(&mut self, offset: isize);
}

pub struct Writer<W: io::Write> {
    offset: isize,
    indent_next: bool,
    writer: W,
}

impl<W: io::Write> Writer<W> {
    pub fn new(writer: W) -> Self {
        Writer {
            offset: 0,
            indent_next: true,
            writer: writer,
        }
    }

    pub fn into_inner(self) -> W {
        self.writer
    }

    pub fn indent_maybe(&mut self) {
        if self.indent_next {
            self.indent_next = false;
            for _ in 0..self.offset {
                write!(self.writer, "    ").unwrap();
            }
        }
    }
}

impl<W: io::Write> Formatter for Writer<W> {
    fn token(&mut self, token: &str) {
        self.indent_maybe();
        write!(self.writer, "{}", token).unwrap();
    }

    fn string(&mut self, string: &str) {
        self.indent_maybe();
        write!(self.writer, "{:?}", string).unwrap();
    }

    fn new_line(&mut self) {
        write!(self.writer, "\n").unwrap();
        self.indent_next = true;
    }

    fn add_offset(&mut self, offset: isize) {
        self.offset += offset;
    }
}

macro_rules! toks {
    ( $fmt:expr, $( $item:expr ),* ) => ({
        $( $item.to_code($fmt); )*
    });
}

macro_rules! tokln {
    ( $fmt:expr, $( $item:expr ),* ) => ({
        toks!($fmt, $( $item ),* );
        $fmt.new_line();
    });
}

macro_rules! block {
    ( $fmt:expr, $body:block ) => (
        $fmt.add_offset(1);
        $body
        $fmt.add_offset(-1);
    );
}

pub trait ToCode {
    fn to_code(&self, fmt: &mut Formatter);
}

impl<'a> ToCode for &'a str {
    fn to_code(&self, fmt: &mut Formatter) {
        fmt.token(self);
    }
}

impl<'a> ToCode for String {
    fn to_code(&self, fmt: &mut Formatter) {
        fmt.token(self);
    }
}

impl<'a> ToCode for Box<str> {
    fn to_code(&self, fmt: &mut Formatter) {
        fmt.token(self);
    }
}

impl<T: ToCode> ToCode for Box<T> {
    fn to_code(&self, fmt: &mut Formatter) {
        self.as_ref().to_code(fmt);
    }
}

impl<T: ToCode> ToCode for RefCell<T> {
    fn to_code(&self, fmt: &mut Formatter) {
        self.borrow().to_code(fmt);
    }
}

impl<T: ToCode> ToCode for Option<T> {
    fn to_code(&self, fmt: &mut Formatter) {
        if let Some(ref v) = *self {
            v.to_code(fmt);
        }
    }
}

impl<'a, T: Fn(&mut Formatter)> ToCode for &'a T {
    fn to_code(&self, fmt: &mut Formatter) {
        tokln!(fmt, "{");
        self(fmt);
        toks!(fmt, "}");
    }
}

#[test]
fn test_codegen() {
    let mut fmt = Writer::new(io::Cursor::new(Vec::new()));

    {
        let fmt = &mut fmt;
        tokln!(fmt, "fn", " ", "main", "(", ")", " ", "{");
        block!(fmt, {
            tokln!(fmt, "println!", "(", "\"Hello world!\"", ")", ";");
        });
        tokln!(fmt, "}");
    }

    let val = String::from_utf8(fmt.into_inner().into_inner()).unwrap();
    assert_eq!(val, "fn main() {\n    println!(\"Hello world!\");\n}\n");
}

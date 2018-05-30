use std::error::Error;
use std::ffi::OsStr;
use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, Write};

pub fn generate() -> Result<(), Box<Error>> {
    let args = std::env::args_os().collect::<Vec<_>>();
    let in_path = &args[1];
    let out_path = &args[2];

    let tests = parse_tests_md(&in_path)?;
    write_test_code(&tests, &out_path)?;

    Ok(())
}

#[derive(Debug)]
struct TestCase {
    name: String,
    c_text: String,
    rust_text: String,
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum Block {
    None,
    C,
    Rust,
}

fn parse_tests_md(path: &OsStr) -> Result<Vec<TestCase>, Box<Error>> {
    let reader = BufReader::new(File::open(&path)?);
    let mut tests = Vec::new();
    let mut last_idx = 0;
    let mut block = Block::None;

    for (line_idx, line) in reader.lines().enumerate() {
        let line = line?;

        if line.starts_with("# ") {
            tests.push(TestCase {
                name: line[2..].to_owned(),
                c_text: String::new(),
                rust_text: String::new(),
            });
            last_idx = tests.len() - 1;
        }
        if line == "```c" {
            assert_eq!(
                block,
                Block::None,
                "{}:{}: unexpected C block start when in {:?} mode",
                path.to_string_lossy(),
                line_idx + 1,
                block
            );
            block = Block::C;
        } else if line == "```rust" {
            assert_eq!(
                block,
                Block::None,
                "{}:{}: unexpected Rust block start when in {:?} mode",
                path.to_string_lossy(),
                line_idx + 1,
                block
            );
            block = Block::Rust;
        } else if line == "```" {
            assert_ne!(
                block,
                Block::None,
                "{}:{}: unexpected block block end when not in a block",
                path.to_string_lossy(),
                line_idx + 1
            );
            block = Block::None;
        } else if block == Block::C {
            let buf = &mut tests[last_idx].c_text;
            buf.push_str(&line);
            buf.push_str("\n");
        } else if block == Block::Rust {
            let buf = &mut tests[last_idx].rust_text;
            buf.push_str(&line);
            buf.push_str("\n");
        }
    }

    Ok(tests)
}

fn write_test_code(tests: &[TestCase], output_path: &OsStr) -> Result<(), Box<Error>> {
    let output = &mut BufWriter::new(File::create(output_path)?);

    for test in tests {
        writeln!(output, "#[test]")?;
        writeln!(output, "fn {}() {{", test.name)?;
        writeln!(
            output,
            "    check!({:?}, {:?});",
            test.c_text, test.rust_text
        )?;
        writeln!(output, "}}")?;
    }
    Ok(())
}

fn main() {
    generate().unwrap();
}

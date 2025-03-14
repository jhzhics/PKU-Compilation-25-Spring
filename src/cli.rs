use std::{env, process::exit};

#[derive(Debug)]
pub enum Mode {
    /// Generate Koopa IR
    Koopa,
    /// Generate RISC-V assembly
    Riscv,
    /// Performance test
    Perf,
}
pub struct Cli {
    pub mode: Mode,
    /// Input file
    /// If not specified, read from stdin
    pub input_file: Option<String>,
    /// Output file
    /// If not specified, write to stdout
    pub output_file: Option<String>,
}

const USAGE: &str = r#"Usage: 
SysY [options] input_file -o output_file
Options:
    -koopa: Generate Koopa IR
    -riscv: Generate RISC-V assembly
    -perf: Performance test

If no input file is specified, read from stdin
If no output file is specified, write to stdout
"#;

impl Cli {
    pub fn parse() -> Self {
        let mode: Mode;
        let input_file;
        let output_file;

        if env::args().len() > 5 || env::args().len() == 1 {
            print!("{}", USAGE);
            exit(1);
        }

        let args: Vec<String> = env::args().collect();
        match args[1].as_str() {
            "-koopa" => {
                mode = Mode::Koopa;
            }
            "-riscv" => {
                mode = Mode::Riscv;
            }
            "-perf" => {
                mode = Mode::Perf;
            }
            _ => {
                print!("{}", USAGE);
                exit(1);
            }
        }

        if args.len() == 2 {
            input_file = None;
            output_file = None;
        } else if args.len() == 3 {
            input_file = Some(args[2].clone());
            output_file = None;
        } else if args.len() == 4 {
            print!("{}", USAGE);
            exit(1);
        } else {
            assert!(args.len() == 5);
            input_file = Some(args[2].clone());
            if args[3] != "-o" {
                print!("{}", USAGE);
                exit(1);
            }
            output_file = Some(args[4].clone());
        }

        Self {
            mode,
            input_file,
            output_file,
        }
    }
}

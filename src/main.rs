mod asm;
mod ast;
mod cli;
mod ir;
use cli::Cli;

use lalrpop_util::lalrpop_mod;
use std::{
    io::{Read, Write}
};
lalrpop_mod!(sysy);

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    let mut istream: Box<dyn Read>;
    let mut ostream: Box<dyn Write>;
    match cli.input_file {
        Some(file) => {
            istream = Box::new(std::fs::File::open(file)?);
        }
        None => {
            istream = Box::new(std::io::stdin());
        }
    }
    match cli.output_file {
        Some(file) => {
            ostream = Box::new(std::fs::File::create(file)?);
        }
        None => {
            ostream = Box::new(std::io::stdout());
        }
    }

    match cli.mode {
        cli::Mode::Koopa => excute_koopa(istream.as_mut(), ostream.as_mut()),
        cli::Mode::Riscv => excute_riscv(istream.as_mut(), ostream.as_mut()),
        cli::Mode::Perf => {
            return Err("Perf mode is not implemented yet".into());
        }
    }
}

fn excute_riscv(
    istream: &mut dyn Read,
    ostream: &mut dyn Write,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut input = String::new();
    istream.read_to_string(&mut input)?;

    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
    let koopa_program = ir::build_koopa(ast);

    let compiled = asm::compile(koopa_program);
    ostream.write(compiled.as_bytes())?;
    ostream.flush()?;
    Ok(())
}

fn excute_koopa(
    istream: &mut dyn Read,
    ostream: &mut dyn Write,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut input = String::new();
    istream.read_to_string(&mut input)?;

    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
    let koopa_program = ir::build_koopa(ast);
    koopa::back::KoopaGenerator::new(ostream).generate_on(&koopa_program)?;
    Ok(())
}

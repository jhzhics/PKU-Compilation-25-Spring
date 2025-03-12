mod cli;
use cli::Cli;
use ir::build_koopa;
mod ast;
mod ir;

use std::{io::{Read, Write}};
use lalrpop_util::lalrpop_mod;
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
        cli::Mode::Koopa => {
            excute_koopa(istream.as_mut(), ostream.as_mut())
        }
        cli::Mode::Riscv => {
            return Err("Riscv mode is not implemented yet".into());
        }
        cli::Mode::Perf => {
            return Err("Perf mode is not implemented yet".into());
        }
    }
}

fn excute_koopa(istream: &mut dyn Read, ostream: &mut dyn Write) -> Result<(), Box<dyn std::error::Error>>
{
    let mut input = String::new();
    istream.read_to_string(&mut input)?;

    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
    let koopa_program = build_koopa(ast)?;
    koopa::back::KoopaGenerator::new(ostream).generate_on(&koopa_program)?;
    Ok(())
}
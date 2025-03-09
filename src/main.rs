mod lexer;

mod cli;
use cli::Cli;
use lexer::Token;
use logos::Logos;
use std::{io::{Read, Write}};
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
    
    let mut lexer_result = Token::lexer(&input);
    let mut tokens = Vec::new();
    while let Some(token) = lexer_result.next() {
        match token {
            Err(_) => {
                eprintln!("Lexer Result: {:?}", Token::lexer(&input).collect::<Vec<_>>());
                return Err("Lexer error".into());
            }
            Ok(t) => {
                tokens.push(t);
            }
        }
    }
    println!("Lexer Result: {:?}", tokens);
    Ok(())
}
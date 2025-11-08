mod lexer;
mod parser;
mod source;

use crate::lexer::Lexer;
use crate::parser::ActionScriptError;
use anyhow::Result;
use clap::Parser;
use std::fs;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(name = "ruasc", version, author, about)]
struct Opt {
    /// Input source file to compile.
    #[arg(name = "FILE")]
    src: PathBuf,
}

fn main() -> Result<()> {
    let opt = Opt::parse();
    let src = fs::read_to_string(&opt.src)?;
    let filename = opt.src.file_name().unwrap().to_string_lossy();
    let tokens = Lexer::new(&src).into_vec();
    println!("{tokens:#?}");
    let document = parser::parse_document(&tokens)
        .map_err(|e| ActionScriptError::from_parse(&filename, &src, e));
    match document {
        Ok(document) => {
            println!("{document:#?}");
        }
        Err(err) => {
            println!("{err}");
        }
    }
    Ok(())
}

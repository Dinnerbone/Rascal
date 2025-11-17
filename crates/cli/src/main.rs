use anyhow::Result;
use clap::Parser;
use ruasc_as2::ActionScript;
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
    let actionscript = ActionScript::new(&filename, &src);
    let document = actionscript.to_ast();
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

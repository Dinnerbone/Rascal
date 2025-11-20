use anyhow::Result;
use clap::Parser;
use ruasc_as2::ActionScript;
use ruasc_as2_codegen::ast_to_pcode;
use ruasc_as2_pcode::{PCode, pcode_to_swf};
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
    let pcode = if filename.ends_with(".as") {
        let actionscript = ActionScript::new(&filename, &src);
        let document = actionscript.to_ast().unwrap_or_else(|e| panic!("{}", e));
        ast_to_pcode(&document)
    } else {
        let pcode = PCode::new(&filename, &src);
        pcode.to_actions().unwrap_or_else(|e| panic!("{}", e))
    };
    let swf = pcode_to_swf(&pcode)?;
    fs::write(opt.src.with_extension("swf"), swf)?;
    Ok(())
}

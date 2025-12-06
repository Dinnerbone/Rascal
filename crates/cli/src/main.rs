use anyhow::Result;
use clap::Parser;
use rascal_as2::program::{FileSystemSourceProvider, ProgramBuilder};
use rascal_as2_codegen::hir_to_pcode;
use rascal_as2_pcode::{PCode, pcode_to_swf};
use std::fs;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(name = "rascal", version, author, about)]
struct Opt {
    /// Input source file to compile.
    #[arg(name = "FILE")]
    src: PathBuf,
}

fn main() -> Result<()> {
    let opt = Opt::parse();
    let src = fs::read_to_string(&opt.src)?;
    let filename = opt.src.to_string_lossy();
    let pcode = if filename.ends_with(".as") {
        let mut builder =
            ProgramBuilder::new(FileSystemSourceProvider::with_root(PathBuf::from(".")));
        builder.add_script(&filename);
        let parsed = builder.build().unwrap_or_else(|e| panic!("{}", e));
        hir_to_pcode(&parsed.initial_script)
    } else {
        let pcode = PCode::new(&filename, &src);
        pcode.to_actions().unwrap_or_else(|e| panic!("{}", e))
    };
    let swf = pcode_to_swf(&pcode)?;
    fs::write(opt.src.with_extension("swf"), swf)?;
    Ok(())
}

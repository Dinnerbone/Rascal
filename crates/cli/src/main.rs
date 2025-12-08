use anyhow::Result;
use clap::Parser;
use rascal_as2::program::{FileSystemSourceProvider, ProgramBuilder};
use rascal_as2_codegen::hir_to_pcode;
use rascal_as2_pcode::{CompiledProgram, PCode, pcode_to_swf};
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Parser, Debug)]
#[command(name = "rascal", version, author, about)]
struct Opt {
    /// Input source file to compile.
    #[arg(name = "FILE")]
    src: PathBuf,
}

fn main() -> Result<()> {
    let opt = Opt::parse();
    let filename = opt.src.to_string_lossy();
    let pcode = if filename.ends_with(".as") {
        let root = opt.src.parent().unwrap_or_else(|| Path::new("."));
        let provider = FileSystemSourceProvider::with_root(root.to_owned());
        let mut builder = ProgramBuilder::new(provider);
        builder.add_script(&opt.src.file_name().unwrap().to_string_lossy());
        let parsed = builder.build().unwrap_or_else(|e| panic!("{}", e));
        hir_to_pcode(&parsed)
    } else {
        let src = fs::read_to_string(&opt.src)?;
        let pcode = PCode::new(&filename, &src);
        CompiledProgram {
            initializer: Some(pcode.to_actions().unwrap_or_else(|e| panic!("{}", e))),
            extra_modules: vec![],
        }
    };
    let swf = pcode_to_swf(&pcode)?;
    fs::write(opt.src.with_extension("swf"), swf)?;
    Ok(())
}

use anyhow::Result;
use clap::Parser;
use rascal_as2::program::{FileSystemSourceProvider, ProgramBuilder};
use rascal_as2_codegen::hir_to_pcode;
use rascal_as2_pcode::pcode_to_swf;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Parser, Debug)]
#[command(name = "rascal", version, author, about)]
struct Opt {
    /// A loose script (not class file) to add to the compiled program.
    ///
    /// Multiple scripts may be added to one program.
    /// Scripts will be executed in order that they are added.
    #[arg(name = "SCRIPT")]
    script: Vec<PathBuf>,

    /// Output file path. This will be overwritten if it already exists.
    ///
    /// If not specified, the first script path (with ".swf" instead of ".as") will be used.
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// SWF version to use.
    #[arg(short = 'v', long, default_value_t = 15)]
    swf_version: u8,

    /// Frame rate of the output SWF.
    #[arg(long, default_value_t = 24.0)]
    frame_rate: f32,
}

fn main() -> Result<()> {
    let opt = Opt::parse();

    let Some(first_script) = opt.script.first() else {
        return Err(anyhow::anyhow!("No scripts specified (see --help)."));
    };

    let root = first_script.parent().unwrap_or_else(|| Path::new("."));
    let provider = FileSystemSourceProvider::with_root(root.to_owned());
    let mut builder = ProgramBuilder::new(provider);
    for src in &opt.script {
        builder.add_script(&src.file_name().unwrap().to_string_lossy());
    }
    let parsed = builder.build().unwrap_or_else(|e| panic!("{}", e));
    let pcode = hir_to_pcode(&parsed, opt.swf_version);
    let output_path = opt
        .output
        .unwrap_or_else(|| first_script.with_extension("swf"));
    let swf = pcode_to_swf(&pcode, opt.frame_rate)?;
    fs::write(&output_path, swf)?;
    Ok(())
}

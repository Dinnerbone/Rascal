use anyhow::Result;
use clap::Parser;
use rascal::ProgramBuilder;
use rascal::{CompileOptions, FileSystemSourceProvider, SwfOptions};
use std::fs;
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(name = "rascal", version, author, about)]
struct Opt {
    /// A loose script (not class file) to add to the compiled program.
    ///
    /// Multiple scripts may be added to one program.
    /// Scripts will be executed in order that they are added, before any classes are loaded and after any raw pcode.
    ///
    /// It is valid to not specify any scripts - if so, the entry point of the program will be the first
    /// class (see `--class`) that contains a `static function main()` method.
    #[arg(name = "SCRIPT", help_heading = "Inputs")]
    script: Vec<PathBuf>,

    /// Path to some raw pcode to add to the compiled program.
    ///
    /// Multiple pcodes may be added to one program.
    /// Pcodes will be executed in order that they are added, before any scripts are executed.
    ///
    /// It is valid to not specify any scripts - if so, the entry point of the program will be the first
    /// class (see `--class`) that contains a `static function main()` method.
    #[arg(long, help_heading = "Inputs")]
    pcode: Vec<PathBuf>,

    /// Output file path. This will be overwritten if it already exists.
    ///
    /// If not specified:
    /// - If there are any scripts (see `SCRIPT`) specified, the first one will be used to determine the output path.
    /// - `output.swf` will be used if all else fails.
    #[arg(
        short,
        long,
        verbatim_doc_comment,
        help_heading = "Generated SWF Options"
    )]
    output: Option<PathBuf>,

    /// A name of a class to be added to the compiled program.
    ///
    /// This should be the canonical name of a class (e.g. `Foo` or `foo.bar.Baz`) which should be found in the current directory
    /// (e.g. at `Foo.as` or `foo/bar/Baz.as`).
    ///
    /// All classes will be loaded after any scripts (see `SCRIPT`).
    /// If no scripts are specified, one (and only one) class is expected to have an entry point in
    /// the form of a `static function main()` method.
    #[arg(short, long, help_heading = "Inputs")]
    class: Vec<String>,

    /// Adds a directory to be searched for classes.
    ///
    /// If no classpaths are specified, the current working directory will be used instead.
    #[arg(long, long, help_heading = "Inputs")]
    classpath: Vec<PathBuf>,

    /// SWF version to use.
    #[arg(
        short = 'v',
        long,
        default_value_t = 15,
        help_heading = "Generated SWF Options"
    )]
    swf_version: u8,

    /// Frame rate of the output SWF.
    #[arg(long, default_value_t = 24.0, help_heading = "Generated SWF Options")]
    frame_rate: f32,

    /// Whether the SWF uses the network sandbox.
    /// If set, the SWF is allowed to make network requests but cannot access local files.
    /// If not set, the SWF can access local files but cannot make network requests.
    #[arg(long, help_heading = "Generated SWF Options")]
    use_network_sandbox: bool,
}

fn main() -> Result<()> {
    let mut opt = Opt::parse();

    if opt.classpath.is_empty() {
        opt.classpath.push(PathBuf::from("."));
    }
    let provider = FileSystemSourceProvider::with_roots(opt.classpath);
    let mut builder = ProgramBuilder::new(provider);

    for src in &opt.pcode {
        builder.add_pcode(&src.to_string_lossy());
    }
    for src in &opt.script {
        builder.add_script(&src.to_string_lossy());
    }
    for class in &opt.class {
        builder.add_class(class);
    }

    let parsed = builder.build().unwrap_or_else(|e| panic!("{}", e));
    let pcode = parsed.compile(CompileOptions::default().with_swf_version(opt.swf_version));
    let output_path = opt
        .output
        .or_else(|| opt.script.first().map(|s| s.with_extension("swf")))
        .unwrap_or_else(|| PathBuf::from("output.swf"));
    let swf = pcode.to_swf(
        &SwfOptions::default()
            .with_frame_rate(opt.frame_rate)
            .with_network_sandbox(opt.use_network_sandbox),
    )?;
    fs::write(&output_path, swf)?;
    Ok(())
}

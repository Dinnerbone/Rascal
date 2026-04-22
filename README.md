# Rascal

[![License](https://img.shields.io/badge/license-MIT%2FApache-blue.svg)](https://github.com/Dinnerbone/rascal#license)

Rascal is an open source ActionScript 2 compiler, with aspirations for further ActionScript and SWF editing capabilities
in the future.

It was primarily developed for use with [Ruffle](https://ruffle.rs), and allows the authoring and editing of
ActionScript 1 and 2 content without the need to hunt down a copy of Flash from eBay.

## Command Line Usage

A basic CLI is provided for compiling ActionScript files to SWF files.

### Installation

- Clone the repository
- Run `cargo install --path crates/cli --release`

### Usage

For full CLI usage, run `rascal --help`.

There are two kinds of inputs, and they are intermixable.

- `rascal foo.as` - Compiles a script (not a class, loose code) into a SWF file.
    - Multiple script files can be specified, and they will execute in the order they are specified.
- `rascal -c some.ClassName` - Compiles a class named `some.ClassName` (expected to be available at `some/ClassName.as`)
  into a SWF file.
    - The classpath defaults to working directory, but can be specified via the `--classpath` flag.
    - Multiple class names can be specified, and they will be loaded in the order they are specified.
    - If one (and only one) class has a `static function main()`, it will be executed as the entry point to the swf.

Various options about the output are available:

- `-o foo.swf`/`--output foo.swf` - The output SWF file
    - This defaults to either the first script file + `.swf`, else `output.swf`
- `-v 10`/`--swf-version 10` - The version of SWF file to produce
    - This defaults to 15,
      which [corresponds to Flash Player 11.2](https://github.com/ruffle-rs/ruffle/wiki/SWF-version-chart)
- `--frame-rate 30` - The frame rate of the SWF file
    - This defaults to 24

## Library

TODO :D

## License

Rascal is licensed MIT or Apache-2.0, at your option.

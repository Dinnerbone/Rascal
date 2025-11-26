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

For now, it's a simple `rascal filename.as` or `rascal filename.pcode`, and it will output `filename.swf`.

This definitely needs some love with more options - including multiple input files, SWF version, optimisation levels,
etc.

## Library

TODO :D

## License

Rascal is licensed MIT or Apache-2.0, at your option.

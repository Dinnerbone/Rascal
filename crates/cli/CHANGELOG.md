# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.2.0](https://github.com/Dinnerbone/Rascal/compare/rascal_cli-v0.1.2...rascal_cli-v0.2.0) - 2026-04-22

### Other

- Update version to 0.2.0
- Move all internal crates to just modules
- Add a bunch of crate metadata
- Restructure everything to have a public crate 'rascal', which internally uses other crates
- Separate CLI help menu into different categories
- Add `--classpath` cli argument
- Add `--class` cli argument
- Allow multiple scripts to be specified on cli
- Add `--output` cli argument
- Add --frame-rate option, defaults to 24fps
- Add -v (or --swf-version) to cli
- Add swf_version to hir_to_pcode(program, swf_version)
- Codegen can spit out interfaces properly!
- Codegen now technically supports multiple modules
- Remove error generation from codegen, it's now infallible
- Introduce Error struct from as2 lib
- Introduce ProgramBuilder, for compiling multiple files
- Rename project from ruasc to Rascal
- Codegen can now emit errors
- Move codegen to own crate
- Allow compiling pcode files from cli
- Compile pcode to swfs
- Add pcode to_string impl
- Add pcode parser
- Restructure to a workspace with cli and as2 crates

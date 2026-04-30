# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.2.3](https://github.com/Dinnerbone/Rascal/compare/rascal-v0.2.2...rascal-v0.2.3) - 2026-04-30

### Other

- Support network_sandbox (FileAttributes swf tag) through API and CLI

## [0.2.2](https://github.com/Dinnerbone/Rascal/compare/rascal-v0.2.1...rascal-v0.2.2) - 2026-04-29

### Other

- Bump annotate-snippets from 0.12.12 to 0.12.15
- Add SwfOptions::with_stage_size
- Move swf_version and frame_rate into structs with builder style API, for less breakage
- Add ProgramBuilder::add_pcode(path) and --pcode CLI

## [0.2.0](https://github.com/Dinnerbone/Rascal/compare/rascal-v0.1.2...rascal-v0.2.0) - 2026-04-22

### Other

- Move all internal crates to just modules
- Add a bunch of crate metadata
- Restructure everything to have a public crate 'rascal', which internally uses other crates

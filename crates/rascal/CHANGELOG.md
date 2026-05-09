# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.2.7](https://github.com/Dinnerbone/Rascal/compare/rascal-v0.2.6...rascal-v0.2.7) - 2026-05-09

### Added

- *(codegen)* Fold constants to ints with int() special function
- *(codegen)* Fold constants to numbers with - binary operator
- *(codegen)* Fold constants to numbers or strings with + binary operator
- *(codegen)* Fold constants to numbers with ~ unary operator
- *(codegen)* Fold constants to numbers with + unary operator
- *(codegen)* Fold constants to numbers with - unary operator
- *(codegen)* Fold constants to bools with ! unary operator
- *(codegen)* Fold "a" + "b" into "ab"
- *(api)* Add OptimizationOptions to control which optimization passes get applied

### Fixed

- *(parser)* Parse +foo as a unary add, as it'll later convert things to float

### Other

- *(codegen)* Slightly refactor constant folding to make it easier to implement more
- *(codegen)* Rename Optimizer to RegisterPromoter
- *(codegen)* Rename simplifier.as to constant_folding.as
- *(codegen)* Rename Simplifier to ConstantFolder

## [0.2.6](https://github.com/Dinnerbone/Rascal/compare/rascal-v0.2.5...rascal-v0.2.6) - 2026-05-09

### Other

- Use u32::Max as double for negating things, to match Flash
- Flip ternary expressions; this matches Flash, previously it matched mtasc

## [0.2.5](https://github.com/Dinnerbone/Rascal/compare/rascal-v0.2.4...rascal-v0.2.5) - 2026-05-09

### Other

- Fix `x instanceof Foo && otherExpr`
- Store variables and parameters in registers; implements DefineFunction2 properly
- Resolve ast Vec<Declaration> to single declarations
- Added function level scoping
- Change hir::Script(Vec<StatementKind>) to {statements: ...}
- Introduce MutVisitor, switch simplifying to use it
- Remove hir::ConstantKind::This
- Support reading and writing of DefineFunction2

## [0.2.4](https://github.com/Dinnerbone/Rascal/compare/rascal-v0.2.3...rascal-v0.2.4) - 2026-05-02

### Other

- Fix codegen for try-catch (jump to end after try statements)
- Update try_catch test with (broken) try{/*no throw*/}catch(){} examples

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

# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.3.5](https://github.com/Dinnerbone/Rascal/compare/rascal-v0.3.4...rascal-v0.3.5) - 2026-06-10

### Fixed

- *(codegen)* Stop adding to constants if the constants tag will become full.

### Other

- *(deps)* bump log from 0.4.28 to 0.4.31
- release v0.3.3

## [0.3.4](https://github.com/Dinnerbone/Rascal/compare/rascal-v0.3.2...rascal-v0.3.4) - 2026-05-17

### Added

- Support #include "foo.as"
- *(lexer)* Lex # tokens (will be used for preprocessor, e.g. #include)

### Fixed

- *(api)* Make sure our error type is Sync for compatibility with anyhow; use Arcs instead of Rc

### Other

- release v0.3.2
- Made error handling a little cleaner and work by span's `file_id` rather than a fixed file name
- *(ast)* Box ast::SwitchElement::Statement as it's three times bigger than Case
- Add file id to Span

## [0.3.3](https://github.com/Dinnerbone/Rascal/compare/rascal-v0.3.2...rascal-v0.3.3) - 2026-05-16

### Added

- Support #include "foo.as"
- *(lexer)* Lex # tokens (will be used for preprocessor, e.g. #include)

### Other

- Made error handling a little cleaner and work by span's `file_id` rather than a fixed file name
- *(ast)* Box ast::SwitchElement::Statement as it's three times bigger than Case
- Add file id to Span

## [0.3.2](https://github.com/Dinnerbone/Rascal/compare/rascal-v0.3.1...rascal-v0.3.2) - 2026-05-13

### Fixed

- *(codegen)* guard against CharacterId overflow
- *(codegen)* guard against overflow in action and block lengths
- *(codegen)* guard against NUL bytes in strings

### Other

- `cargo fmt` was missed

## [0.3.1](https://github.com/Dinnerbone/Rascal/compare/rascal-v0.3.0...rascal-v0.3.1) - 2026-05-13

### Fixed

- *(api)* Fix not respecting optimization options during build

## [0.3.0](https://github.com/Dinnerbone/Rascal/compare/rascal-v0.2.7...rascal-v0.3.0) - 2026-05-12

### Added

- *(api)* [**breaking**] Move optimizations into compile options
- *(api)* [**breaking**] Support having compile options on the builder directly, as that's where most of the actual "compiling" happens

### Fixed

- *(codegen)* Don't use Extends opcode below swf 7
- *(codegen)* Replace virtual properties with `__set__X`/`__get__X` where appropriate
- *(codegen)* Support casting objects using `ClassName(obj)`
- *(resolving)* Fix looking up paths relative to the root (e.g. `Foo` as opposed to `foo.Bar`)
- *(codegen)* Allow optimization passes on class functions
- *(codegen)* Allow register promotion for `for (var foo : obj)`
- *(pcode)* Fix parsing `Push NaN` and `Push Infinity`

### Other

- *(codegen)* Track type names of variables
- *(api)* [**breaking**] Make Program::compile() consume self
- *(tests)* Add more tests for virtual properties
- Bump annotate-snippets from 0.12.15 to 0.12.16
- *(codegen)* Extract out convenient methods for scope finding
- *(codegen)* Split up for enumerator into an enum so that we can have registers too

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

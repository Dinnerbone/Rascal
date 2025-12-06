use crate::error::{Error, ErrorSet};
use crate::lexer::Lexer;
use crate::resolver::resolve_hir;
use crate::{hir, parser};
use serde::Serialize;
use std::path::PathBuf;

pub trait SourceProvider {
    fn load(&self, path: &str) -> Result<String, std::io::Error>;
}

pub struct FileSystemSourceProvider {
    root: PathBuf,
}

impl FileSystemSourceProvider {
    pub fn with_root(root: PathBuf) -> Self {
        Self { root }
    }
}

impl SourceProvider for FileSystemSourceProvider {
    fn load(&self, path: &str) -> Result<String, std::io::Error> {
        std::fs::read_to_string(self.root.join(path))
    }
}

#[derive(Debug, Serialize)]
pub struct Program {
    pub initial_script: Vec<hir::StatementKind>,
}

pub struct ProgramBuilder<P> {
    provider: P,
    scripts: Vec<String>,
}

impl<P> ProgramBuilder<P> {
    pub fn add_script(&mut self, path: &str) {
        self.scripts.push(path.to_owned());
    }
}

impl<P: SourceProvider> ProgramBuilder<P> {
    pub fn new(provider: P) -> Self {
        Self {
            provider,
            scripts: vec![],
        }
    }

    pub fn build(self) -> Result<Program, Error> {
        let mut initial_script = vec![];
        let mut errors = ErrorSet::new();

        for path in self.scripts {
            let source = match self.provider.load(&path) {
                Ok(source) => source,
                Err(e) => {
                    errors.add_io_error(&path, e);
                    continue;
                }
            };
            let tokens = Lexer::new(&source).into_vec();
            let ast = match parser::parse_document(&tokens) {
                Ok(ast) => ast,
                Err(e) => {
                    errors.add_parsing_error(&path, &source, e.into());
                    continue;
                }
            };
            let (mut hir, hir_errors) = resolve_hir(ast);
            for error in hir_errors {
                errors.add_parsing_error(&path, &source, error);
            }
            initial_script.append(&mut hir.statements);
        }

        errors.error_unless_empty()?;

        Ok(Program { initial_script })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_all_samples() {
        insta::glob!("../../../samples/as2", "**/*.as", |path| {
            let mut builder = ProgramBuilder::new(FileSystemSourceProvider::with_root(
                path.parent().unwrap().to_owned(),
            ));
            builder.add_script(path.file_name().unwrap().to_str().unwrap());
            let parsed = builder.build();
            insta::assert_yaml_snapshot!(parsed);
        });
    }

    #[test]
    fn test_fail_samples() {
        insta::glob!("../../../samples/as2_errors", "**/*.as", |path| {
            let mut builder = ProgramBuilder::new(FileSystemSourceProvider::with_root(
                path.parent().unwrap().to_owned(),
            ));
            builder.add_script(path.file_name().unwrap().to_str().unwrap());
            let parsed = builder.build().unwrap_err();
            insta::assert_snapshot!(parsed);
        });
    }
}

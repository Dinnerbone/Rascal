use crate::lexer::Lexer;
use crate::parser::ActionScriptError;
use crate::resolver::resolve_hir;
use crate::{hir, parser};
use serde::Serialize;
use std::io::Error;
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
    fn load(&self, path: &str) -> Result<String, Error> {
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

    pub fn build(self) -> Result<Program, String> {
        let mut initial_script = vec![];

        for path in self.scripts {
            let source = self.provider.load(&path).map_err(|e| e.to_string())?;
            let tokens = Lexer::new(&source).into_vec();
            let ast = parser::parse_document(&tokens)
                .map_err(|e| ActionScriptError::from_parse(&path, &source, e).to_string())?;
            let mut hir = resolve_hir(ast);
            initial_script.append(&mut hir.statements);
        }

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
}

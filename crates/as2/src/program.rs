use crate::error::{Error, ErrorSet};
use crate::hir::Document;
use crate::lexer::Lexer;
use crate::resolver::resolve_hir;
use crate::{hir, parser};
use indexmap::IndexSet;
use serde::Serialize;
use std::path::PathBuf;

pub trait SourceProvider {
    fn load(&self, path: &str) -> Result<String, std::io::Error>;

    fn is_file(&self, path: &str) -> bool;
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

    fn is_file(&self, path: &str) -> bool {
        self.root.join(path).is_file()
    }
}

#[derive(Debug, Serialize)]
pub struct Program {
    pub initial_script: Vec<hir::StatementKind>,
    pub interfaces: Vec<hir::Interface>,
    pub classes: Vec<hir::Class>,
}

pub struct ProgramBuilder<P> {
    provider: P,
    scripts: Vec<String>,
    classes: Vec<String>,
}

impl<P> ProgramBuilder<P> {
    pub fn add_script(&mut self, path: &str) {
        self.scripts.push(path.to_owned());
    }

    pub fn add_class(&mut self, path: &str) {
        self.classes.push(path.to_owned());
    }
}

impl<P: SourceProvider> ProgramBuilder<P> {
    pub fn new(provider: P) -> Self {
        Self {
            provider,
            scripts: vec![],
            classes: vec![],
        }
    }

    pub fn build(self) -> Result<Program, Error> {
        let mut initial_script = vec![];
        let mut interfaces = vec![];
        let mut classes = vec![];
        let mut errors = ErrorSet::new();
        let mut loaded_classes = IndexSet::new();
        let mut pending_classes = self.classes;

        fn load_file<P: SourceProvider>(
            provider: &P,
            errors: &mut ErrorSet,
            loaded_classes: &mut IndexSet<String>,
            pending_classes: &mut Vec<String>,
            path: &str,
            is_script: bool,
        ) -> Option<Document> {
            let source = match provider.load(path) {
                Ok(source) => source,
                Err(e) => {
                    errors.add_io_error(path, e);
                    return None;
                }
            };
            let tokens = Lexer::new(&source).into_vec();
            let ast = match parser::parse_document(&tokens) {
                Ok(ast) => ast,
                Err(e) => {
                    errors.add_parsing_error(path, &source, e.into());
                    return None;
                }
            };
            let expected_name = path
                .split('/')
                .next_back()
                .unwrap()
                .split('.')
                .next()
                .unwrap();
            let (mut hir, hir_errors, dependencies) =
                resolve_hir(provider, ast, is_script, expected_name);
            for error in hir_errors {
                errors.add_parsing_error(path, &source, error);
            }
            for name in dependencies {
                if loaded_classes.insert(name.to_owned()) {
                    pending_classes.push(name + ".as"); // TODO do better :D
                }
            }
            while hir.simplify() {
                // Keep going until nothing changed
            }
            Some(hir)
        }

        for path in self.scripts {
            if let Some(document) = load_file(
                &self.provider,
                &mut errors,
                &mut loaded_classes,
                &mut pending_classes,
                &path,
                true,
            ) && let Document::Script(statements) = document
            {
                initial_script.extend(statements);
            }
        }

        while let Some(path) = pending_classes.pop() {
            if let Some(document) = load_file(
                &self.provider,
                &mut errors,
                &mut loaded_classes,
                &mut pending_classes,
                &path,
                false,
            ) {
                match document {
                    Document::Interface(interface) => {
                        interfaces.push(interface);
                    }
                    Document::Class(class) => {
                        classes.push(*class);
                    }
                    _ => {}
                }
            }
        }

        errors.error_unless_empty()?;

        Ok(Program {
            initial_script,
            interfaces,
            classes,
        })
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
        insta::glob!("../../../samples/as2_classes", "*.as", |path| {
            let mut builder = ProgramBuilder::new(FileSystemSourceProvider::with_root(
                path.parent().unwrap().to_owned(),
            ));
            builder.add_class(path.file_name().unwrap().to_str().unwrap());
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

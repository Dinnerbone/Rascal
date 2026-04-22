use crate::error::{Error, ErrorSet};
use crate::hir::{ConstantKind, Document, Expr, ExprKind, StatementKind};
use crate::lexer::Lexer;
use crate::resolver::resolve_hir;
use crate::{hir, parser, type_path_to_file_path};
use indexmap::IndexSet;
use rascal_common::span::Span;
use serde::Serialize;
use std::path::PathBuf;

pub trait SourceProvider {
    fn load(&self, path: &str) -> Result<String, std::io::Error>;

    fn is_file(&self, path: &str) -> bool;
}

pub struct FileSystemSourceProvider {
    roots: Vec<PathBuf>,
}

impl FileSystemSourceProvider {
    pub fn with_root(root: PathBuf) -> Self {
        Self::with_roots(vec![root])
    }

    pub fn with_roots(roots: Vec<PathBuf>) -> Self {
        Self { roots }
    }
}

impl SourceProvider for FileSystemSourceProvider {
    fn load(&self, path: &str) -> Result<String, std::io::Error> {
        for root in &self.roots {
            let actual_path = root.join(path);
            if actual_path.is_file() {
                return std::fs::read_to_string(actual_path);
            }
        }
        Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "File not found",
        ))
    }

    fn is_file(&self, path: &str) -> bool {
        for root in &self.roots {
            let actual_path = root.join(path);
            if actual_path.is_file() {
                return true;
            }
        }
        false
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
            type_name: &str,
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
            let (mut hir, hir_errors, dependencies) =
                resolve_hir(provider, ast, is_script, type_name);
            for error in hir_errors {
                errors.add_parsing_error(path, &source, error);
            }
            for name in dependencies {
                if loaded_classes.insert(name.to_owned()) {
                    pending_classes.push(name);
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
                "",
                true,
            ) && let Document::Script(statements) = document
            {
                initial_script.extend(statements);
            }
        }

        let mut entry_point_class = vec![];

        while let Some(type_name) = pending_classes.pop() {
            let filename = type_path_to_file_path(&type_name);
            if let Some(document) = load_file(
                &self.provider,
                &mut errors,
                &mut loaded_classes,
                &mut pending_classes,
                &filename,
                &type_name,
                false,
            ) {
                match document {
                    Document::Interface(interface) => {
                        interfaces.push(interface);
                    }
                    Document::Class(class) => {
                        if let Some(main) = class.functions.get("main")
                            && main.is_static
                        {
                            entry_point_class.push(class.name.clone());
                        }
                        classes.push(*class);
                    }
                    _ => {}
                }
            }
        }

        match entry_point_class.len() {
            0 => {
                if initial_script.is_empty() {
                    errors.add_misc_error("No entry point found (either 'static function main()' inside a class, or the initial file must be a script)".to_owned());
                }
            }
            1 => initial_script.push(call_main_method(entry_point_class.first().unwrap())),
            _ => errors.add_misc_error(format!(
                "Conflicting entry points found on classes: {}",
                entry_point_class.join(", ")
            )),
        };

        errors.error_unless_empty()?;

        Ok(Program {
            initial_script,
            interfaces,
            classes,
        })
    }
}

fn call_main_method(class: &str) -> StatementKind {
    let mut path = class.split(".").collect::<Vec<&str>>();
    path.push("main");
    let path: Vec<String> = path.into_iter().map(|s| s.to_owned()).collect();

    let mut name = None;
    for part in path.into_iter() {
        if let Some(prev) = name.take() {
            name = Some(Expr::new(
                Span::default(),
                ExprKind::Field(
                    Box::new(prev),
                    Box::new(Expr::new(
                        Span::default(),
                        ExprKind::Constant(ConstantKind::String(part)),
                    )),
                ),
            ));
        } else {
            name = Some(Expr::new(
                Span::default(),
                ExprKind::Constant(ConstantKind::Identifier(part)),
            ));
        }
    }
    // Name has to be something, as we always added 'main' to the path
    let name = name.unwrap();

    StatementKind::Expr(Expr::new(
        Span::default(),
        ExprKind::Call {
            name: Box::new(name),
            args: vec![Expr::new(
                Span::default(),
                ExprKind::Constant(ConstantKind::This),
            )],
        },
    ))
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
            builder.add_class(
                path.file_name()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .strip_suffix(".as")
                    .unwrap(),
            );
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

    #[test]
    fn test_main_method_simple() {
        assert_eq!(
            call_main_method("foo"),
            StatementKind::Expr(Expr::new(
                Span::default(),
                ExprKind::Call {
                    name: Box::new(Expr::new(
                        Span::default(),
                        ExprKind::Field(
                            Box::new(Expr::new(
                                Span::default(),
                                ExprKind::Constant(ConstantKind::Identifier("foo".to_owned()))
                            )),
                            Box::new(Expr::new(
                                Span::default(),
                                ExprKind::Constant(ConstantKind::String("main".to_owned()))
                            ))
                        )
                    )),
                    args: vec![Expr::new(
                        Span::default(),
                        ExprKind::Constant(ConstantKind::This)
                    )]
                }
            ))
        );
    }

    #[test]
    fn test_main_method_path() {
        assert_eq!(
            call_main_method("foo.bar.baz"),
            StatementKind::Expr(Expr::new(
                Span::default(),
                ExprKind::Call {
                    name: Box::new(Expr::new(
                        Span::default(),
                        ExprKind::Field(
                            Box::new(Expr::new(
                                Span::default(),
                                ExprKind::Field(
                                    Box::new(Expr::new(
                                        Span::default(),
                                        ExprKind::Field(
                                            Box::new(Expr::new(
                                                Span::default(),
                                                ExprKind::Constant(ConstantKind::Identifier(
                                                    "foo".to_owned()
                                                ))
                                            )),
                                            Box::new(Expr::new(
                                                Span::default(),
                                                ExprKind::Constant(ConstantKind::String(
                                                    "bar".to_owned()
                                                ))
                                            ))
                                        )
                                    )),
                                    Box::new(Expr::new(
                                        Span::default(),
                                        ExprKind::Constant(ConstantKind::String("baz".to_owned()))
                                    )),
                                )
                            )),
                            Box::new(Expr::new(
                                Span::default(),
                                ExprKind::Constant(ConstantKind::String("main".to_owned()))
                            ))
                        )
                    )),
                    args: vec![Expr::new(
                        Span::default(),
                        ExprKind::Constant(ConstantKind::This)
                    )]
                }
            ))
        );
    }
}

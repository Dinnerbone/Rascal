use crate::error::{Error, ErrorSet};
use crate::internal::as2::lexer::Lexer;
use crate::internal::as2::resolver::resolve_hir;
use crate::internal::as2::{hir, parser, type_path_to_file_path};
use crate::internal::as2_codegen::{class_to_actions, interface_to_actions, script_to_actions};
use crate::internal::as2_pcode::Actions;
use crate::internal::span::Span;
use crate::provider::SourceProvider;
use indexmap::IndexSet;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct SwfOptions {
    pub(crate) frame_rate: f32,
    pub(crate) stage_x_min: f64,
    pub(crate) stage_y_min: f64,
    pub(crate) stage_x_max: f64,
    pub(crate) stage_y_max: f64,
    pub(crate) use_network_sandbox: bool,
}

impl Default for SwfOptions {
    fn default() -> Self {
        Self {
            frame_rate: 24.0,
            stage_x_min: 0.0,
            stage_y_min: 0.0,
            stage_x_max: 100.0,
            stage_y_max: 100.0,
            use_network_sandbox: false,
        }
    }
}

impl SwfOptions {
    pub fn with_frame_rate(mut self, frame_rate: f32) -> Self {
        self.frame_rate = frame_rate;
        self
    }

    pub fn with_stage_size(
        mut self,
        stage_x_min: f64,
        stage_y_min: f64,
        stage_x_max: f64,
        stage_y_max: f64,
    ) -> Self {
        self.stage_x_min = stage_x_min;
        self.stage_y_min = stage_y_min;
        self.stage_x_max = stage_x_max;
        self.stage_y_max = stage_y_max;
        self
    }

    pub fn with_network_sandbox(mut self, use_network_sandbox: bool) -> Self {
        self.use_network_sandbox = use_network_sandbox;
        self
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct CompileOptions {
    pub(crate) swf_version: u8,
}

impl Default for CompileOptions {
    fn default() -> Self {
        Self { swf_version: 15 }
    }
}

impl CompileOptions {
    pub fn with_swf_version(mut self, swf_version: u8) -> Self {
        self.swf_version = swf_version;
        self
    }
}

#[derive(Debug, Serialize)]
pub struct Program {
    pub(crate) initial_script: Vec<hir::StatementKind>,
    pub(crate) interfaces: Vec<hir::Interface>,
    pub(crate) classes: Vec<hir::Class>,
    pub(crate) custom_pcodes: Vec<Actions>,
}

impl Program {
    pub fn compile(&self, compile_options: CompileOptions) -> CompiledProgram {
        let initializer = if self.initial_script.is_empty() {
            None
        } else {
            Some(script_to_actions(&self.initial_script))
        };
        let mut extra_modules = vec![];
        for interface in self.interfaces.iter().rev() {
            let actions = interface_to_actions(interface);
            extra_modules.push((interface.name.to_owned(), actions));
        }
        for class in self.classes.iter().rev() {
            let actions = class_to_actions(class);
            extra_modules.push((class.name.to_owned(), actions));
        }
        CompiledProgram {
            initializer,
            extra_modules,
            compile_options,
            custom_pcodes: self.custom_pcodes.clone(),
        }
    }
}

#[derive(Serialize)]
pub struct CompiledProgram {
    pub(crate) initializer: Option<Actions>,
    pub(crate) extra_modules: Vec<(String, Actions)>,
    pub(crate) compile_options: CompileOptions,
    pub(crate) custom_pcodes: Vec<Actions>,
}

impl CompiledProgram {
    pub fn to_swf(&self, swf_options: &SwfOptions) -> swf::error::Result<Vec<u8>> {
        crate::swf::pcode_to_swf(self, swf_options)
    }
}

pub struct ProgramBuilder<P> {
    provider: P,
    scripts: Vec<String>,
    pcodes: Vec<String>,
    classes: Vec<String>,
}

impl<P> ProgramBuilder<P> {
    pub fn add_script(&mut self, path: &str) {
        self.scripts.push(path.to_owned());
    }

    pub fn add_pcode(&mut self, path: &str) {
        self.pcodes.push(path.to_owned());
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
            pcodes: vec![],
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
        let mut custom_pcodes = vec![];

        fn load_actionscript<P: SourceProvider>(
            provider: &P,
            errors: &mut ErrorSet,
            loaded_classes: &mut IndexSet<String>,
            pending_classes: &mut Vec<String>,
            path: &str,
            type_name: &str,
            is_script: bool,
        ) -> Option<hir::Document> {
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

        fn load_pcode<P: SourceProvider>(
            provider: &P,
            errors: &mut ErrorSet,
            path: &str,
            custom_pcodes: &mut Vec<Actions>,
        ) {
            let source = match provider.load(path) {
                Ok(source) => source,
                Err(e) => {
                    errors.add_io_error(path, e);
                    return;
                }
            };
            let tokens = crate::internal::as2_pcode::Lexer::new(&source).into_vec();
            match crate::internal::as2_pcode::parse_actions(&tokens) {
                Ok(actions) => custom_pcodes.push(actions),
                Err(e) => errors.add_parsing_error(path, &source, e.into()),
            }
        }

        for path in self.pcodes {
            load_pcode(&self.provider, &mut errors, &path, &mut custom_pcodes);
        }

        for path in self.scripts {
            if let Some(document) = load_actionscript(
                &self.provider,
                &mut errors,
                &mut loaded_classes,
                &mut pending_classes,
                &path,
                "",
                true,
            ) && let hir::Document::Script(statements) = document
            {
                initial_script.extend(statements);
            }
        }

        let mut entry_point_class = vec![];

        while let Some(type_name) = pending_classes.pop() {
            let filename = type_path_to_file_path(&type_name);
            if let Some(document) = load_actionscript(
                &self.provider,
                &mut errors,
                &mut loaded_classes,
                &mut pending_classes,
                &filename,
                &type_name,
                false,
            ) {
                match document {
                    hir::Document::Interface(interface) => {
                        interfaces.push(interface);
                    }
                    hir::Document::Class(class) => {
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
            0 => {} // It's fine to have no entry point... even if it _may_ not entirely make sense :D
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
            custom_pcodes,
        })
    }
}

fn call_main_method(class: &str) -> hir::StatementKind {
    let mut path = class.split(".").collect::<Vec<&str>>();
    path.push("main");
    let path: Vec<String> = path.into_iter().map(|s| s.to_owned()).collect();

    let mut name = None;
    for part in path.into_iter() {
        if let Some(prev) = name.take() {
            name = Some(hir::Expr::new(
                Span::default(),
                hir::ExprKind::Field(
                    Box::new(prev),
                    Box::new(hir::Expr::new(
                        Span::default(),
                        hir::ExprKind::Constant(hir::ConstantKind::String(part)),
                    )),
                ),
            ));
        } else {
            name = Some(hir::Expr::new(
                Span::default(),
                hir::ExprKind::Constant(hir::ConstantKind::Identifier(part)),
            ));
        }
    }
    // Name has to be something, as we always added 'main' to the path
    let name = name.unwrap();

    hir::StatementKind::Expr(hir::Expr::new(
        Span::default(),
        hir::ExprKind::Call {
            name: Box::new(name),
            args: vec![hir::Expr::new(
                Span::default(),
                hir::ExprKind::Constant(hir::ConstantKind::This),
            )],
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::provider::FileSystemSourceProvider;

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
            hir::StatementKind::Expr(hir::Expr::new(
                Span::default(),
                hir::ExprKind::Call {
                    name: Box::new(hir::Expr::new(
                        Span::default(),
                        hir::ExprKind::Field(
                            Box::new(hir::Expr::new(
                                Span::default(),
                                hir::ExprKind::Constant(hir::ConstantKind::Identifier(
                                    "foo".to_owned()
                                ))
                            )),
                            Box::new(hir::Expr::new(
                                Span::default(),
                                hir::ExprKind::Constant(hir::ConstantKind::String(
                                    "main".to_owned()
                                ))
                            ))
                        )
                    )),
                    args: vec![hir::Expr::new(
                        Span::default(),
                        hir::ExprKind::Constant(hir::ConstantKind::This)
                    )]
                }
            ))
        );
    }

    #[test]
    fn test_main_method_path() {
        assert_eq!(
            call_main_method("foo.bar.baz"),
            hir::StatementKind::Expr(hir::Expr::new(
                Span::default(),
                hir::ExprKind::Call {
                    name: Box::new(hir::Expr::new(
                        Span::default(),
                        hir::ExprKind::Field(
                            Box::new(hir::Expr::new(
                                Span::default(),
                                hir::ExprKind::Field(
                                    Box::new(hir::Expr::new(
                                        Span::default(),
                                        hir::ExprKind::Field(
                                            Box::new(hir::Expr::new(
                                                Span::default(),
                                                hir::ExprKind::Constant(
                                                    hir::ConstantKind::Identifier("foo".to_owned())
                                                )
                                            )),
                                            Box::new(hir::Expr::new(
                                                Span::default(),
                                                hir::ExprKind::Constant(hir::ConstantKind::String(
                                                    "bar".to_owned()
                                                ))
                                            ))
                                        )
                                    )),
                                    Box::new(hir::Expr::new(
                                        Span::default(),
                                        hir::ExprKind::Constant(hir::ConstantKind::String(
                                            "baz".to_owned()
                                        ))
                                    )),
                                )
                            )),
                            Box::new(hir::Expr::new(
                                Span::default(),
                                hir::ExprKind::Constant(hir::ConstantKind::String(
                                    "main".to_owned()
                                ))
                            ))
                        )
                    )),
                    args: vec![hir::Expr::new(
                        Span::default(),
                        hir::ExprKind::Constant(hir::ConstantKind::This)
                    )]
                }
            ))
        );
    }
}

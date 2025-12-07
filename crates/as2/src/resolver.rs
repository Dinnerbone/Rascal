mod special_functions;
mod special_properties;

use crate::ast;
use crate::error::ParsingError;
use crate::global_types::GLOBAL_TYPES;
use crate::hir;
use crate::program::SourceProvider;
use crate::resolver::special_functions::resolve_special_call;
use indexmap::IndexSet;
use rascal_common::span::{Span, Spanned};
use std::collections::HashMap;

struct ModuleContext<'a> {
    imports: HashMap<String, Vec<String>>,
    errors: Vec<ParsingError>,
    dependencies: IndexSet<String>,
    provider: &'a dyn SourceProvider,
    is_script: bool,
}

impl<'a> ModuleContext<'a> {
    fn new(provider: &'a dyn SourceProvider, is_script: bool) -> Self {
        Self {
            imports: HashMap::new(),
            errors: vec![],
            dependencies: IndexSet::new(),
            provider,
            is_script,
        }
    }

    fn import(&mut self, path: Vec<String>, name: String) {
        self.imports.insert(name, path);
    }

    fn error(&mut self, message: impl ToString, span: Span) {
        self.errors
            .push(ParsingError::new(message.to_string(), span));
    }

    fn expand_identifier(&self, name: String, span: Span) -> Option<hir::Expr> {
        if let Some(path) = self.imports.get(&name)
            && !path.is_empty()
        {
            let mut parent = Box::new(hir::Expr::new(
                span,
                hir::ExprKind::Constant(hir::ConstantKind::Identifier(
                    path.first().unwrap().to_owned(),
                )),
            ));
            let wrap_parent = |parent: Box<hir::Expr>, child: String| {
                hir::Expr::new(
                    span,
                    hir::ExprKind::Field(
                        parent,
                        Box::new(hir::Expr::new(
                            span,
                            hir::ExprKind::Constant(hir::ConstantKind::String(child)),
                        )),
                    ),
                )
            };
            for segment in path.iter().skip(1) {
                parent = Box::new(wrap_parent(parent, segment.to_owned()));
            }
            return Some(wrap_parent(parent, name));
        }
        None
    }

    fn add_dependency(&mut self, span: Span, name: &str, required: bool) {
        if GLOBAL_TYPES.contains(&name) {
            return;
        }
        if !self.provider.is_file(&(name.replace(".", "/") + ".as")) {
            if required {
                self.error(
                    format!("The class or interface `{}` could not be loaded.", name),
                    span,
                );
            }
            return;
        }
        self.dependencies.insert(name.to_owned());
    }
}

pub fn resolve_hir<P: SourceProvider>(
    provider: &P,
    ast: ast::Document,
    is_script: bool,
    expected_name: &str,
) -> (hir::Document, Vec<ParsingError>, IndexSet<String>) {
    let mut context = ModuleContext::new(provider, is_script);
    let document = if is_script {
        hir::Document::Script(resolve_statement_vec(&mut context, &ast.statements))
    } else {
        resolve_class_or_interface(&mut context, &ast.statements, expected_name)
    };
    (document, context.errors, context.dependencies)
}

fn resolve_statement_vec(
    context: &mut ModuleContext,
    input: &[ast::Statement],
) -> Vec<hir::StatementKind> {
    input
        .iter()
        .map(|statement| resolve_statement(context, statement))
        .collect()
}

fn resolve_class_or_interface(
    context: &mut ModuleContext,
    input: &[ast::Statement],
    expected_name: &str,
) -> hir::Document {
    let mut result = None;
    for statement in input {
        match &statement.value {
            ast::StatementKind::Interface {
                name,
                extends,
                body,
            } => {
                if name.value == expected_name {
                    if result.is_some() {
                        context.error(
                            "Only one class or interface can be defined per ActionScript 2.0 .as file.",
                            statement.span,
                        );
                        continue;
                    }
                    if let Some(extends) = extends {
                        context.add_dependency(extends.span, extends.value, true);
                    }
                    result = Some(hir::Document::Interface(resolve_interface(
                        context,
                        name.value.to_owned(),
                        extends.map(|e| e.value.to_owned()),
                        body,
                    )));
                } else {
                    context.error(
                        format!("The class '{0}' needs to be defined in a file whose relative path is '{0}.as'.", name.value), name.span
                    );
                }
            }
            _ => context.error(
                "ActionScript 2.0 class scripts may only define class or interface constructs.",
                statement.span,
            ),
        }
    }

    result.unwrap_or(hir::Document::Invalid)
}

fn resolve_interface(
    context: &mut ModuleContext,
    name: String,
    extends: Option<String>,
    body: &[ast::Statement],
) -> hir::Interface {
    hir::Interface {
        name,
        extends,
        body: resolve_statement_vec(context, body),
    }
}

fn resolve_statement_box(
    context: &mut ModuleContext,
    input: &ast::Statement,
) -> Box<hir::StatementKind> {
    Box::new(resolve_statement(context, input))
}

fn resolve_statement(context: &mut ModuleContext, input: &ast::Statement) -> hir::StatementKind {
    match &input.value {
        ast::StatementKind::Declare(declarations) => hir::StatementKind::Declare(
            declarations
                .iter()
                .map(|d| hir::Declaration {
                    name: d.name.to_owned(),
                    type_name: resolve_opt_type_name(context, &d.type_name),
                    value: d
                        .value
                        .value
                        .as_ref()
                        .map(|expr| resolve_expr(context, expr)),
                })
                .collect(),
        ),
        ast::StatementKind::Return(values) => {
            hir::StatementKind::Return(resolve_expr_vec(context, values))
        }
        ast::StatementKind::Throw(values) => {
            hir::StatementKind::Throw(resolve_expr_vec(context, values))
        }
        ast::StatementKind::Expr(expr) => hir::StatementKind::Expr(resolve_expr(context, expr)),
        ast::StatementKind::Block(statements) => {
            hir::StatementKind::Block(resolve_statement_vec(context, statements))
        }
        ast::StatementKind::ForIn { condition, body } => hir::StatementKind::ForIn {
            condition: resolve_for_condition(context, condition),
            body: resolve_statement_box(context, body),
        },
        ast::StatementKind::While { condition, body } => hir::StatementKind::While {
            condition: resolve_expr(context, condition),
            body: resolve_statement_box(context, body),
        },
        ast::StatementKind::If { condition, yes, no } => hir::StatementKind::If {
            condition: resolve_expr(context, condition),
            yes: resolve_statement_box(context, yes),
            no: no
                .as_ref()
                .map(|statement| resolve_statement_box(context, statement)),
        },
        ast::StatementKind::Break => hir::StatementKind::Break,
        ast::StatementKind::Continue => hir::StatementKind::Continue,
        ast::StatementKind::Try(try_catch) => hir::StatementKind::Try(hir::TryCatch {
            try_body: resolve_statement_vec(context, &try_catch.try_body),
            catch_all: try_catch
                .catch_all
                .as_ref()
                .map(|catch| resolve_catch(context, catch)),
            typed_catches: try_catch
                .typed_catches
                .iter()
                .map(|(type_name, catch)| {
                    (
                        resolve_type_name(context, type_name),
                        resolve_catch(context, catch),
                    )
                })
                .collect(),
            finally: resolve_statement_vec(context, &try_catch.finally),
        }),
        ast::StatementKind::WaitForFrame {
            frame,
            scene,
            if_loaded,
        } => hir::StatementKind::WaitForFrame {
            frame: resolve_expr(context, frame),
            scene: scene.as_ref().map(|expr| resolve_expr(context, expr)),
            if_loaded: resolve_statement_box(context, if_loaded),
        },
        ast::StatementKind::TellTarget { target, body } => hir::StatementKind::TellTarget {
            target: resolve_expr(context, target),
            body: resolve_statement_box(context, body),
        },
        ast::StatementKind::InlinePCode(pcode) => {
            hir::StatementKind::InlinePCode((*pcode).to_owned())
        }
        ast::StatementKind::With { target, body } => hir::StatementKind::With {
            target: resolve_expr(context, target),
            body: resolve_statement_box(context, body),
        },
        ast::StatementKind::Switch { target, elements } => hir::StatementKind::Switch {
            target: resolve_expr(context, target),
            elements: elements
                .iter()
                .map(|element| resolve_switch_element(context, element))
                .collect(),
        },
        ast::StatementKind::Import { path, name } => {
            context.import(
                path.iter().map(|s| (*s).to_owned()).collect(),
                (*name).to_owned(),
            );
            hir::StatementKind::Block(vec![]) // todo, resolving statements shouldn't be a 1:1, we should be able to do nothing here
        }
        ast::StatementKind::Interface { .. } => {
            if context.is_script {
                context.error(
                    "Classes may only be defined in external ActionScript 2.0 class scripts.",
                    input.span,
                );
                hir::StatementKind::Block(vec![]) // todo, resolving statements shouldn't be a 1:1, we should be able to do nothing here
            } else {
                context.error(
                    "Class and interface definitions cannot be nested.",
                    input.span,
                );
                hir::StatementKind::Block(vec![]) // todo, resolving statements shouldn't be a 1:1, we should be able to do nothing here
            }
        }
    }
}

fn resolve_for_condition(
    context: &mut ModuleContext,
    input: &ast::ForCondition,
) -> hir::ForCondition {
    match input {
        ast::ForCondition::Enumerate {
            variable,
            declare,
            object,
        } => hir::ForCondition::Enumerate {
            variable: (*variable).to_owned(),
            declare: *declare,
            object: resolve_expr(context, object),
        },
        ast::ForCondition::Classic {
            initialize,
            condition,
            update,
        } => hir::ForCondition::Classic {
            initialize: initialize
                .as_ref()
                .map(|statement| resolve_statement_box(context, statement)),
            condition: resolve_expr_vec(context, condition),
            update: resolve_expr_vec(context, update),
        },
    }
}

fn resolve_catch(context: &mut ModuleContext, input: &ast::Catch) -> hir::Catch {
    hir::Catch {
        name: Spanned::new(input.name.span, input.name.value.to_owned()),
        body: resolve_statement_vec(context, &input.body),
    }
}

fn resolve_switch_element(
    context: &mut ModuleContext,
    input: &ast::SwitchElement,
) -> hir::SwitchElement {
    match input {
        ast::SwitchElement::Case(expr) => hir::SwitchElement::Case(resolve_expr(context, expr)),
        ast::SwitchElement::Default => hir::SwitchElement::Default,
        ast::SwitchElement::Statement(statement) => {
            hir::SwitchElement::Statement(resolve_statement(context, statement))
        }
    }
}

fn resolve_expr_box(context: &mut ModuleContext, input: &ast::Expr) -> Box<hir::Expr> {
    Box::new(resolve_expr(context, input))
}

fn resolve_expr_vec(context: &mut ModuleContext, input: &[ast::Expr]) -> Vec<hir::Expr> {
    input
        .iter()
        .map(|expr| resolve_expr(context, expr))
        .collect()
}

fn resolve_constant(constant: &ast::ConstantKind) -> hir::ConstantKind {
    match constant {
        ast::ConstantKind::String(v) => hir::ConstantKind::String(v.to_string()),
        ast::ConstantKind::Identifier(v) => match *v {
            "true" => hir::ConstantKind::Boolean(true),
            "false" => hir::ConstantKind::Boolean(false),
            other => hir::ConstantKind::Identifier(other.to_owned()),
        },
        ast::ConstantKind::Float(v) => hir::ConstantKind::Float(*v),
        ast::ConstantKind::Integer(v) => hir::ConstantKind::Integer(*v),
    }
}

fn resolve_expr(context: &mut ModuleContext, input: &ast::Expr) -> hir::Expr {
    let span = input.span;
    let result = match &input.value {
        ast::ExprKind::Constant(ast::ConstantKind::Identifier(identifier)) => {
            if let Some(path) = context.expand_identifier((*identifier).to_owned(), input.span) {
                return path;
            } else {
                hir::ExprKind::Constant(resolve_constant(&ast::ConstantKind::Identifier(
                    identifier,
                )))
            }
        }
        ast::ExprKind::Constant(value) => hir::ExprKind::Constant(resolve_constant(value)),
        ast::ExprKind::Call { name, args } => resolve_call(context, span, name, args),
        ast::ExprKind::New { name, args } => hir::ExprKind::New {
            name: resolve_expr_box(context, name),
            args: resolve_expr_vec(context, args),
        },
        ast::ExprKind::BinaryOperator(op, left, right) => hir::ExprKind::BinaryOperator(
            *op,
            resolve_expr_box(context, left),
            resolve_expr_box(context, right),
        ),
        ast::ExprKind::UnaryOperator(op, value) => {
            hir::ExprKind::UnaryOperator(*op, resolve_expr_box(context, value))
        }
        ast::ExprKind::Parenthesis(expr) => return resolve_expr(context, expr),
        ast::ExprKind::Ternary { condition, yes, no } => hir::ExprKind::Ternary {
            condition: resolve_expr_box(context, condition),
            yes: resolve_expr_box(context, yes),
            no: resolve_expr_box(context, no),
        },
        ast::ExprKind::InitObject(values) => hir::ExprKind::InitObject(
            values
                .iter()
                .map(|(name, value)| ((*name).to_owned(), resolve_expr(context, value)))
                .collect(),
        ),
        ast::ExprKind::InitArray(values) => hir::ExprKind::InitArray(
            values
                .iter()
                .map(|expr| resolve_expr(context, expr))
                .collect(),
        ),
        ast::ExprKind::Field(object, field) => {
            // This looks weird and it will match on parts of the same expr multiple times (e.g. `(a.b).(c)` + `(a).(b)` both get checked)
            // But Flash works this way too. `foo.bar.baz.enabled = true` will try to load `foo.as`, `foo/bar.as`, `foo/bar/baz.as` and even `foo/bar/baz/enabled.as`
            // There's simply no way to know if such a reference is for a class on disk, without just assuming it'll work at runtime and possibly load something extra if we find it.
            // (Actually - there's one caveat here; Flash won't check if it knows there's a variable by the same name/prefix in scope, but at the time of writing this, Rascal doesn't have scope resolution)
            if let Some(path) = try_field_to_path(&object.value, &field.value) {
                context.add_dependency(span, &path, false);
            }

            hir::ExprKind::Field(
                resolve_expr_box(context, object),
                resolve_expr_box(context, field),
            )
        }
        ast::ExprKind::TypeOf(values) => hir::ExprKind::TypeOf(
            values
                .iter()
                .map(|expr| resolve_expr(context, expr))
                .collect(),
        ),
        ast::ExprKind::Delete(values) => hir::ExprKind::Delete(
            values
                .iter()
                .map(|expr| resolve_expr(context, expr))
                .collect(),
        ),
        ast::ExprKind::Void(values) => hir::ExprKind::Void(
            values
                .iter()
                .map(|expr| resolve_expr(context, expr))
                .collect(),
        ),
        ast::ExprKind::Function(function) => {
            hir::ExprKind::Function(resolve_function(context, function))
        }
        ast::ExprKind::GetVariable(name) => {
            hir::ExprKind::GetVariable(resolve_expr_box(context, name))
        }
        ast::ExprKind::SetVariable(name, value) => hir::ExprKind::SetVariable(
            resolve_expr_box(context, name),
            resolve_expr_box(context, value),
        ),
    };
    hir::Expr::new(input.span, result)
}

fn try_field_to_path(obj: &ast::ExprKind, key: &ast::ExprKind) -> Option<String> {
    let mut path = vec![];

    if let ast::ExprKind::Constant(ast::ConstantKind::String(identifier)) = key {
        path.push(identifier.as_ref());
    } else {
        return None;
    }

    let mut parent = obj;
    loop {
        match parent {
            ast::ExprKind::Constant(ast::ConstantKind::Identifier(identifier)) => {
                path.push(identifier);
                break;
            }
            ast::ExprKind::Field(obj, key) => {
                parent = obj;
                if let ast::ExprKind::Constant(ast::ConstantKind::String(key)) = &key.value {
                    path.push(key);
                } else {
                    return None;
                }
            }
            _ => return None,
        }
    }

    Some(
        path.iter()
            .rev()
            .map(|s| (*s).to_owned())
            .collect::<Vec<_>>()
            .join("."),
    )
}

fn resolve_call(
    context: &mut ModuleContext,
    span: Span,
    name: &ast::Expr,
    args: &[ast::Expr],
) -> hir::ExprKind {
    if let ast::Expr {
        value: ast::ExprKind::Constant(ast::ConstantKind::Identifier(name)),
        ..
    } = *name
        && let Some(special) = resolve_special_call(context, span, name, args)
    {
        return special;
    }
    hir::ExprKind::Call {
        name: resolve_expr_box(context, name),
        args: resolve_expr_vec(context, args),
    }
}

fn resolve_function(context: &mut ModuleContext, input: &ast::Function) -> hir::Function {
    hir::Function {
        name: input.name.map(|name| name.to_owned()),
        args: input
            .args
            .iter()
            .map(|arg| hir::FunctionArgument {
                name: arg.name.to_owned(),
                type_name: resolve_opt_type_name(context, &arg.type_name),
            })
            .collect(),
        body: resolve_statement_vec(context, &input.body),
    }
}

fn resolve_type_name(context: &mut ModuleContext, type_name: &Spanned<&str>) -> Spanned<String> {
    context.add_dependency(type_name.span, type_name.value, true);
    Spanned::new(type_name.span, type_name.value.to_owned())
}

fn resolve_opt_type_name(
    context: &mut ModuleContext,
    type_name: &Option<Spanned<&str>>,
) -> Option<Spanned<String>> {
    type_name
        .as_ref()
        .map(|type_name| resolve_type_name(context, type_name))
}

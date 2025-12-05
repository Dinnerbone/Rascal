mod special_functions;

use crate::ast;
use crate::error::ParsingError;
use crate::hir;
use crate::resolver::special_functions::resolve_special_call;
use rascal_common::span::{Span, Spanned};
use std::collections::HashMap;

struct ModuleContext {
    imports: HashMap<String, Vec<String>>,
    errors: Vec<ParsingError>,
}

impl ModuleContext {
    fn new() -> Self {
        Self {
            imports: HashMap::new(),
            errors: vec![],
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
}

pub fn resolve_hir(ast: ast::Document) -> (hir::Document, Vec<ParsingError>) {
    let mut context = ModuleContext::new();
    let statements = resolve_statement_vec(&mut context, ast.statements);
    (hir::Document { statements }, context.errors)
}

fn resolve_statement_vec(
    context: &mut ModuleContext,
    input: Vec<ast::StatementKind>,
) -> Vec<hir::StatementKind> {
    input
        .into_iter()
        .map(|statement| resolve_statement(context, statement))
        .collect()
}

#[expect(clippy::boxed_local)]
fn resolve_statement_box(
    context: &mut ModuleContext,
    input: Box<ast::StatementKind>,
) -> Box<hir::StatementKind> {
    Box::new(resolve_statement(context, *input))
}

fn resolve_statement(context: &mut ModuleContext, input: ast::StatementKind) -> hir::StatementKind {
    match input {
        ast::StatementKind::Declare(declarations) => hir::StatementKind::Declare(
            declarations
                .into_iter()
                .map(|d| hir::Declaration {
                    name: d.name.to_owned(),
                    type_name: d
                        .type_name
                        .map(|s| Spanned::new(s.span, s.value.to_owned())),
                    value: d.value.map(|expr| resolve_expr(context, expr)),
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
            no: no.map(|statement| resolve_statement_box(context, statement)),
        },
        ast::StatementKind::Break => hir::StatementKind::Break,
        ast::StatementKind::Continue => hir::StatementKind::Continue,
        ast::StatementKind::Try(try_catch) => hir::StatementKind::Try(hir::TryCatch {
            try_body: resolve_statement_vec(context, try_catch.try_body),
            catch_all: try_catch
                .catch_all
                .map(|catch| resolve_catch(context, catch)),
            typed_catches: try_catch
                .typed_catches
                .into_iter()
                .map(|(type_name, catch)| {
                    (
                        Spanned::new(type_name.span, type_name.value.to_owned()),
                        resolve_catch(context, catch),
                    )
                })
                .collect(),
            finally: resolve_statement_vec(context, try_catch.finally),
        }),
        ast::StatementKind::WaitForFrame {
            frame,
            scene,
            if_loaded,
        } => hir::StatementKind::WaitForFrame {
            frame: resolve_expr(context, frame),
            scene: scene.map(|expr| resolve_expr(context, expr)),
            if_loaded: resolve_statement_box(context, if_loaded),
        },
        ast::StatementKind::TellTarget { target, body } => hir::StatementKind::TellTarget {
            target: resolve_expr(context, target),
            body: resolve_statement_box(context, body),
        },
        ast::StatementKind::InlinePCode(pcode) => hir::StatementKind::InlinePCode(pcode.to_owned()),
        ast::StatementKind::With { target, body } => hir::StatementKind::With {
            target: resolve_expr(context, target),
            body: resolve_statement_box(context, body),
        },
        ast::StatementKind::Switch { target, elements } => hir::StatementKind::Switch {
            target: resolve_expr(context, target),
            elements: elements
                .into_iter()
                .map(|element| resolve_switch_element(context, element))
                .collect(),
        },
        ast::StatementKind::Import { path, name } => {
            context.import(
                path.iter().map(|s| (*s).to_owned()).collect(),
                name.to_owned(),
            );
            hir::StatementKind::Block(vec![]) // todo, resolving statements shouldn't be a 1:1, we should be able to do nothing here
        }
    }
}

fn resolve_for_condition(
    context: &mut ModuleContext,
    input: ast::ForCondition,
) -> hir::ForCondition {
    match input {
        ast::ForCondition::Enumerate {
            variable,
            declare,
            object,
        } => hir::ForCondition::Enumerate {
            variable: variable.to_owned(),
            declare,
            object: resolve_expr(context, object),
        },
        ast::ForCondition::Classic {
            initialize,
            condition,
            update,
        } => hir::ForCondition::Classic {
            initialize: initialize.map(|statement| resolve_statement_box(context, statement)),
            condition: resolve_expr_vec(context, condition),
            update: resolve_expr_vec(context, update),
        },
    }
}

fn resolve_catch(context: &mut ModuleContext, input: ast::Catch) -> hir::Catch {
    hir::Catch {
        name: Spanned::new(input.name.span, input.name.value.to_owned()),
        body: resolve_statement_vec(context, input.body),
    }
}

fn resolve_switch_element(
    context: &mut ModuleContext,
    input: ast::SwitchElement,
) -> hir::SwitchElement {
    match input {
        ast::SwitchElement::Case(expr) => hir::SwitchElement::Case(resolve_expr(context, expr)),
        ast::SwitchElement::Default => hir::SwitchElement::Default,
        ast::SwitchElement::Statement(statement) => {
            hir::SwitchElement::Statement(resolve_statement(context, statement))
        }
    }
}

#[expect(clippy::boxed_local)]
fn resolve_expr_box(context: &mut ModuleContext, input: Box<ast::Expr>) -> Box<hir::Expr> {
    Box::new(resolve_expr(context, *input))
}

fn resolve_expr_vec(context: &mut ModuleContext, input: Vec<ast::Expr>) -> Vec<hir::Expr> {
    input
        .into_iter()
        .map(|expr| resolve_expr(context, expr))
        .collect()
}

fn resolve_constant(constant: ast::ConstantKind) -> hir::ConstantKind {
    match constant {
        ast::ConstantKind::String(v) => hir::ConstantKind::String(v.to_string()),
        ast::ConstantKind::Identifier(v) => hir::ConstantKind::Identifier(v.to_owned()),
        ast::ConstantKind::Float(v) => hir::ConstantKind::Float(v),
        ast::ConstantKind::Integer(v) => hir::ConstantKind::Integer(v),
    }
}

fn resolve_expr(context: &mut ModuleContext, input: ast::Expr) -> hir::Expr {
    let span = input.span;
    let result = match input.value {
        ast::ExprKind::Constant(ast::ConstantKind::Identifier(identifier)) => {
            if let Some(path) = context.expand_identifier(identifier.to_owned(), input.span) {
                return path;
            } else {
                hir::ExprKind::Constant(hir::ConstantKind::Identifier(identifier.to_owned()))
            }
        }
        ast::ExprKind::Constant(value) => hir::ExprKind::Constant(resolve_constant(value)),
        ast::ExprKind::Call { name, args } => resolve_call(context, span, name, args),
        ast::ExprKind::New { name, args } => hir::ExprKind::New {
            name: resolve_expr_box(context, name),
            args: resolve_expr_vec(context, args),
        },
        ast::ExprKind::BinaryOperator(op, left, right) => hir::ExprKind::BinaryOperator(
            op,
            resolve_expr_box(context, left),
            resolve_expr_box(context, right),
        ),
        ast::ExprKind::UnaryOperator(op, value) => {
            hir::ExprKind::UnaryOperator(op, resolve_expr_box(context, value))
        }
        ast::ExprKind::Parenthesis(expr) => return resolve_expr(context, *expr),
        ast::ExprKind::Ternary { condition, yes, no } => hir::ExprKind::Ternary {
            condition: resolve_expr_box(context, condition),
            yes: resolve_expr_box(context, yes),
            no: resolve_expr_box(context, no),
        },
        ast::ExprKind::InitObject(values) => hir::ExprKind::InitObject(
            values
                .into_iter()
                .map(|(name, value)| (name.to_owned(), resolve_expr(context, value)))
                .collect(),
        ),
        ast::ExprKind::InitArray(values) => hir::ExprKind::InitArray(
            values
                .into_iter()
                .map(|expr| resolve_expr(context, expr))
                .collect(),
        ),
        ast::ExprKind::Field(object, field) => hir::ExprKind::Field(
            resolve_expr_box(context, object),
            resolve_expr_box(context, field),
        ),
        ast::ExprKind::TypeOf(values) => hir::ExprKind::TypeOf(
            values
                .into_iter()
                .map(|expr| resolve_expr(context, expr))
                .collect(),
        ),
        ast::ExprKind::Delete(values) => hir::ExprKind::Delete(
            values
                .into_iter()
                .map(|expr| resolve_expr(context, expr))
                .collect(),
        ),
        ast::ExprKind::Void(values) => hir::ExprKind::Void(
            values
                .into_iter()
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

fn resolve_call(
    context: &mut ModuleContext,
    span: Span,
    name: Box<ast::Expr>,
    args: Vec<ast::Expr>,
) -> hir::ExprKind {
    if let ast::Expr {
        value: ast::ExprKind::Constant(ast::ConstantKind::Identifier(name)),
        ..
    } = *name
        && let Some(special) = resolve_special_call(context, span, name, &args)
    {
        return special;
    }
    hir::ExprKind::Call {
        name: resolve_expr_box(context, name),
        args: resolve_expr_vec(context, args),
    }
}

fn resolve_function(context: &mut ModuleContext, input: ast::Function) -> hir::Function {
    hir::Function {
        name: input.name.map(|name| name.to_owned()),
        args: input
            .args
            .into_iter()
            .map(|arg| hir::FunctionArgument {
                name: arg.name.to_owned(),
                type_name: arg
                    .type_name
                    .map(|s| Spanned::new(s.span, s.value.to_owned())),
            })
            .collect(),
        body: resolve_statement_vec(context, input.body),
    }
}

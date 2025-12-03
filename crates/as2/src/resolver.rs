use crate::ast;
use crate::hir;
use std::marker::PhantomData;

struct ModuleContext<'src>(PhantomData<&'src ()>);

impl<'src> ModuleContext<'src> {
    fn new() -> Self {
        Self(PhantomData)
    }
}

pub fn resolve_hir(ast: ast::Document) -> hir::Document {
    let mut context = ModuleContext::new();
    hir::Document {
        statements: resolve_statement_vec(&mut context, ast.statements),
    }
}

fn resolve_statement_vec<'src>(
    context: &mut ModuleContext<'src>,
    input: Vec<ast::StatementKind<'src>>,
) -> Vec<hir::StatementKind<'src>> {
    input
        .into_iter()
        .map(|statement| resolve_statement(context, statement))
        .collect()
}

#[expect(clippy::boxed_local)]
fn resolve_statement_box<'src>(
    context: &mut ModuleContext<'src>,
    input: Box<ast::StatementKind<'src>>,
) -> Box<hir::StatementKind<'src>> {
    Box::new(resolve_statement(context, *input))
}

fn resolve_statement<'src>(
    context: &mut ModuleContext<'src>,
    input: ast::StatementKind<'src>,
) -> hir::StatementKind<'src> {
    match input {
        ast::StatementKind::Declare(declarations) => hir::StatementKind::Declare(
            declarations
                .into_iter()
                .map(|d| hir::Declaration {
                    name: d.name,
                    type_name: d.type_name,
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
                .map(|(type_name, catch)| (type_name, resolve_catch(context, catch)))
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
        ast::StatementKind::InlinePCode(pcode) => hir::StatementKind::InlinePCode(pcode),
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
    }
}

fn resolve_for_condition<'src>(
    context: &mut ModuleContext<'src>,
    input: ast::ForCondition<'src>,
) -> hir::ForCondition<'src> {
    match input {
        ast::ForCondition::Enumerate {
            variable,
            declare,
            object,
        } => hir::ForCondition::Enumerate {
            variable,
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

fn resolve_catch<'src>(
    context: &mut ModuleContext<'src>,
    input: ast::Catch<'src>,
) -> hir::Catch<'src> {
    hir::Catch {
        name: input.name,
        body: resolve_statement_vec(context, input.body),
    }
}

fn resolve_switch_element<'src>(
    context: &mut ModuleContext<'src>,
    input: ast::SwitchElement<'src>,
) -> hir::SwitchElement<'src> {
    match input {
        ast::SwitchElement::Case(expr) => hir::SwitchElement::Case(resolve_expr(context, expr)),
        ast::SwitchElement::Default => hir::SwitchElement::Default,
        ast::SwitchElement::Statement(statement) => {
            hir::SwitchElement::Statement(resolve_statement(context, statement))
        }
    }
}

#[expect(clippy::boxed_local)]
fn resolve_expr_box<'src>(
    context: &mut ModuleContext<'src>,
    input: Box<ast::Expr<'src>>,
) -> Box<hir::Expr<'src>> {
    Box::new(resolve_expr(context, *input))
}

fn resolve_expr_vec<'src>(
    context: &mut ModuleContext<'src>,
    input: Vec<ast::Expr<'src>>,
) -> Vec<hir::Expr<'src>> {
    input
        .into_iter()
        .map(|expr| resolve_expr(context, expr))
        .collect()
}

fn resolve_expr<'src>(
    context: &mut ModuleContext<'src>,
    input: ast::Expr<'src>,
) -> hir::Expr<'src> {
    let result = match input.value {
        ast::ExprKind::Constant(value) => hir::ExprKind::Constant(value),
        ast::ExprKind::Call { name, args } => hir::ExprKind::Call {
            name: resolve_expr_box(context, name),
            args: resolve_expr_vec(context, args),
        },
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
                .map(|(name, value)| (name, resolve_expr(context, value)))
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

fn resolve_function<'src>(
    context: &mut ModuleContext<'src>,
    input: ast::Function<'src>,
) -> hir::Function<'src> {
    hir::Function {
        name: input.name,
        args: input
            .args
            .into_iter()
            .map(|arg| hir::FunctionArgument {
                name: arg.name,
                type_name: arg.type_name,
            })
            .collect(),
        body: resolve_statement_vec(context, input.body),
    }
}

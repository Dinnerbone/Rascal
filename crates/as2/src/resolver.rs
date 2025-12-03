use crate::ast;
use crate::hir;

pub fn resolve_hir(ast: ast::Document) -> hir::Document {
    hir::Document {
        statements: resolve_statement_vec(ast.statements),
    }
}

fn resolve_statement_vec(input: Vec<ast::StatementKind>) -> Vec<hir::StatementKind> {
    input.into_iter().map(resolve_statement).collect()
}

#[expect(clippy::boxed_local)]
fn resolve_statement_box(input: Box<ast::StatementKind>) -> Box<hir::StatementKind> {
    Box::new(resolve_statement(*input))
}

fn resolve_statement(input: ast::StatementKind) -> hir::StatementKind {
    match input {
        ast::StatementKind::Declare(declarations) => hir::StatementKind::Declare(
            declarations
                .into_iter()
                .map(|d| hir::Declaration {
                    name: d.name,
                    type_name: d.type_name,
                    value: d.value.map(resolve_expr),
                })
                .collect(),
        ),
        ast::StatementKind::Return(values) => hir::StatementKind::Return(resolve_expr_vec(values)),
        ast::StatementKind::Throw(values) => hir::StatementKind::Throw(resolve_expr_vec(values)),
        ast::StatementKind::Expr(expr) => hir::StatementKind::Expr(resolve_expr(expr)),
        ast::StatementKind::Block(statements) => {
            hir::StatementKind::Block(resolve_statement_vec(statements))
        }
        ast::StatementKind::ForIn { condition, body } => hir::StatementKind::ForIn {
            condition: resolve_for_condition(condition),
            body: resolve_statement_box(body),
        },
        ast::StatementKind::While { condition, body } => hir::StatementKind::While {
            condition: resolve_expr(condition),
            body: resolve_statement_box(body),
        },
        ast::StatementKind::If { condition, yes, no } => hir::StatementKind::If {
            condition: resolve_expr(condition),
            yes: resolve_statement_box(yes),
            no: no.map(resolve_statement_box),
        },
        ast::StatementKind::Break => hir::StatementKind::Break,
        ast::StatementKind::Continue => hir::StatementKind::Continue,
        ast::StatementKind::Try(try_catch) => hir::StatementKind::Try(hir::TryCatch {
            try_body: resolve_statement_vec(try_catch.try_body),
            catch_all: try_catch.catch_all.map(resolve_catch),
            typed_catches: try_catch
                .typed_catches
                .into_iter()
                .map(|(type_name, catch)| (type_name, resolve_catch(catch)))
                .collect(),
            finally: resolve_statement_vec(try_catch.finally),
        }),
        ast::StatementKind::WaitForFrame {
            frame,
            scene,
            if_loaded,
        } => hir::StatementKind::WaitForFrame {
            frame: resolve_expr(frame),
            scene: scene.map(resolve_expr),
            if_loaded: resolve_statement_box(if_loaded),
        },
        ast::StatementKind::TellTarget { target, body } => hir::StatementKind::TellTarget {
            target: resolve_expr(target),
            body: resolve_statement_box(body),
        },
        ast::StatementKind::InlinePCode(pcode) => hir::StatementKind::InlinePCode(pcode),
        ast::StatementKind::With { target, body } => hir::StatementKind::With {
            target: resolve_expr(target),
            body: resolve_statement_box(body),
        },
        ast::StatementKind::Switch { target, elements } => hir::StatementKind::Switch {
            target: resolve_expr(target),
            elements: elements.into_iter().map(resolve_switch_element).collect(),
        },
    }
}

fn resolve_for_condition(input: ast::ForCondition) -> hir::ForCondition {
    match input {
        ast::ForCondition::Enumerate {
            variable,
            declare,
            object,
        } => hir::ForCondition::Enumerate {
            variable,
            declare,
            object: resolve_expr(object),
        },
        ast::ForCondition::Classic {
            initialize,
            condition,
            update,
        } => hir::ForCondition::Classic {
            initialize: initialize.map(resolve_statement_box),
            condition: resolve_expr_vec(condition),
            update: resolve_expr_vec(update),
        },
    }
}

fn resolve_catch(input: ast::Catch) -> hir::Catch {
    hir::Catch {
        name: input.name,
        body: resolve_statement_vec(input.body),
    }
}

fn resolve_switch_element(input: ast::SwitchElement) -> hir::SwitchElement {
    match input {
        ast::SwitchElement::Case(expr) => hir::SwitchElement::Case(resolve_expr(expr)),
        ast::SwitchElement::Default => hir::SwitchElement::Default,
        ast::SwitchElement::Statement(statement) => {
            hir::SwitchElement::Statement(resolve_statement(statement))
        }
    }
}

#[expect(clippy::boxed_local)]
fn resolve_expr_box(input: Box<ast::Expr>) -> Box<hir::Expr> {
    Box::new(resolve_expr(*input))
}

fn resolve_expr_vec(input: Vec<ast::Expr>) -> Vec<hir::Expr> {
    input.into_iter().map(resolve_expr).collect()
}

fn resolve_expr(input: ast::Expr) -> hir::Expr {
    let result = match input.value {
        ast::ExprKind::Constant(value) => hir::ExprKind::Constant(value),
        ast::ExprKind::Call { name, args } => hir::ExprKind::Call {
            name: resolve_expr_box(name),
            args: resolve_expr_vec(args),
        },
        ast::ExprKind::New { name, args } => hir::ExprKind::New {
            name: resolve_expr_box(name),
            args: resolve_expr_vec(args),
        },
        ast::ExprKind::BinaryOperator(op, left, right) => {
            hir::ExprKind::BinaryOperator(op, resolve_expr_box(left), resolve_expr_box(right))
        }
        ast::ExprKind::UnaryOperator(op, value) => {
            hir::ExprKind::UnaryOperator(op, resolve_expr_box(value))
        }
        ast::ExprKind::Parenthesis(expr) => return resolve_expr(*expr),
        ast::ExprKind::Ternary { condition, yes, no } => hir::ExprKind::Ternary {
            condition: resolve_expr_box(condition),
            yes: resolve_expr_box(yes),
            no: resolve_expr_box(no),
        },
        ast::ExprKind::InitObject(values) => hir::ExprKind::InitObject(
            values
                .into_iter()
                .map(|(name, value)| (name, resolve_expr(value)))
                .collect(),
        ),
        ast::ExprKind::InitArray(values) => {
            hir::ExprKind::InitArray(values.into_iter().map(resolve_expr).collect())
        }
        ast::ExprKind::Field(object, field) => {
            hir::ExprKind::Field(resolve_expr_box(object), resolve_expr_box(field))
        }
        ast::ExprKind::TypeOf(values) => {
            hir::ExprKind::TypeOf(values.into_iter().map(resolve_expr).collect())
        }
        ast::ExprKind::Delete(values) => {
            hir::ExprKind::Delete(values.into_iter().map(resolve_expr).collect())
        }
        ast::ExprKind::Void(values) => {
            hir::ExprKind::Void(values.into_iter().map(resolve_expr).collect())
        }
        ast::ExprKind::Function(function) => hir::ExprKind::Function(resolve_function(function)),
        ast::ExprKind::GetVariable(name) => hir::ExprKind::GetVariable(resolve_expr_box(name)),
        ast::ExprKind::SetVariable(name, value) => {
            hir::ExprKind::SetVariable(resolve_expr_box(name), resolve_expr_box(value))
        }
    };
    hir::Expr::new(input.span, result)
}

fn resolve_function(input: ast::Function) -> hir::Function {
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
        body: resolve_statement_vec(input.body),
    }
}

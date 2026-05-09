use crate::internal::as2::ast::{BinaryOperator, UnaryOperator};
use crate::internal::as2::hir::visitor::{MutVisitor, walk_expr};
use crate::internal::as2::hir::{ConstantKind, Document, Expr, ExprKind};
use std::borrow::Cow;

struct ConstantFolder {
    anything_changed: bool,
}

impl MutVisitor for ConstantFolder {
    fn visit_expr(&mut self, expr: &mut Expr) {
        walk_expr(self, expr);
        match &mut expr.value {
            ExprKind::BinaryOperator(op, left, right) => {
                if let ExprKind::Constant(left) = &left.value
                    && let ExprKind::Constant(right) = &right.value
                    && let Some(result) = evaluate_binary_operator(*op, left, right)
                {
                    expr.value = ExprKind::Constant(result);
                    self.anything_changed = true;
                }
            }
            ExprKind::UnaryOperator(op, inner) => {
                if let ExprKind::Constant(value) = &inner.value
                    && let Some(result) = evaluate_unary_operator(*op, value)
                {
                    expr.value = ExprKind::Constant(result);
                    self.anything_changed = true;
                }
            }
            _ => {}
        }
    }
}

fn as_string(value: &'_ ConstantKind) -> Option<Cow<'_, str>> {
    match value {
        ConstantKind::String(value) => Some(Cow::Borrowed(value)),
        ConstantKind::Boolean(true) => Some(Cow::Borrowed("true")),
        ConstantKind::Boolean(false) => Some(Cow::Borrowed("false")),
        ConstantKind::Integer(value) => Some(Cow::Owned(value.to_string())),
        ConstantKind::Float(value) => Some(Cow::Owned(value.to_string())),
        _ => None,
    }
}

fn evaluate_binary_operator(
    op: BinaryOperator,
    left: &ConstantKind,
    right: &ConstantKind,
) -> Option<ConstantKind> {
    Some(match op {
        BinaryOperator::StringAdd | BinaryOperator::Add => {
            if let Some(left) = as_string(left)
                && let Some(right) = as_string(right)
            {
                ConstantKind::String(format!("{}{}", left, right))
            } else {
                return None;
            }
        }
        _ => return None,
    })
}

fn evaluate_unary_operator(op: UnaryOperator, value: &ConstantKind) -> Option<ConstantKind> {
    Some(match (op, value) {
        (UnaryOperator::LogicalNot, ConstantKind::Boolean(value)) => ConstantKind::Boolean(!value),
        _ => return None,
    })
}

pub fn fold_constants(document: &mut Document) -> bool {
    let mut folder = ConstantFolder {
        anything_changed: false,
    };

    match document {
        Document::Script { statements, .. } => {
            for statement in statements {
                folder.visit_statement(statement);
            }
        }
        Document::Interface(_interface) => {}
        Document::Class(_class) => {}
        Document::Invalid => {}
    }

    folder.anything_changed
}

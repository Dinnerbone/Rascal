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
            ExprKind::CastToInteger(value) => {
                if let ExprKind::Constant(value) = &value.value
                    && let Some(value) = as_int(value, i32::MIN) {
                        expr.value = ExprKind::Constant(ConstantKind::Integer(value));
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

fn as_bool(value: &'_ ConstantKind) -> Option<bool> {
    match value {
        ConstantKind::String(_) => {
            let value = as_float(value).unwrap_or_default();
            Some(value != 0.0 && !value.is_nan())
        }
        ConstantKind::Boolean(value) => Some(*value),
        ConstantKind::Integer(value) => Some(*value != 0),
        ConstantKind::Float(value) => Some(*value != 0.0),
        _ => None,
    }
}

fn as_float(value: &'_ ConstantKind) -> Option<f64> {
    match value {
        ConstantKind::String(value) => Some(value.parse().unwrap_or(f64::NAN)),
        ConstantKind::Boolean(true) => Some(1.0),
        ConstantKind::Boolean(false) => Some(0.0),
        ConstantKind::Integer(value) => Some(*value as f64),
        ConstantKind::Float(value) => Some(*value),
        _ => None,
    }
}

fn as_int(value: &'_ ConstantKind, nan: i32) -> Option<i32> {
    match value {
        ConstantKind::String(_) => {
            let value = as_float(value).unwrap_or_default();
            Some(if value.is_nan() { nan } else { value as i32 })
        }
        ConstantKind::Boolean(true) => Some(1),
        ConstantKind::Boolean(false) => Some(0),
        ConstantKind::Integer(value) => Some(*value),
        ConstantKind::Float(value) => Some(*value as i32),
        _ => None,
    }
}

fn float_as_constant(value: f64) -> ConstantKind {
    if value.is_finite()
        && value.fract() == 0.0
        && value >= i32::MIN as f64
        && value <= i32::MAX as f64
    {
        ConstantKind::Integer(value as i32)
    } else {
        ConstantKind::Float(value)
    }
}

fn evaluate_binary_operator(
    op: BinaryOperator,
    left: &ConstantKind,
    right: &ConstantKind,
) -> Option<ConstantKind> {
    Some(match op {
        BinaryOperator::StringAdd => {
            if let (Some(left), Some(right)) = (as_string(left), as_string(right)) {
                ConstantKind::String(format!("{}{}", left, right))
            } else {
                return None;
            }
        }
        BinaryOperator::Add => {
            if matches!(left, ConstantKind::String(_)) || matches!(right, ConstantKind::String(_)) {
                if let (Some(left), Some(right)) = (as_string(left), as_string(right)) {
                    ConstantKind::String(format!("{}{}", left, right))
                } else {
                    return None;
                }
            } else if let (Some(left), Some(right)) = (as_float(left), as_float(right)) {
                float_as_constant(left + right)
            } else {
                return None;
            }
        }
        BinaryOperator::Sub => {
            if let (Some(left), Some(right)) = (as_float(left), as_float(right)) {
                float_as_constant(left - right)
            } else {
                return None;
            }
        }
        _ => return None,
    })
}

fn evaluate_unary_operator(op: UnaryOperator, value: &ConstantKind) -> Option<ConstantKind> {
    Some(match op {
        UnaryOperator::LogicalNot => {
            if let Some(value) = as_bool(value) {
                ConstantKind::Boolean(!value)
            } else {
                return None;
            }
        }
        UnaryOperator::Sub => {
            if let Some(value) = as_float(value) {
                float_as_constant(-value)
            } else {
                return None;
            }
        }
        UnaryOperator::Add => {
            if let Some(value) = as_float(value) {
                float_as_constant(value)
            } else {
                return None;
            }
        }
        UnaryOperator::BitNot => {
            if let Some(value) = as_int(value, 0) {
                ConstantKind::Integer(!value)
            } else {
                return None;
            }
        }
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

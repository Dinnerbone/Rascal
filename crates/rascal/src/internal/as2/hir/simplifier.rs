use crate::internal::as2::ast::{BinaryOperator, UnaryOperator};
use crate::internal::as2::hir::visitor::{MutVisitor, walk_expr};
use crate::internal::as2::hir::{ConstantKind, Document, Expr, ExprKind};

struct Simplifier {
    anything_changed: bool,
}

impl MutVisitor for Simplifier {
    fn visit_expr(&mut self, expr: &mut Expr) {
        walk_expr(self, expr);
        match &mut expr.value {
            ExprKind::BinaryOperator(op, left, right) => {
                if let ExprKind::Constant(left) = &left.value
                    && let ExprKind::Constant(right) = &right.value
                {
                    #[expect(clippy::single_match)]
                    match (op, left, right) {
                        (
                            BinaryOperator::StringAdd,
                            ConstantKind::String(left),
                            ConstantKind::String(right),
                        ) => {
                            expr.value = ExprKind::Constant(ConstantKind::String(format!(
                                "{}{}",
                                left, right
                            )));
                            self.anything_changed = true;
                        }
                        _ => {}
                    }
                }
            }
            ExprKind::UnaryOperator(op, inner) => {
                if let ExprKind::Constant(value) = &inner.value {
                    #[expect(clippy::single_match)]
                    match (op, value) {
                        (UnaryOperator::LogicalNot, ConstantKind::Boolean(value)) => {
                            expr.value = ExprKind::Constant(ConstantKind::Boolean(!value));
                            self.anything_changed = true;
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
}

pub fn simplify(document: &mut Document) -> bool {
    let mut simplifier = Simplifier {
        anything_changed: false,
    };

    match document {
        Document::Script { statements } => {
            for statement in statements {
                simplifier.visit_statement(statement);
            }
        }
        Document::Interface(_interface) => {}
        Document::Class(_class) => {}
        Document::Invalid => {}
    }

    simplifier.anything_changed
}

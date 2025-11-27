use crate::ast::{BinaryOperator, Expr, ExprKind, UnaryOperator};
use crate::lexer::operator::Operator;
use crate::lexer::tokens::{Token, TokenKind};
use crate::parser::Tokens;
use rascal_common::span::Span;
use serde::Serialize;
use std::cmp::Ordering;
use winnow::error::ParserError;
use winnow::token::any;
use winnow::{ModalResult, Parser};

#[derive(Debug, Clone, Copy, Serialize, PartialEq, Eq, PartialOrd, Ord)]
enum OperatorPrecedence {
    MulDivMod,
    AddSub,
    BitShifts,
    Comparison,
    Equality,
    BitMath,
    Logic,
    Other,
    Assignment,
}

impl BinaryOperator {
    pub(crate) fn should_swap(left: BinaryOperator, right: BinaryOperator) -> bool {
        match right.precedence().cmp(&left.precedence()) {
            Ordering::Greater => true,
            Ordering::Equal if left.precedence() < OperatorPrecedence::Other => true,
            _ => false,
        }
    }
}

impl BinaryOperator {
    fn precedence(&self) -> OperatorPrecedence {
        match self {
            BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::Modulo => {
                OperatorPrecedence::MulDivMod
            }
            BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::StringAdd => {
                OperatorPrecedence::AddSub
            }
            BinaryOperator::BitShiftRight
            | BinaryOperator::BitShiftLeft
            | BinaryOperator::BitShiftRightUnsigned => OperatorPrecedence::BitShifts,
            BinaryOperator::LessThan
            | BinaryOperator::LessThanEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterThanEqual
            | BinaryOperator::StringLessThan
            | BinaryOperator::StringGreaterThan
            | BinaryOperator::StringGreaterThanEqual
            | BinaryOperator::StringLessThanEqual => OperatorPrecedence::Comparison,
            BinaryOperator::Equal
            | BinaryOperator::StrictEqual
            | BinaryOperator::NotEqual
            | BinaryOperator::StrictNotEqual
            | BinaryOperator::StringEqual
            | BinaryOperator::StringNotEqual => OperatorPrecedence::Equality,
            BinaryOperator::BitAnd | BinaryOperator::BitOr | BinaryOperator::BitXor => {
                OperatorPrecedence::BitMath
            }
            BinaryOperator::LogicalOr
            | BinaryOperator::LogicalAnd
            | BinaryOperator::BooleanAnd
            | BinaryOperator::BooleanOr => OperatorPrecedence::Logic,
            BinaryOperator::Assign
            | BinaryOperator::AddAssign
            | BinaryOperator::SubAssign
            | BinaryOperator::DivideAssign
            | BinaryOperator::MultiplyAssign
            | BinaryOperator::ModuloAssign
            | BinaryOperator::BitAndAssign
            | BinaryOperator::BitOrAssign
            | BinaryOperator::BitXorAssign
            | BinaryOperator::BitShiftLeftAssign
            | BinaryOperator::BitShiftRightAssign
            | BinaryOperator::BitShiftRightUnsignedAssign => OperatorPrecedence::Assignment,
            BinaryOperator::InstanceOf => OperatorPrecedence::Other,
        }
    }
}

pub(crate) fn expr_for_binary_operator<'i>(
    op: BinaryOperator,
    a: Box<Expr<'i>>,
    b: Box<Expr<'i>>,
) -> Expr<'i> {
    match *b {
        Expr {
            value: ExprKind::Ternary { condition, yes, no },
            ..
        } if op.precedence() != OperatorPrecedence::Assignment => Expr::new(
            Span::encompassing(condition.span, no.span),
            ExprKind::Ternary {
                condition: Box::new(expr_for_binary_operator(op, a, condition)),
                yes,
                no,
            },
        ),
        Expr {
            value: ExprKind::BinaryOperator(bop, ba, bb),
            ..
        } if BinaryOperator::should_swap(op, bop) => {
            let new_left = Box::new(expr_for_binary_operator(op, a, ba));
            Expr::new(
                Span::encompassing(new_left.span, bb.span),
                ExprKind::BinaryOperator(bop, new_left, bb),
            )
        }
        _ => Expr::new(
            Span::encompassing(a.span, b.span),
            ExprKind::BinaryOperator(op, a, b),
        ),
    }
}

pub(crate) fn expr_for_unary_operator(op: UnaryOperator, expr: Box<Expr>, op_span: Span) -> Expr {
    match *expr {
        Expr {
            value: ExprKind::BinaryOperator(binary_op, a, b),
            ..
        } => {
            let new_left = Box::new(expr_for_unary_operator(op, a, op_span));
            Expr::new(
                Span::encompassing(new_left.span, b.span),
                ExprKind::BinaryOperator(binary_op, new_left, b),
            )
        }
        Expr {
            value: ExprKind::Ternary { condition, yes, no },
            ..
        } => {
            let new_condition = Box::new(expr_for_unary_operator(op, condition, op_span));
            Expr::new(
                Span::encompassing(new_condition.span, no.span),
                ExprKind::Ternary {
                    condition: new_condition,
                    yes,
                    no,
                },
            )
        }
        _ => Expr::new(
            Span::encompassing(op_span, expr.span),
            ExprKind::UnaryOperator(op, expr),
        ),
    }
}

pub fn binary_operator(i: &mut Tokens<'_>) -> ModalResult<BinaryOperator> {
    let Token {
        kind: TokenKind::Operator(operator),
        ..
    } = any.parse_next(i)?
    else {
        return Err(ParserError::from_input(i));
    };
    Ok(match operator {
        Operator::Add => BinaryOperator::Add,
        Operator::Assign => BinaryOperator::Assign,
        Operator::AddAssign => BinaryOperator::AddAssign,
        Operator::Sub => BinaryOperator::Sub,
        Operator::SubAssign => BinaryOperator::SubAssign,
        Operator::Divide => BinaryOperator::Divide,
        Operator::DivideAssign => BinaryOperator::DivideAssign,
        Operator::Multiply => BinaryOperator::Multiply,
        Operator::MultiplyAssign => BinaryOperator::MultiplyAssign,
        Operator::Modulo => BinaryOperator::Modulo,
        Operator::ModuloAssign => BinaryOperator::ModuloAssign,
        Operator::BitAnd => BinaryOperator::BitAnd,
        Operator::BitOr => BinaryOperator::BitOr,
        Operator::BitXor => BinaryOperator::BitXor,
        Operator::BitShiftLeft => BinaryOperator::BitShiftLeft,
        Operator::BitShiftRight => BinaryOperator::BitShiftRight,
        Operator::BitShiftRightUnsigned => BinaryOperator::BitShiftRightUnsigned,
        Operator::BitAndAssign => BinaryOperator::BitAndAssign,
        Operator::BitOrAssign => BinaryOperator::BitOrAssign,
        Operator::BitXorAssign => BinaryOperator::BitXorAssign,
        Operator::BitShiftLeftAssign => BinaryOperator::BitShiftLeftAssign,
        Operator::BitShiftRightAssign => BinaryOperator::BitShiftRightAssign,
        Operator::BitShiftRightUnsignedAssign => BinaryOperator::BitShiftRightUnsignedAssign,
        Operator::Equal => BinaryOperator::Equal,
        Operator::StrictEqual => BinaryOperator::StrictEqual,
        Operator::NotEqual => BinaryOperator::NotEqual,
        Operator::StrictNotEqual => BinaryOperator::StrictNotEqual,
        Operator::LessThan => BinaryOperator::LessThan,
        Operator::LessThanEqual => BinaryOperator::LessThanEqual,
        Operator::GreaterThan => BinaryOperator::GreaterThan,
        Operator::GreaterThanEqual => BinaryOperator::GreaterThanEqual,
        Operator::LogicalAnd => BinaryOperator::LogicalAnd,
        Operator::LogicalOr => BinaryOperator::LogicalOr,
        _ => return Err(ParserError::from_input(i)),
    })
}

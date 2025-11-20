use crate::ast::{BinaryOperator, ExprKind, UnaryOperator};
use crate::lexer::operator::Operator;
use crate::lexer::tokens::{Token, TokenKind};
use crate::parser::Tokens;
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
            BinaryOperator::Add | BinaryOperator::Sub => OperatorPrecedence::AddSub,
            BinaryOperator::BitShiftRight
            | BinaryOperator::BitShiftLeft
            | BinaryOperator::BitShiftRightUnsigned => OperatorPrecedence::BitShifts,
            BinaryOperator::LessThan
            | BinaryOperator::LessThanEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterThanEqual => OperatorPrecedence::Comparison,
            BinaryOperator::Equal
            | BinaryOperator::StrictEqual
            | BinaryOperator::NotEqual
            | BinaryOperator::StrictNotEqual => OperatorPrecedence::Equality,
            BinaryOperator::BitAnd | BinaryOperator::BitOr | BinaryOperator::BitXor => {
                OperatorPrecedence::BitMath
            }
            BinaryOperator::LogicalOr | BinaryOperator::LogicalAnd => OperatorPrecedence::Logic,
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

impl<'i> ExprKind<'i> {
    pub(crate) fn for_binary_operator(
        op: BinaryOperator,
        a: Box<ExprKind<'i>>,
        b: Box<ExprKind<'i>>,
    ) -> ExprKind<'i> {
        match *b {
            ExprKind::Ternary { condition, yes, no }
                if op.precedence() != OperatorPrecedence::Assignment =>
            {
                ExprKind::Ternary {
                    condition: Box::new(ExprKind::for_binary_operator(op, a, condition)),
                    yes,
                    no,
                }
            }
            ExprKind::BinaryOperator(bop, ba, bb) if BinaryOperator::should_swap(op, bop) => {
                ExprKind::BinaryOperator(
                    bop,
                    Box::new(ExprKind::for_binary_operator(op, a, ba)),
                    bb,
                )
            }
            _ => ExprKind::BinaryOperator(op, a, b),
        }
    }
}

impl<'i> ExprKind<'i> {
    pub(crate) fn for_unary_operator(op: UnaryOperator, expr: Box<ExprKind<'i>>) -> ExprKind<'i> {
        match *expr {
            ExprKind::BinaryOperator(binary_op, a, b) => ExprKind::BinaryOperator(
                binary_op,
                Box::new(ExprKind::for_unary_operator(op, a)),
                b,
            ),
            ExprKind::Ternary { condition, yes, no } => ExprKind::Ternary {
                condition: Box::new(ExprKind::for_unary_operator(op, condition)),
                yes,
                no,
            },
            _ => ExprKind::UnaryOperator(op, expr),
        }
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

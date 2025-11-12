use crate::lexer::operator::Operator;
use crate::lexer::tokens::{Token, TokenKind};
use crate::parser::Tokens;
use crate::parser::expression::Expr;
use serde::Serialize;
use winnow::error::ParserError;
use winnow::token::any;
use winnow::{ModalResult, Parser};

#[derive(Debug, Clone, Serialize, PartialEq)]
pub(crate) enum BinaryOperator {
    Add,
    Assign,
    AddAssign,
    Sub,
    SubAssign,
    Divide,
    DivideAssign,
    Multiply,
    MultiplyAssign,
    Modulo,
    ModuloAssign,
    BitAnd,
    BitAndAssign,
    BitOr,
    BitOrAssign,
    BitXor,
    BitXorAssign,
    BitShiftLeft,
    BitShiftLeftAssign,
    BitShiftRight,
    BitShiftRightAssign,
    BitShiftRightUnsigned,
    BitShiftRightUnsignedAssign,
    Equal,
    StrictEqual,
    NotEqual,
    StrictNotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub(crate) enum UnaryOperator {
    Sub,
    BitNot,
    Increment(Affix),
    Decrement(Affix),
    LogicalNot,
}

impl Expr {
    pub(crate) fn for_unary_operator(op: UnaryOperator, expr: Box<Expr>) -> Expr {
        if let Expr::BinaryOperator(binary_op, a, b) = *expr {
            Expr::BinaryOperator(binary_op, Box::new(Expr::for_unary_operator(op, a)), b)
        } else {
            Expr::UnaryOperator(op, expr)
        }
    }
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum Affix {
    Postfix,
    Prefix,
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

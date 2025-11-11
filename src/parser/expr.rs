use crate::lexer::tokens::BinaryOperator;
use serde::Serialize;

#[derive(Debug, Clone, Serialize, PartialEq)]
#[allow(dead_code)]
pub(crate) enum Expr {
    Constant(Constant),
    Call { name: Box<Expr>, args: Vec<Expr> },
    BinaryOperator(BinaryOperator, Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, Serialize, PartialEq)]
#[allow(dead_code)]
pub(crate) enum Constant {
    String(String),
    Identifier(String),
}

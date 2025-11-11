use crate::lexer::tokens::{BinaryOperator, TokenKind};
use crate::parser::{Tokens, identifier, skip_newline, string};
use serde::Serialize;
use winnow::combinator::{fail, peek, separated};
use winnow::error::ParserError;
use winnow::error::{ContextError, ErrMode, StrContext};
use winnow::stream::ParseSlice;
use winnow::token::{any, take_while};
use winnow::{ModalResult, Parser};

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
    Float(f64),
    Integer(i32),
}

pub(crate) fn expression(i: &mut Tokens<'_>) -> ModalResult<Expr> {
    take_while(0.., TokenKind::Newline).parse_next(i)?;
    let token = peek(any).parse_next(i)?;
    match token.kind {
        TokenKind::String => {
            let val = string.parse_next(i)?;
            expr_next(Expr::Constant(Constant::String(val))).context(StrContext::Label("string"))
        }
        TokenKind::Identifier => {
            let val = identifier.parse_next(i)?;
            expr_next(Expr::Constant(Constant::Identifier(val)))
                .context(StrContext::Label("identifier"))
        }
        TokenKind::Float => {
            let val = float.parse_next(i)?;
            expr_next(Expr::Constant(Constant::Float(val))).context(StrContext::Label("float"))
        }
        TokenKind::Integer => {
            let val = integer.parse_next(i)?;
            expr_next(Expr::Constant(Constant::Integer(val))).context(StrContext::Label("integer"))
        }
        _ => {
            return fail.parse_next(i);
        }
    }
    .parse_next(i)
}

fn float(i: &mut Tokens<'_>) -> ModalResult<f64> {
    let raw = TokenKind::Float.parse_next(i)?.raw;
    let value = raw
        .parse_slice()
        .ok_or_else(|| ParserError::from_input(&raw))?;
    Ok(value)
}

fn integer(i: &mut Tokens<'_>) -> ModalResult<i32> {
    let raw = TokenKind::Integer.parse_next(i)?.raw;
    let value = raw
        .parse_slice()
        .ok_or_else(|| ParserError::from_input(&raw))?;
    Ok(value)
}

fn expr_next<'i>(prior: Expr) -> impl Parser<Tokens<'i>, Expr, ErrMode<ContextError>> {
    move |i: &mut Tokens<'i>| {
        let prior = prior.clone();
        take_while(0.., TokenKind::Newline).parse_next(i)?;
        if i.is_empty() {
            return Ok(prior);
        }
        let token = peek(any).parse_next(i)?;
        match token.kind {
            TokenKind::OpenParen => {
                TokenKind::OpenParen.parse_next(i)?;
                let args = expr_list
                    .context(StrContext::Label("arguments"))
                    .parse_next(i)?;
                TokenKind::CloseParen.parse_next(i)?;
                Ok(Expr::Call {
                    name: Box::new(prior),
                    args,
                })
            }
            TokenKind::BinaryOperator(op) => {
                TokenKind::BinaryOperator(op).parse_next(i)?;
                expression
                    .parse_next(i)
                    .map(|next| Expr::BinaryOperator(op, Box::new(prior), Box::new(next)))
            }
            _ => Ok(prior),
        }
    }
}

pub(crate) fn expr_list(i: &mut Tokens<'_>) -> ModalResult<Vec<Expr>> {
    separated(0.., skip_newline(expression), TokenKind::Comma).parse_next(i)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::tokens::{BinaryOperator, Token, TokenKind};
    use crate::parser::tests::build_tokens;
    use winnow::stream::TokenSlice;

    fn parse_expr(tokens: &[Token<'_>]) -> ModalResult<Expr> {
        expression(&mut TokenSlice::new(tokens))
    }

    #[test]
    fn test_identifier() {
        let tokens = build_tokens(&[(TokenKind::Identifier, "foo")]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(Expr::Constant(Constant::Identifier("foo".to_string())))
        );
    }

    #[test]
    fn test_string() {
        let tokens = build_tokens(&[(TokenKind::String, "hello")]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(Expr::Constant(Constant::String("hello".to_string())))
        );
    }

    #[test]
    fn test_call_no_args() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "foo"),
            (TokenKind::OpenParen, "("),
            (TokenKind::CloseParen, ")"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(Expr::Call {
                name: Box::new(Expr::Constant(Constant::Identifier("foo".to_string()))),
                args: vec![]
            })
        );
    }

    #[test]
    fn test_call_two_args() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "foo"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::Comma, ","),
            (TokenKind::String, "str"),
            (TokenKind::CloseParen, ")"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(Expr::Call {
                name: Box::new(Expr::Constant(Constant::Identifier("foo".to_string()))),
                args: vec![
                    Expr::Constant(Constant::Identifier("a".to_string())),
                    Expr::Constant(Constant::String("str".to_string()))
                ]
            })
        );
    }

    #[test]
    fn test_binary_add() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::BinaryOperator(BinaryOperator::Add), "+"),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(Expr::BinaryOperator(
                BinaryOperator::Add,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
        );
    }

    #[test]
    fn test_binary_right_associative() {
        // a + b + c parses as a + (b + c)
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::BinaryOperator(BinaryOperator::Add), "+"),
            (TokenKind::Identifier, "b"),
            (TokenKind::BinaryOperator(BinaryOperator::Add), "+"),
            (TokenKind::Identifier, "c"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(Expr::BinaryOperator(
                BinaryOperator::Add,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::BinaryOperator(
                    BinaryOperator::Add,
                    Box::new(Expr::Constant(Constant::Identifier("b".to_string()))),
                    Box::new(Expr::Constant(Constant::Identifier("c".to_string())))
                ))
            ))
        );
    }

    #[test]
    fn test_integer() {
        let tokens = build_tokens(&[(TokenKind::Integer, "0123")]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(Expr::Constant(Constant::Integer(123)))
        );
    }

    #[test]
    fn test_float() {
        let tokens = build_tokens(&[(TokenKind::Float, "012.345")]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(Expr::Constant(Constant::Float(12.345)))
        );
    }

    #[test]
    fn test_float_leading_period() {
        let tokens = build_tokens(&[(TokenKind::Float, ".123")]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(Expr::Constant(Constant::Float(0.123)))
        );
    }

    #[test]
    fn test_float_trailing_period() {
        let tokens = build_tokens(&[(TokenKind::Float, "123.")]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(Expr::Constant(Constant::Float(123.0)))
        );
    }
}

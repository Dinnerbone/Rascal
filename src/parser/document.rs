use crate::lexer::tokens::TokenKind;
use crate::parser::expr::{Constant, Expr};
use crate::parser::statement::{Statement, statement};
use crate::parser::{Tokens, identifier, skip_newline, string};
use serde::Serialize;
use winnow::combinator::{eof, fail, peek, repeat_till, separated};
use winnow::error::{ContextError, ErrMode, StrContext};
use winnow::token::{any, take_while};
use winnow::{ModalResult, Parser};

#[derive(Debug, Serialize)]
pub struct Document {
    #[allow(dead_code)]
    statements: Vec<Statement>,
}

pub fn document(tokens: &mut Tokens<'_>) -> ModalResult<Document> {
    let (statements, _) = repeat_till(0.., statement.context(StrContext::Label("statement")), eof)
        .parse_next(tokens)?;
    Ok(Document { statements })
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
        _ => {
            return fail.parse_next(i);
        }
    }
    .parse_next(i)
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

fn expr_list(i: &mut Tokens<'_>) -> ModalResult<Vec<Expr>> {
    separated(0.., skip_newline(expression), TokenKind::Comma).parse_next(i)
}

#[cfg(test)]
mod stmt_tests {
    use super::*;
    use crate::lexer::tokens::{BinaryOperator, Token, TokenKind};
    use crate::parser::tests::build_tokens;
    use winnow::stream::TokenSlice;

    fn parse_expr(tokens: &[Token<'_>]) -> ModalResult<Expr> {
        expression(&mut TokenSlice::new(tokens))
    }

    #[test]
    fn expr_identifier() {
        let tokens = build_tokens(&[(TokenKind::Identifier, "foo")]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(Expr::Constant(Constant::Identifier("foo".to_string())))
        );
    }

    #[test]
    fn expr_string() {
        let tokens = build_tokens(&[(TokenKind::String, "hello")]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(Expr::Constant(Constant::String("hello".to_string())))
        );
    }

    #[test]
    fn expr_call_no_args() {
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
    fn expr_call_two_args() {
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
    fn expr_binary_add() {
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
    fn expr_binary_right_associative() {
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
}

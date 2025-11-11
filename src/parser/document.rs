use crate::lexer::tokens::{BinaryOperator, Keyword, TokenKind};
use crate::parser::Tokens;
use crate::parser::expr::{Constant, Expr, Statement};
use serde::Serialize;
use winnow::combinator::{alt, cond, cut_err, eof, fail, opt, peek, repeat_till, separated};
use winnow::error::{ContextError, ErrMode, StrContext, StrContextValue};
use winnow::stream::Stream;
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

fn statement(i: &mut Tokens<'_>) -> ModalResult<Statement> {
    take_while(0.., TokenKind::Newline).parse_next(i)?;
    let checkpoint = i.checkpoint();
    let token = any.parse_next(i)?;
    let result = match token.kind {
        TokenKind::Keyword(Keyword::Var) => declaration
            .context(StrContext::Label("declaration"))
            .parse_next(i),
        _ => {
            i.reset(&checkpoint);
            expression.parse_next(i).map(Statement::Expr)
        }
    }?;

    if !i.is_empty() {
        alt((TokenKind::Semicolon, TokenKind::Newline))
            .context(StrContext::Label("end of line"))
            .context(StrContext::Expected(StrContextValue::CharLiteral(';')))
            .parse_next(i)?;
    }

    take_while(0.., TokenKind::Newline).parse_next(i)?;

    Ok(result)
}

fn expression(i: &mut Tokens<'_>) -> ModalResult<Expr> {
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

fn string(i: &mut Tokens<'_>) -> ModalResult<String> {
    Ok(TokenKind::String.parse_next(i)?.raw.to_string())
}

fn identifier(i: &mut Tokens<'_>) -> ModalResult<String> {
    Ok(TokenKind::Identifier.parse_next(i)?.raw.to_string())
}

fn declaration(i: &mut Tokens<'_>) -> ModalResult<Statement> {
    let name = cut_err(skip_newline(identifier))
        .context(StrContext::Label("variable name"))
        .parse_next(i)?;
    let equals = cut_err(opt(skip_newline(TokenKind::BinaryOperator(
        BinaryOperator::Assign,
    ))))
    .parse_next(i)?
    .is_some();
    let value = cond(equals, skip_newline(expression)).parse_next(i)?;

    Ok(Statement::Declare { name, value })
}

fn expr_list(i: &mut Tokens<'_>) -> ModalResult<Vec<Expr>> {
    separated(0.., skip_newline(expression), TokenKind::Comma).parse_next(i)
}

fn skip_newline<'i, O, P>(mut inner: P) -> impl Parser<Tokens<'i>, O, ErrMode<ContextError>>
where
    P: Parser<Tokens<'i>, O, ErrMode<ContextError>>,
{
    move |input: &mut Tokens<'i>| {
        opt(TokenKind::Newline).parse_next(input)?;
        inner.parse_next(input)
    }
}

#[cfg(test)]
mod stmt_tests {
    use super::*;
    use crate::lexer::tokens::{BinaryOperator, Keyword, Token, TokenKind};
    use crate::source::Span;
    use winnow::stream::TokenSlice;

    fn build_tokens<'a>(spec: &'a [(TokenKind, &'a str)]) -> Vec<Token<'a>> {
        let mut pos = 0usize;
        spec.iter()
            .map(|(k, raw)| {
                let start = pos;
                pos += raw.len().max(1);
                let end = pos;
                Token::new(*k, Span::new_unchecked(start, end), raw)
            })
            .collect()
    }

    fn parse_stmt(tokens: &[Token<'_>]) -> ModalResult<Statement> {
        statement(&mut TokenSlice::new(tokens))
    }

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

    #[test]
    fn stmt_declare_no_value() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Var), "var"),
            (TokenKind::Identifier, "x"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::Declare {
                name: "x".to_string(),
                value: None
            })
        );
    }

    #[test]
    fn stmt_declare_assign_constant() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Var), "var"),
            (TokenKind::Identifier, "x"),
            (TokenKind::BinaryOperator(BinaryOperator::Assign), "="),
            (TokenKind::String, "hi"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::Declare {
                name: "x".to_string(),
                value: Some(Expr::Constant(Constant::String("hi".to_string())))
            })
        );
    }

    #[test]
    fn stmt_declare_assign_call() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Var), "var"),
            (TokenKind::Identifier, "x"),
            (TokenKind::BinaryOperator(BinaryOperator::Assign), "="),
            (TokenKind::Identifier, "foo"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::CloseParen, ")"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::Declare {
                name: "x".to_string(),
                value: Some(Expr::Call {
                    name: Box::new(Expr::Constant(Constant::Identifier("foo".to_string()))),
                    args: vec![Expr::Constant(Constant::Identifier("a".to_string()))]
                })
            })
        );
    }

    #[test]
    fn stmt_wraps_expr() {
        let tokens = build_tokens(&[(TokenKind::Identifier, "foo")]);
        let got = parse_stmt(&tokens);
        assert_eq!(
            got,
            Ok(Statement::Expr(Expr::Constant(Constant::Identifier(
                "foo".to_string()
            ))))
        );
    }

    #[test]
    fn stmt_skips_leading_newline() {
        let tokens = build_tokens(&[(TokenKind::Newline, "\n"), (TokenKind::Identifier, "bar")]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::Expr(Expr::Constant(Constant::Identifier(
                "bar".to_string()
            ))))
        );
    }
}

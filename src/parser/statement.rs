use crate::lexer::tokens::{Keyword, Operator, TokenKind};
use crate::parser::expression::{Expr, expression};
use crate::parser::{Tokens, identifier, skip_newline};
use serde::Serialize;
use winnow::combinator::{alt, cond, cut_err, opt};
use winnow::error::{StrContext, StrContextValue};
use winnow::stream::Stream;
use winnow::token::{any, take_while};
use winnow::{ModalResult, Parser};

#[derive(Debug, Clone, Serialize, PartialEq)]
#[allow(dead_code)]
pub(crate) enum Statement {
    Declare { name: String, value: Option<Expr> },
    Expr(Expr),
}

pub(crate) fn statement(i: &mut Tokens<'_>) -> ModalResult<Statement> {
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

fn declaration(i: &mut Tokens<'_>) -> ModalResult<Statement> {
    let name = cut_err(skip_newline(identifier))
        .context(StrContext::Label("variable name"))
        .parse_next(i)?;
    let equals = cut_err(opt(skip_newline(TokenKind::Operator(Operator::Assign))))
        .parse_next(i)?
        .is_some();
    let value = cond(equals, skip_newline(expression)).parse_next(i)?;

    Ok(Statement::Declare { name, value })
}

#[cfg(test)]
mod stmt_tests {
    use super::*;
    use crate::lexer::tokens::{Keyword, Operator, Token, TokenKind};
    use crate::parser::expression::{Constant, Expr};
    use crate::parser::tests::build_tokens;
    use winnow::stream::TokenSlice;

    fn parse_stmt(tokens: &[Token<'_>]) -> ModalResult<Statement> {
        statement(&mut TokenSlice::new(tokens))
    }

    #[test]
    fn test_declare_no_value() {
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
    fn test_declare_assign_constant() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Var), "var"),
            (TokenKind::Identifier, "x"),
            (TokenKind::Operator(Operator::Assign), "="),
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
    fn test_declare_assign_call() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Var), "var"),
            (TokenKind::Identifier, "x"),
            (TokenKind::Operator(Operator::Assign), "="),
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
    fn test_wraps_expr() {
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
    fn test_skips_leading_newline() {
        let tokens = build_tokens(&[(TokenKind::Newline, "\n"), (TokenKind::Identifier, "bar")]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::Expr(Expr::Constant(Constant::Identifier(
                "bar".to_string()
            ))))
        );
    }
}

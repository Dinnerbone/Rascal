use crate::lexer::operator::Operator;
use crate::lexer::tokens::{Keyword, TokenKind};
use crate::parser::expression::{Expr, expr_list, expression};
use crate::parser::{Tokens, identifier, skip_newline};
use serde::Serialize;
use winnow::combinator::{cond, cut_err, opt, peek, separated};
use winnow::error::{ContextError, ErrMode, StrContext};
use winnow::stream::Stream;
use winnow::token::any;
use winnow::{ModalResult, Parser};

#[derive(Debug, Clone, Serialize, PartialEq)]
pub(crate) enum Statement {
    Declare(Vec<Declaration>),
    Return(Vec<Expr>),
    Expr(Expr),
    Block(Vec<Statement>),
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub(crate) struct Function {
    pub(crate) name: Option<String>,
    pub(crate) args: Vec<String>,
    pub(crate) body: Vec<Statement>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub(crate) struct Declaration {
    pub(crate) name: String,
    pub(crate) value: Option<Expr>,
}

pub(crate) fn statement(i: &mut Tokens<'_>) -> ModalResult<Statement> {
    let checkpoint = i.checkpoint();
    let token = any.parse_next(i)?;
    let result = match token.kind {
        TokenKind::Keyword(Keyword::Var) => declaration_list
            .context(StrContext::Label("declaration"))
            .parse_next(i)?,
        TokenKind::Keyword(Keyword::Return) => {
            let values = if opt(TokenKind::OpenParen).parse_next(i)?.is_some() {
                let values = expr_list.parse_next(i)?;
                TokenKind::CloseParen.parse_next(i)?;
                values
            } else if let Some(expr) = opt(expression).parse_next(i)? {
                vec![expr]
            } else {
                vec![]
            };
            Statement::Return(values)
        }
        TokenKind::OpenBrace => {
            let statements = statement_list(true).parse_next(i)?;
            TokenKind::CloseBrace.parse_next(i)?;
            Statement::Block(statements)
        }
        _ => {
            i.reset(&checkpoint);
            expression.parse_next(i).map(Statement::Expr)?
        }
    };

    Ok(result)
}

pub(crate) fn statement_list<'i>(
    is_block: bool,
) -> impl Parser<Tokens<'i>, Vec<Statement>, ErrMode<ContextError>> {
    move |i: &mut Tokens<'i>| {
        let mut result = vec![];
        while let Ok(peek) = peek(any::<_, ErrMode<ContextError>>).parse_next(i) {
            match &peek.kind {
                TokenKind::Newline | TokenKind::Semicolon => {
                    any.parse_next(i)?;
                }
                TokenKind::CloseBrace if is_block => {
                    break;
                }
                _ => {
                    result.push(statement.parse_next(i)?);
                }
            }
        }

        Ok(result)
    }
}

fn declaration_list(i: &mut Tokens<'_>) -> ModalResult<Statement> {
    let declarations = separated(0.., declaration, TokenKind::Comma).parse_next(i)?;

    Ok(Statement::Declare(declarations))
}

fn declaration(i: &mut Tokens<'_>) -> ModalResult<Declaration> {
    let name = cut_err(skip_newline(identifier))
        .context(StrContext::Label("variable name"))
        .parse_next(i)?;
    let equals = cut_err(opt(skip_newline(TokenKind::Operator(Operator::Assign))))
        .parse_next(i)?
        .is_some();
    let value = cond(equals, skip_newline(expression)).parse_next(i)?;

    Ok(Declaration { name, value })
}

pub(crate) fn function(i: &mut Tokens<'_>) -> ModalResult<Function> {
    let name = skip_newline(opt(identifier)).parse_next(i)?;
    skip_newline(TokenKind::OpenParen).parse_next(i)?;
    let args = separated(0.., skip_newline(identifier), TokenKind::Comma).parse_next(i)?;
    skip_newline(TokenKind::CloseParen).parse_next(i)?;
    skip_newline(TokenKind::OpenBrace).parse_next(i)?;
    let body = statement_list(true).parse_next(i)?;
    skip_newline(TokenKind::CloseBrace).parse_next(i)?;

    Ok(Function { name, args, body })
}

#[cfg(test)]
mod stmt_tests {
    use super::*;
    use crate::lexer::tokens::{Keyword, QuoteKind, Token, TokenKind};
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
            Ok(Statement::Declare(vec![Declaration {
                name: "x".to_string(),
                value: None
            }]))
        );
    }

    #[test]
    fn test_declare_assign_constant() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Var), "var"),
            (TokenKind::Identifier, "x"),
            (TokenKind::Operator(Operator::Assign), "="),
            (TokenKind::String(QuoteKind::Double), "hi"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::Declare(vec![Declaration {
                name: "x".to_string(),
                value: Some(Expr::Constant(Constant::String("hi".to_string())))
            }]))
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
            Ok(Statement::Declare(vec![Declaration {
                name: "x".to_string(),
                value: Some(Expr::Call {
                    name: Box::new(Expr::Constant(Constant::Identifier("foo".to_string()))),
                    args: vec![Expr::Constant(Constant::Identifier("a".to_string()))]
                })
            }]))
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

    #[test]
    fn test_return_no_value() {
        let tokens = build_tokens(&[(TokenKind::Keyword(Keyword::Return), "return")]);
        assert_eq!(parse_stmt(&tokens), Ok(Statement::Return(vec![])))
    }

    #[test]
    fn test_return_regular_value() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Return), "return"),
            (TokenKind::Identifier, "a"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::Return(vec![Expr::Constant(
                Constant::Identifier("a".to_string())
            )]))
        )
    }

    #[test]
    fn test_return_as_function() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Return), "return"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::Comma, ","),
            (TokenKind::Identifier, "b"),
            (TokenKind::CloseParen, ")"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::Return(vec![
                Expr::Constant(Constant::Identifier("a".to_string())),
                Expr::Constant(Constant::Identifier("b".to_string()))
            ]))
        )
    }
}

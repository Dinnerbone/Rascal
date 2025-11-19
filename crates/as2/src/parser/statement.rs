use crate::ast::{Declaration, ForCondition, Function, Statement};
use crate::lexer::operator::Operator;
use crate::lexer::tokens::{Keyword, TokenKind};
use crate::parser::expression::{expr_list, expression, type_name};
use crate::parser::{Tokens, identifier, skip_newlines};
use winnow::combinator::{alt, cond, cut_err, opt, peek, separated};
use winnow::error::{ContextError, ErrMode, StrContext};
use winnow::stream::Stream;
use winnow::token::{any, take_while};
use winnow::{ModalResult, Parser};

pub(crate) fn statement<'i>(i: &mut Tokens<'i>) -> ModalResult<Statement<'i>> {
    let checkpoint = i.checkpoint();
    skip_newlines(i)?;
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
        TokenKind::Keyword(Keyword::For) => for_loop
            .context(StrContext::Label("for loop"))
            .parse_next(i)?,
        TokenKind::Keyword(Keyword::If) => if_else
            .context(StrContext::Label("if statement"))
            .parse_next(i)?,
        TokenKind::Keyword(Keyword::Break) => Statement::Break,
        TokenKind::Keyword(Keyword::Continue) => Statement::Continue,
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
) -> impl Parser<Tokens<'i>, Vec<Statement<'i>>, ErrMode<ContextError>> {
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

fn declaration_list<'i>(i: &mut Tokens<'i>) -> ModalResult<Statement<'i>> {
    skip_newlines(i)?;
    let declarations = separated(0.., declaration, TokenKind::Comma).parse_next(i)?;

    Ok(Statement::Declare(declarations))
}

fn declaration<'i>(i: &mut Tokens<'i>) -> ModalResult<Declaration<'i>> {
    let name = cut_err(identifier)
        .context(StrContext::Label("variable name"))
        .parse_next(i)?;
    opt(type_name).parse_next(i)?;
    let equals = cut_err(opt(TokenKind::Operator(Operator::Assign)))
        .parse_next(i)?
        .is_some();
    let value = cond(equals, expression).parse_next(i)?;

    Ok(Declaration {
        name: name.to_owned(),
        value,
    })
}

fn if_else<'i>(i: &mut Tokens<'i>) -> ModalResult<Statement<'i>> {
    TokenKind::OpenParen.parse_next(i)?;
    let condition = expression.parse_next(i)?;
    TokenKind::CloseParen.parse_next(i)?;
    let yes = Box::new(statement.parse_next(i)?);
    take_while(0.., TokenKind::Semicolon).parse_next(i)?;

    let no = if opt(TokenKind::Keyword(Keyword::Else))
        .parse_next(i)?
        .is_some()
    {
        Some(Box::new(statement.parse_next(i)?))
    } else {
        None
    };

    Ok(Statement::If { condition, yes, no })
}

fn for_loop<'i>(i: &mut Tokens<'i>) -> ModalResult<Statement<'i>> {
    TokenKind::OpenParen.parse_next(i)?;
    let condition = if let Some((var, name, _)) = opt((
        opt(TokenKind::Keyword(Keyword::Var)),
        identifier,
        TokenKind::Keyword(Keyword::In),
    ))
    .parse_next(i)?
    {
        ForCondition::Enumerate {
            variable: name,
            declare: var.is_some(),
            object: expression.parse_next(i)?,
        }
    } else {
        let (init, _, cond, _, next) = (
            opt(alt((
                expression.map(Statement::Expr),
                (TokenKind::Keyword(Keyword::Var), declaration_list).map(|v| v.1),
            ))),
            TokenKind::Semicolon,
            separated(0.., expression, TokenKind::Comma),
            TokenKind::Semicolon,
            separated(0.., expression, TokenKind::Comma),
        )
            .parse_next(i)?;
        ForCondition::Classic {
            initialize: init.map(Box::new),
            condition: cond,
            update: next,
        }
    };
    TokenKind::CloseParen.parse_next(i)?;

    // Braces are optional, so a body is just one statement - which is _usually_ a block of other statements
    let body = statement.parse_next(i)?;
    Ok(Statement::ForIn {
        condition,
        body: Box::new(body),
    })
}

pub(crate) fn function<'i>(i: &mut Tokens<'i>) -> ModalResult<Function<'i>> {
    let name = opt(identifier).parse_next(i)?;
    TokenKind::OpenParen.parse_next(i)?;
    let args = separated(
        0..,
        (identifier, opt(type_name)).map(|(i, _)| i.to_owned()),
        TokenKind::Comma,
    )
    .parse_next(i)?;
    TokenKind::CloseParen.parse_next(i)?;
    opt(type_name).parse_next(i)?;
    TokenKind::OpenBrace.parse_next(i)?;
    let body = statement_list(true).parse_next(i)?;
    TokenKind::CloseBrace.parse_next(i)?;

    Ok(Function { name, args, body })
}

#[cfg(test)]
mod stmt_tests {
    use super::*;
    use crate::ast::{Affix, BinaryOperator, Constant, Expr, UnaryOperator};
    use crate::lexer::tokens::{Keyword, QuoteKind, Token, TokenKind};
    use crate::parser::tests::build_tokens;
    use std::borrow::Cow;
    use winnow::stream::TokenSlice;

    fn parse_stmt<'i>(tokens: &'i [Token<'i>]) -> ModalResult<Statement<'i>> {
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
                value: Some(Expr::Constant(Constant::String(Cow::Borrowed("hi"))))
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
                    name: Box::new(Expr::Constant(Constant::Identifier("foo"))),
                    args: vec![Expr::Constant(Constant::Identifier("a"))]
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
            Ok(Statement::Expr(Expr::Constant(Constant::Identifier("foo"))))
        );
    }

    #[test]
    fn test_skips_leading_newline() {
        let tokens = build_tokens(&[(TokenKind::Newline, "\n"), (TokenKind::Identifier, "bar")]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::Expr(Expr::Constant(Constant::Identifier("bar"))))
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
                Constant::Identifier("a")
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
                Expr::Constant(Constant::Identifier("a")),
                Expr::Constant(Constant::Identifier("b"))
            ]))
        )
    }

    #[test]
    fn test_for_classic() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::For), "for"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Keyword(Keyword::Var), "var"),
            (TokenKind::Identifier, "i"),
            (TokenKind::Operator(Operator::Assign), "="),
            (TokenKind::Identifier, "0"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Identifier, "i"),
            (TokenKind::Operator(Operator::LessThan), "<"),
            (TokenKind::Identifier, "10"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Identifier, "i"),
            (TokenKind::Operator(Operator::Increment), "++"),
            (TokenKind::CloseParen, ")"),
            (TokenKind::OpenBrace, "{"),
            (TokenKind::Identifier, "trace"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "i"),
            (TokenKind::CloseParen, ")"),
            (TokenKind::CloseBrace, "}"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::ForIn {
                condition: ForCondition::Classic {
                    initialize: Some(Box::new(Statement::Declare(vec![Declaration {
                        name: "i".to_string(),
                        value: Some(Expr::Constant(Constant::Identifier("0")))
                    }]))),
                    condition: vec![Expr::BinaryOperator(
                        BinaryOperator::LessThan,
                        Box::new(Expr::Constant(Constant::Identifier("i"))),
                        Box::new(Expr::Constant(Constant::Identifier("10")))
                    )],
                    update: vec![Expr::UnaryOperator(
                        UnaryOperator::Increment(Affix::Postfix),
                        Box::new(Expr::Constant(Constant::Identifier("i")))
                    )]
                },
                body: Box::new(Statement::Block(vec![Statement::Expr(Expr::Call {
                    name: Box::new(Expr::Constant(Constant::Identifier("trace"))),
                    args: vec![Expr::Constant(Constant::Identifier("i"))]
                })]))
            })
        )
    }

    #[test]
    fn test_for_enumerate() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::For), "for"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::Keyword(Keyword::In), "in"),
            (TokenKind::Identifier, "b"),
            (TokenKind::CloseParen, ")"),
            (TokenKind::OpenBrace, "{"),
            (TokenKind::Identifier, "trace"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::CloseParen, ")"),
            (TokenKind::CloseBrace, "}"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::ForIn {
                condition: ForCondition::Enumerate {
                    variable: "a",
                    declare: false,
                    object: Expr::Constant(Constant::Identifier("b"))
                },
                body: Box::new(Statement::Block(vec![Statement::Expr(Expr::Call {
                    name: Box::new(Expr::Constant(Constant::Identifier("trace"))),
                    args: vec![Expr::Constant(Constant::Identifier("a"))]
                })]))
            })
        )
    }

    #[test]
    fn test_if_no_else() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::If), "if"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::CloseParen, ")"),
            (TokenKind::OpenBrace, "{"),
            (TokenKind::Identifier, "trace"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::CloseParen, ")"),
            (TokenKind::CloseBrace, "}"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::If {
                condition: Expr::Constant(Constant::Identifier("a")),
                yes: Box::new(Statement::Block(vec![Statement::Expr(Expr::Call {
                    name: Box::new(Expr::Constant(Constant::Identifier("trace"))),
                    args: vec![Expr::Constant(Constant::Identifier("a"))]
                })])),
                no: None,
            })
        )
    }

    #[test]
    fn test_if_else() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::If), "if"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::CloseParen, ")"),
            (TokenKind::Identifier, "trace"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::CloseParen, ")"),
            (TokenKind::Keyword(Keyword::Else), "else"),
            (TokenKind::Identifier, "trace"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "b"),
            (TokenKind::CloseParen, ")"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::If {
                condition: Expr::Constant(Constant::Identifier("a")),
                yes: Box::new(Statement::Expr(Expr::Call {
                    name: Box::new(Expr::Constant(Constant::Identifier("trace"))),
                    args: vec![Expr::Constant(Constant::Identifier("a"))]
                })),
                no: Some(Box::new(Statement::Expr(Expr::Call {
                    name: Box::new(Expr::Constant(Constant::Identifier("trace"))),
                    args: vec![Expr::Constant(Constant::Identifier("b"))]
                })))
            })
        )
    }

    #[test]
    fn test_break() {
        let tokens = build_tokens(&[(TokenKind::Keyword(Keyword::Break), "break")]);
        assert_eq!(parse_stmt(&tokens), Ok(Statement::Break))
    }

    #[test]
    fn test_continue() {
        let tokens = build_tokens(&[(TokenKind::Keyword(Keyword::Continue), "continue")]);
        assert_eq!(parse_stmt(&tokens), Ok(Statement::Continue))
    }
}

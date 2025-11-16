use crate::lexer::operator::Operator;
use crate::lexer::tokens::{Keyword, TokenKind};
use crate::parser::expression::{Expr, expr_list, expression};
use crate::parser::{Tokens, identifier, skip_newlines};
use serde::Serialize;
use winnow::combinator::{alt, cond, cut_err, opt, peek, separated};
use winnow::error::{ContextError, ErrMode, StrContext};
use winnow::stream::Stream;
use winnow::token::{any, take_while};
use winnow::{ModalResult, Parser};

#[derive(Debug, Clone, Serialize, PartialEq)]
pub(crate) enum Statement {
    Declare(Vec<Declaration>),
    Return(Vec<Expr>),
    Expr(Expr),
    Block(Vec<Statement>),
    ForIn {
        condition: ForCondition,
        body: Box<Statement>,
    },
    If {
        condition: Expr,
        yes: Box<Statement>,
        no: Option<Box<Statement>>,
    },
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub(crate) enum ForCondition {
    Enumerate {
        variable: String,
        declare: bool,
        object: Expr,
    },
    Classic {
        initialize: Option<Box<Statement>>,
        // This is technically incorrect, we treat `a++, b++` as two different expressions, but they should be one (and we'd have a `Next(a, b)` expression)
        // Unfortunately that seems difficult to implement _correctly_, so this is a good stopgap (did anyone even use `,` in regular code, outside for loops?)
        condition: Vec<Expr>,
        update: Vec<Expr>,
    },
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
    skip_newlines(i)?;
    let declarations = separated(0.., declaration, TokenKind::Comma).parse_next(i)?;

    Ok(Statement::Declare(declarations))
}

fn declaration(i: &mut Tokens<'_>) -> ModalResult<Declaration> {
    let name = cut_err(identifier)
        .context(StrContext::Label("variable name"))
        .parse_next(i)?;
    let equals = cut_err(opt(TokenKind::Operator(Operator::Assign)))
        .parse_next(i)?
        .is_some();
    let value = cond(equals, expression).parse_next(i)?;

    Ok(Declaration { name, value })
}

fn if_else(i: &mut Tokens<'_>) -> ModalResult<Statement> {
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

fn for_loop(i: &mut Tokens<'_>) -> ModalResult<Statement> {
    TokenKind::OpenParen.parse_next(i)?;
    let condition = if let Some((var, name, _)) = opt((
        opt(TokenKind::Keyword(Keyword::Var)),
        identifier,
        TokenKind::Keyword(Keyword::In),
    ))
    .parse_next(i)?
    {
        ForCondition::Enumerate {
            variable: name.to_string(),
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

pub(crate) fn function(i: &mut Tokens<'_>) -> ModalResult<Function> {
    let name = opt(identifier).parse_next(i)?;
    TokenKind::OpenParen.parse_next(i)?;
    let args = separated(0.., identifier, TokenKind::Comma).parse_next(i)?;
    TokenKind::CloseParen.parse_next(i)?;
    TokenKind::OpenBrace.parse_next(i)?;
    let body = statement_list(true).parse_next(i)?;
    TokenKind::CloseBrace.parse_next(i)?;

    Ok(Function { name, args, body })
}

#[cfg(test)]
mod stmt_tests {
    use super::*;
    use crate::lexer::tokens::{Keyword, QuoteKind, Token, TokenKind};
    use crate::parser::expression::{Constant, Expr};
    use crate::parser::operator::{Affix, BinaryOperator, UnaryOperator};
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
                        value: Some(Expr::Constant(Constant::Identifier("0".to_string())))
                    }]))),
                    condition: vec![Expr::BinaryOperator(
                        BinaryOperator::LessThan,
                        Box::new(Expr::Constant(Constant::Identifier("i".to_string()))),
                        Box::new(Expr::Constant(Constant::Identifier("10".to_string())))
                    )],
                    update: vec![Expr::UnaryOperator(
                        UnaryOperator::Increment(Affix::Postfix),
                        Box::new(Expr::Constant(Constant::Identifier("i".to_string())))
                    )]
                },
                body: Box::new(Statement::Block(vec![Statement::Expr(Expr::Call {
                    name: Box::new(Expr::Constant(Constant::Identifier("trace".to_string()))),
                    args: vec![Expr::Constant(Constant::Identifier("i".to_string()))]
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
                    variable: "a".to_string(),
                    declare: false,
                    object: Expr::Constant(Constant::Identifier("b".to_string()))
                },
                body: Box::new(Statement::Block(vec![Statement::Expr(Expr::Call {
                    name: Box::new(Expr::Constant(Constant::Identifier("trace".to_string()))),
                    args: vec![Expr::Constant(Constant::Identifier("a".to_string()))]
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
                condition: Expr::Constant(Constant::Identifier("a".to_string())),
                yes: Box::new(Statement::Block(vec![Statement::Expr(Expr::Call {
                    name: Box::new(Expr::Constant(Constant::Identifier("trace".to_string()))),
                    args: vec![Expr::Constant(Constant::Identifier("a".to_string()))]
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
                condition: Expr::Constant(Constant::Identifier("a".to_string())),
                yes: Box::new(Statement::Expr(Expr::Call {
                    name: Box::new(Expr::Constant(Constant::Identifier("trace".to_string()))),
                    args: vec![Expr::Constant(Constant::Identifier("a".to_string()))]
                })),
                no: Some(Box::new(Statement::Expr(Expr::Call {
                    name: Box::new(Expr::Constant(Constant::Identifier("trace".to_string()))),
                    args: vec![Expr::Constant(Constant::Identifier("b".to_string()))]
                })))
            })
        )
    }
}

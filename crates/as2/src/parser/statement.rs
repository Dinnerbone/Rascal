use crate::ast::{Catch, Declaration, ForCondition, Function, StatementKind, TryCatch};
use crate::lexer::operator::Operator;
use crate::lexer::tokens::{Keyword, TokenKind};
use crate::parser::expression::{expr_list, expression, type_name};
use crate::parser::{Tokens, identifier, skip_newlines};
use rascal_common::span::{Span, Spanned};
use winnow::combinator::{alt, cond, cut_err, opt, peek, separated};
use winnow::error::{ContextError, ErrMode, StrContext};
use winnow::stream::Stream;
use winnow::token::{any, take_while};
use winnow::{ModalResult, Parser};

pub(crate) fn statement<'i>(i: &mut Tokens<'i>) -> ModalResult<StatementKind<'i>> {
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
            StatementKind::Return(values)
        }
        TokenKind::Keyword(Keyword::Throw) => {
            let paren = opt(TokenKind::OpenParen).parse_next(i)?;
            let values = expr_list.parse_next(i)?;
            if paren.is_some() {
                TokenKind::CloseParen.parse_next(i)?;
            }
            StatementKind::Throw(values)
        }
        TokenKind::Keyword(Keyword::For) => for_loop
            .context(StrContext::Label("for loop"))
            .parse_next(i)?,
        TokenKind::Keyword(Keyword::If) => if_else
            .context(StrContext::Label("if statement"))
            .parse_next(i)?,
        TokenKind::Keyword(Keyword::Break) => StatementKind::Break,
        TokenKind::Keyword(Keyword::Continue) => StatementKind::Continue,
        TokenKind::Keyword(Keyword::Try) => try_catch_finally.parse_next(i)?,
        TokenKind::Keyword(Keyword::IfFrameLoaded) => if_frame_loaded.parse_next(i)?,
        TokenKind::Keyword(Keyword::TellTarget) => tell_target.parse_next(i)?,
        TokenKind::Keyword(Keyword::While) => while_loop.parse_next(i)?,
        TokenKind::OpenBrace => {
            let statements = statement_list(true).parse_next(i)?;
            TokenKind::CloseBrace.parse_next(i)?;
            StatementKind::Block(statements)
        }
        TokenKind::PCode => StatementKind::InlinePCode(token.raw),
        _ => {
            i.reset(&checkpoint);
            expression.parse_next(i).map(StatementKind::Expr)?
        }
    };

    Ok(result)
}

pub(crate) fn statement_list<'i>(
    is_block: bool,
) -> impl Parser<Tokens<'i>, Vec<StatementKind<'i>>, ErrMode<ContextError>> {
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

fn declaration_list<'i>(i: &mut Tokens<'i>) -> ModalResult<StatementKind<'i>> {
    skip_newlines(i)?;
    let declarations = separated(0.., declaration, TokenKind::Comma).parse_next(i)?;

    Ok(StatementKind::Declare(declarations))
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
        name: name.value,
        value,
    })
}

fn if_else<'i>(i: &mut Tokens<'i>) -> ModalResult<StatementKind<'i>> {
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

    Ok(StatementKind::If { condition, yes, no })
}

fn for_loop<'i>(i: &mut Tokens<'i>) -> ModalResult<StatementKind<'i>> {
    TokenKind::OpenParen.parse_next(i)?;
    let condition = if let Some((var, name, _)) = opt((
        opt(TokenKind::Keyword(Keyword::Var)),
        identifier,
        TokenKind::Keyword(Keyword::In),
    ))
    .parse_next(i)?
    {
        ForCondition::Enumerate {
            variable: name.value,
            declare: var.is_some(),
            object: expression.parse_next(i)?,
        }
    } else {
        let (init, _, cond, _, next) = (
            opt(alt((
                expression.map(StatementKind::Expr),
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
    Ok(StatementKind::ForIn {
        condition,
        body: Box::new(body),
    })
}

pub(crate) fn if_frame_loaded<'i>(i: &mut Tokens<'i>) -> ModalResult<StatementKind<'i>> {
    TokenKind::OpenParen.parse_next(i)?;
    let (scene, frame) = alt((
        (expression, TokenKind::Comma, expression).map(|(s, _, f)| (Some(s), f)),
        (expression).map(|f| (None, f)),
    ))
    .parse_next(i)?;
    TokenKind::CloseParen.parse_next(i)?;
    let body = statement.parse_next(i)?;

    Ok(StatementKind::WaitForFrame {
        frame,
        scene,
        if_loaded: Box::new(body),
    })
}

pub(crate) fn tell_target<'i>(i: &mut Tokens<'i>) -> ModalResult<StatementKind<'i>> {
    TokenKind::OpenParen.parse_next(i)?;
    let target = expression.parse_next(i)?;
    TokenKind::CloseParen.parse_next(i)?;
    let body = opt(statement)
        .parse_next(i)?
        .unwrap_or_else(|| StatementKind::Block(vec![]));

    Ok(StatementKind::TellTarget {
        target,
        body: Box::new(body),
    })
}

pub(crate) fn while_loop<'i>(i: &mut Tokens<'i>) -> ModalResult<StatementKind<'i>> {
    TokenKind::OpenParen.parse_next(i)?;
    let condition = expression.parse_next(i)?;
    TokenKind::CloseParen.parse_next(i)?;
    let body = statement.parse_next(i)?;

    Ok(StatementKind::While {
        condition,
        body: Box::new(body),
    })
}

pub(crate) fn try_catch_finally<'i>(i: &mut Tokens<'i>) -> ModalResult<StatementKind<'i>> {
    TokenKind::OpenBrace.parse_next(i)?;
    let try_body = statement_list(true).parse_next(i)?;
    TokenKind::CloseBrace.parse_next(i)?;
    let mut typed_catches = vec![];
    let mut catch_all = None;

    while opt(TokenKind::Keyword(Keyword::Catch))
        .parse_next(i)?
        .is_some()
    {
        TokenKind::OpenParen.parse_next(i)?;
        let name = identifier.parse_next(i)?;
        let type_name = opt(type_name).parse_next(i)?;
        TokenKind::CloseParen.parse_next(i)?;
        TokenKind::OpenBrace.parse_next(i)?;
        let body = statement_list(true).parse_next(i)?;
        TokenKind::CloseBrace.parse_next(i)?;

        if let Some(type_name) = type_name {
            typed_catches.push((type_name, Catch { name, body }));
        } else {
            catch_all = Some(Catch { name, body });
            break;
        }
    }

    let finally = if opt(TokenKind::Keyword(Keyword::Finally))
        .parse_next(i)?
        .is_some()
    {
        TokenKind::OpenBrace.parse_next(i)?;
        let body = statement_list(true).parse_next(i)?;
        TokenKind::CloseBrace.parse_next(i)?;
        body
    } else {
        vec![]
    };

    Ok(StatementKind::Try(TryCatch {
        try_body,
        typed_catches,
        catch_all,
        finally,
    }))
}

pub(crate) fn function<'i>(i: &mut Tokens<'i>) -> ModalResult<Spanned<Function<'i>>> {
    let start = TokenKind::Keyword(Keyword::Function).parse_next(i)?.span;
    let name = opt(identifier).parse_next(i)?;
    TokenKind::OpenParen.parse_next(i)?;
    let args = separated(
        0..,
        (identifier, opt(type_name)).map(|(i, _)| i.value),
        TokenKind::Comma,
    )
    .parse_next(i)?;
    TokenKind::CloseParen.parse_next(i)?;
    opt(type_name).parse_next(i)?;
    TokenKind::OpenBrace.parse_next(i)?;
    let body = statement_list(true).parse_next(i)?;
    let end = TokenKind::CloseBrace.parse_next(i)?.span;

    Ok(Spanned::new(
        Span::encompassing(start, end),
        Function {
            name: name.map(|n| n.value),
            args,
            body,
        },
    ))
}

#[cfg(test)]
mod stmt_tests {
    use super::*;
    use crate::ast::{
        Affix, BinaryOperator, ConstantKind, Expr, ExprKind, TryCatch, UnaryOperator,
    };
    use crate::lexer::tokens::{Keyword, QuoteKind, Token, TokenKind};
    use crate::parser::tests::build_tokens;
    use std::borrow::Cow;
    use winnow::stream::TokenSlice;

    fn parse_stmt<'i>(tokens: &'i [Token<'i>]) -> ModalResult<StatementKind<'i>> {
        statement(&mut TokenSlice::new(tokens))
    }

    // Helpers to build Expr values with default spans for expectations
    fn ex<'i>(k: ExprKind<'i>) -> Expr<'i> {
        Spanned::new(Span::default(), k)
    }

    fn id<'i>(name: &'i str) -> Expr<'i> {
        ex(ExprKind::Constant(ConstantKind::Identifier(name)))
    }

    fn s<'i>(val: &'i str) -> Expr<'i> {
        ex(ExprKind::Constant(ConstantKind::String(Cow::Borrowed(val))))
    }

    #[test]
    fn test_declare_no_value() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Var), "var"),
            (TokenKind::Identifier, "x"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(StatementKind::Declare(vec![Declaration {
                name: "x",
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
            Ok(StatementKind::Declare(vec![Declaration {
                name: "x",
                value: Some(s("hi"))
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
            Ok(StatementKind::Declare(vec![Declaration {
                name: "x",
                value: Some(ex(ExprKind::Call {
                    name: Box::new(id("foo")),
                    args: vec![id("a")]
                }))
            }]))
        );
    }

    #[test]
    fn test_wraps_expr() {
        let tokens = build_tokens(&[(TokenKind::Identifier, "foo")]);
        let got = parse_stmt(&tokens);
        assert_eq!(got, Ok(StatementKind::Expr(id("foo"))));
    }

    #[test]
    fn test_skips_leading_newline() {
        let tokens = build_tokens(&[(TokenKind::Newline, "\n"), (TokenKind::Identifier, "bar")]);
        assert_eq!(parse_stmt(&tokens), Ok(StatementKind::Expr(id("bar"))));
    }

    #[test]
    fn test_return_no_value() {
        let tokens = build_tokens(&[(TokenKind::Keyword(Keyword::Return), "return")]);
        assert_eq!(parse_stmt(&tokens), Ok(StatementKind::Return(vec![])))
    }

    #[test]
    fn test_return_regular_value() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Return), "return"),
            (TokenKind::Identifier, "a"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(StatementKind::Return(vec![id("a")]))
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
            Ok(StatementKind::Return(vec![id("a"), id("b")]))
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
            Ok(StatementKind::ForIn {
                condition: ForCondition::Classic {
                    initialize: Some(Box::new(StatementKind::Declare(vec![Declaration {
                        name: "i",
                        value: Some(id("0"))
                    }]))),
                    condition: vec![ex(ExprKind::BinaryOperator(
                        BinaryOperator::LessThan,
                        Box::new(id("i")),
                        Box::new(id("10"))
                    ))],
                    update: vec![ex(ExprKind::UnaryOperator(
                        UnaryOperator::Increment(Affix::Postfix),
                        Box::new(id("i"))
                    ))]
                },
                body: Box::new(StatementKind::Block(vec![StatementKind::Expr(ex(
                    ExprKind::Call {
                        name: Box::new(id("trace")),
                        args: vec![id("i")]
                    }
                ))]))
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
            Ok(StatementKind::ForIn {
                condition: ForCondition::Enumerate {
                    variable: "a",
                    declare: false,
                    object: id("b")
                },
                body: Box::new(StatementKind::Block(vec![StatementKind::Expr(ex(
                    ExprKind::Call {
                        name: Box::new(id("trace")),
                        args: vec![id("a")]
                    }
                ))]))
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
            Ok(StatementKind::If {
                condition: id("a"),
                yes: Box::new(StatementKind::Block(vec![StatementKind::Expr(ex(
                    ExprKind::Call {
                        name: Box::new(id("trace")),
                        args: vec![id("a")]
                    }
                ))])),
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
            Ok(StatementKind::If {
                condition: id("a"),
                yes: Box::new(StatementKind::Expr(ex(ExprKind::Call {
                    name: Box::new(id("trace")),
                    args: vec![id("a")]
                }))),
                no: Some(Box::new(StatementKind::Expr(ex(ExprKind::Call {
                    name: Box::new(id("trace")),
                    args: vec![id("b")]
                }))))
            })
        )
    }

    #[test]
    fn test_throw_regular() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Throw), "throw"),
            (TokenKind::Identifier, "a"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(StatementKind::Throw(vec![ex(ExprKind::Constant(
                ConstantKind::Identifier("a")
            ))]))
        )
    }

    #[test]
    fn test_throw_multi_args_no_paren() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Throw), "throw"),
            (TokenKind::Identifier, "a"),
            (TokenKind::Comma, ","),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(StatementKind::Throw(vec![
                ex(ExprKind::Constant(ConstantKind::Identifier("a"))),
                ex(ExprKind::Constant(ConstantKind::Identifier("b")))
            ]))
        )
    }

    #[test]
    fn test_throw_multi_args_with_paren() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Throw), "throw"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::Comma, ","),
            (TokenKind::Identifier, "b"),
            (TokenKind::CloseParen, ")"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(StatementKind::Throw(vec![
                ex(ExprKind::Constant(ConstantKind::Identifier("a"))),
                ex(ExprKind::Constant(ConstantKind::Identifier("b")))
            ]))
        )
    }

    #[test]
    fn test_break() {
        let tokens = build_tokens(&[(TokenKind::Keyword(Keyword::Break), "break")]);
        assert_eq!(parse_stmt(&tokens), Ok(StatementKind::Break))
    }

    #[test]
    fn test_continue() {
        let tokens = build_tokens(&[(TokenKind::Keyword(Keyword::Continue), "continue")]);
        assert_eq!(parse_stmt(&tokens), Ok(StatementKind::Continue))
    }

    #[test]
    fn test_try_catch_finally() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Try), "try"),
            (TokenKind::OpenBrace, "{"),
            (TokenKind::CloseBrace, "}"),
            (TokenKind::Keyword(Keyword::Catch), "catch"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "e"),
            (TokenKind::CloseParen, ")"),
            (TokenKind::OpenBrace, "{"),
            (TokenKind::CloseBrace, "}"),
            (TokenKind::Keyword(Keyword::Finally), "finally"),
            (TokenKind::OpenBrace, "{"),
            (TokenKind::CloseBrace, "}"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(StatementKind::Try(TryCatch {
                try_body: vec![],
                typed_catches: vec![],
                catch_all: Some(Catch {
                    name: Spanned::new(Span::default(), "e"),
                    body: vec![]
                }),
                finally: vec![]
            }))
        )
    }

    #[test]
    fn test_if_frame_loaded() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::IfFrameLoaded), "ifFrameLoaded"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Integer, "123"),
            (TokenKind::CloseParen, ")"),
            (TokenKind::OpenBrace, "{"),
            (TokenKind::CloseBrace, "}"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(StatementKind::WaitForFrame {
                frame: ex(ExprKind::Constant(ConstantKind::Integer(123))),
                scene: None,
                if_loaded: Box::new(StatementKind::Block(vec![])),
            })
        )
    }

    #[test]
    fn test_if_frame_loaded_with_scene() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::IfFrameLoaded), "ifFrameLoaded"),
            (TokenKind::OpenParen, "("),
            (TokenKind::String(QuoteKind::Double), "scene"),
            (TokenKind::Comma, ","),
            (TokenKind::Integer, "123"),
            (TokenKind::CloseParen, ")"),
            (TokenKind::Keyword(Keyword::Return), "return"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(StatementKind::WaitForFrame {
                frame: ex(ExprKind::Constant(ConstantKind::Integer(123))),
                scene: Some(ex(ExprKind::Constant(ConstantKind::String(Cow::Borrowed(
                    "scene"
                ))))),
                if_loaded: Box::new(StatementKind::Return(vec![])),
            })
        )
    }

    #[test]
    fn test_tell_target() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::TellTarget), "tellTarget"),
            (TokenKind::OpenParen, "("),
            (TokenKind::String(QuoteKind::Double), "target"),
            (TokenKind::CloseParen, ")"),
            (TokenKind::OpenBrace, "{"),
            (TokenKind::CloseBrace, "}"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(StatementKind::TellTarget {
                target: s("target"),
                body: Box::new(StatementKind::Block(vec![]))
            })
        )
    }

    #[test]
    fn test_while() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::While), "while"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::CloseParen, ")"),
            (TokenKind::Keyword(Keyword::Continue), "continue"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(StatementKind::While {
                condition: id("a"),
                body: Box::new(StatementKind::Continue)
            })
        )
    }
}

use crate::ast::{
    Catch, Declaration, ForCondition, Function, FunctionArgument, FunctionSignature, Statement,
    StatementKind, SwitchElement, TryCatch,
};
use crate::lexer::operator::Operator;
use crate::lexer::tokens::{Keyword, TokenKind};
use crate::parser::class::class;
use crate::parser::expression::{expr_list, expression, type_name};
use crate::parser::{Tokens, identifier, skip_newlines};
use rascal_common::span::{Span, Spanned};
use winnow::combinator::{alt, cond, cut_err, fail, opt, peek, separated};
use winnow::error::{ContextError, ErrMode, StrContext, StrContextValue};
use winnow::stream::Stream;
use winnow::token::{any, take_while};
use winnow::{ModalResult, Parser};

pub(crate) fn statement<'i>(i: &mut Tokens<'i>) -> ModalResult<Statement<'i>> {
    let checkpoint = i.checkpoint();
    skip_newlines(i)?;
    let token = any.parse_next(i)?;
    let start = token.span;
    let result = match token.kind {
        TokenKind::Keyword(Keyword::Var) => {
            let declarations = declaration_list
                .context(StrContext::Label("declaration"))
                .parse_next(i)?;
            let end = declarations.last().map(|d| d.span).unwrap_or(start);
            Statement::new(
                Span::encompassing(start, end),
                StatementKind::Declare(declarations),
            )
        }
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
            let end = values.last().map(|d| d.span).unwrap_or(start);
            Statement::new(
                Span::encompassing(start, end),
                StatementKind::Return(values),
            )
        }
        TokenKind::Keyword(Keyword::Throw) => {
            let paren = opt(TokenKind::OpenParen).parse_next(i)?;
            let values = expr_list.parse_next(i)?;
            let end = if paren.is_some() {
                TokenKind::CloseParen.parse_next(i)?.span
            } else {
                values.last().map(|d| d.span).unwrap_or(start)
            };

            Statement::new(Span::encompassing(start, end), StatementKind::Throw(values))
        }
        TokenKind::Keyword(Keyword::For) => {
            let result = for_loop
                .context(StrContext::Label("for loop"))
                .parse_next(i)?;
            Statement::new(Span::encompassing(start, result.span), result.value)
        }
        TokenKind::Keyword(Keyword::If) => {
            let result = if_else
                .context(StrContext::Label("if statement"))
                .parse_next(i)?;
            Statement::new(Span::encompassing(start, result.span), result.value)
        }
        TokenKind::Keyword(Keyword::Break) => Statement::new(start, StatementKind::Break),
        TokenKind::Keyword(Keyword::Continue) => Statement::new(start, StatementKind::Continue),
        TokenKind::Keyword(Keyword::Try) => {
            let result = try_catch_finally.parse_next(i)?;
            Statement::new(Span::encompassing(start, result.span), result.value)
        }
        TokenKind::Keyword(Keyword::IfFrameLoaded) => {
            let result = if_frame_loaded.parse_next(i)?;
            Statement::new(Span::encompassing(start, result.span), result.value)
        }
        TokenKind::Keyword(Keyword::TellTarget) => {
            let result = tell_target.parse_next(i)?;
            Statement::new(Span::encompassing(start, result.span), result.value)
        }
        TokenKind::Keyword(Keyword::Interface) => {
            let result = interface.parse_next(i)?;
            Statement::new(Span::encompassing(start, result.span), result.value)
        }
        TokenKind::Keyword(Keyword::Class) => {
            let result = class.parse_next(i)?;
            Statement::new(Span::encompassing(start, result.span), result.value)
        }
        TokenKind::Keyword(Keyword::While) => {
            let result = while_loop.parse_next(i)?;
            Statement::new(Span::encompassing(start, result.span), result.value)
        }
        TokenKind::Keyword(Keyword::With) => {
            let result = with.parse_next(i)?;
            Statement::new(Span::encompassing(start, result.span), result.value)
        }
        TokenKind::Keyword(Keyword::Switch) => {
            let result = switch.parse_next(i)?;
            Statement::new(Span::encompassing(start, result.span), result.value)
        }
        TokenKind::Keyword(Keyword::Import) => {
            let result = import.parse_next(i)?;
            Statement::new(Span::encompassing(start, result.span), result.value)
        }
        TokenKind::OpenBrace => {
            let statements = statement_list(true).parse_next(i)?;
            let end = TokenKind::CloseBrace.parse_next(i)?.span;
            Statement::new(
                Span::encompassing(start, end),
                StatementKind::Block(statements),
            )
        }
        TokenKind::PCode => Statement::new(start, StatementKind::InlinePCode(token.raw)),
        _ => {
            i.reset(&checkpoint);
            let expr = expression.parse_next(i)?;
            Statement::new(expr.span, StatementKind::Expr(expr))
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

fn declaration_list<'i>(i: &mut Tokens<'i>) -> ModalResult<Vec<Spanned<Declaration<'i>>>> {
    skip_newlines(i)?;
    let declarations = separated(0.., declaration, TokenKind::Comma).parse_next(i)?;

    Ok(declarations)
}

fn declaration<'i>(i: &mut Tokens<'i>) -> ModalResult<Spanned<Declaration<'i>>> {
    let name = cut_err(identifier)
        .context(StrContext::Label("variable name"))
        .parse_next(i)?;
    let start = name.span;
    let type_name = opt(type_name).parse_next(i)?;
    let equals = cut_err(opt(TokenKind::Operator(Operator::Assign)))
        .parse_next(i)?
        .is_some();
    let value = cond(equals, expression).parse_next(i)?;
    let end = value
        .as_ref()
        .map(|v| v.span)
        .or_else(|| type_name.as_ref().map(|t| t.span))
        .unwrap_or(start);

    Ok(Spanned::new(
        Span::encompassing(start, end),
        Declaration {
            name: name.value,
            value,
            type_name,
        },
    ))
}

fn interface<'i>(i: &mut Tokens<'i>) -> ModalResult<Statement<'i>> {
    let name = identifier.parse_next(i)?;
    let extends =
        opt((TokenKind::Keyword(Keyword::Extends), identifier).map(|(_, id)| id)).parse_next(i)?;
    TokenKind::OpenBrace.parse_next(i)?;

    skip_newlines.parse_next(i)?;
    let mut body = vec![];
    loop {
        let next = peek(any).parse_next(i)?;
        match next.kind {
            TokenKind::CloseBrace => break,
            TokenKind::Semicolon | TokenKind::Newline => {
                any.parse_next(i)?;
            }
            TokenKind::Keyword(Keyword::Function) => {
                body.push(function_signature.parse_next(i)?);
            }
            _ => {
                fail.context(StrContext::Expected(StrContextValue::StringLiteral(
                    "function",
                )))
                .parse_next(i)?;
                unreachable!()
            }
        }
        skip_newlines.parse_next(i)?;
    }
    let end = TokenKind::CloseBrace.parse_next(i)?.span;

    Ok(Statement::new(
        Span::encompassing(name.span, end),
        StatementKind::Interface {
            name,
            extends,
            body,
        },
    ))
}

fn if_else<'i>(i: &mut Tokens<'i>) -> ModalResult<Statement<'i>> {
    let start = TokenKind::OpenParen.parse_next(i)?.span;
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
    let end = no.as_ref().map(|s| s.span).unwrap_or(yes.span);

    Ok(Statement::new(
        Span::encompassing(start, end),
        StatementKind::If { condition, yes, no },
    ))
}

fn for_loop<'i>(i: &mut Tokens<'i>) -> ModalResult<Statement<'i>> {
    let start = TokenKind::OpenParen.parse_next(i)?.span;
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
                expression.map(|expr| Statement::new(expr.span, StatementKind::Expr(expr))),
                (TokenKind::Keyword(Keyword::Var), declaration_list).map(|v| {
                    Statement::new(
                        Span::encompassing(
                            v.1.first().map(|d| d.span).unwrap_or_default(),
                            v.1.last().map(|d| d.span).unwrap_or_default(),
                        ),
                        StatementKind::Declare(v.1),
                    )
                }),
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
    Ok(Statement::new(
        Span::encompassing(start, body.span),
        StatementKind::ForIn {
            condition,
            body: Box::new(body),
        },
    ))
}

pub(crate) fn if_frame_loaded<'i>(i: &mut Tokens<'i>) -> ModalResult<Statement<'i>> {
    let start = TokenKind::OpenParen.parse_next(i)?.span;
    let (scene, frame) = alt((
        (expression, TokenKind::Comma, expression).map(|(s, _, f)| (Some(s), f)),
        (expression).map(|f| (None, f)),
    ))
    .parse_next(i)?;
    TokenKind::CloseParen.parse_next(i)?;
    let body = statement.parse_next(i)?;

    Ok(Statement::new(
        Span::encompassing(start, body.span),
        StatementKind::WaitForFrame {
            frame,
            scene,
            if_loaded: Box::new(body),
        },
    ))
}

pub(crate) fn tell_target<'i>(i: &mut Tokens<'i>) -> ModalResult<Statement<'i>> {
    let start = TokenKind::OpenParen.parse_next(i)?.span;
    let target = expression.parse_next(i)?;
    let paren_end = TokenKind::CloseParen.parse_next(i)?.span;
    let body = opt(statement)
        .parse_next(i)?
        .unwrap_or_else(|| Statement::new(paren_end, StatementKind::Block(vec![])));

    Ok(Statement::new(
        Span::encompassing(start, body.span),
        StatementKind::TellTarget {
            target,
            body: Box::new(body),
        },
    ))
}

pub(crate) fn while_loop<'i>(i: &mut Tokens<'i>) -> ModalResult<Statement<'i>> {
    let start = TokenKind::OpenParen.parse_next(i)?.span;
    let condition = expression.parse_next(i)?;
    TokenKind::CloseParen.parse_next(i)?;
    let body = statement.parse_next(i)?;

    Ok(Statement::new(
        Span::encompassing(start, body.span),
        StatementKind::While {
            condition,
            body: Box::new(body),
        },
    ))
}

pub(crate) fn with<'i>(i: &mut Tokens<'i>) -> ModalResult<Statement<'i>> {
    let start = TokenKind::OpenParen.parse_next(i)?.span;
    let target = expression.parse_next(i)?;
    TokenKind::CloseParen.parse_next(i)?;
    let body = statement.parse_next(i)?;

    Ok(Statement::new(
        Span::encompassing(start, body.span),
        StatementKind::With {
            target,
            body: Box::new(body),
        },
    ))
}

pub(crate) fn import<'i>(i: &mut Tokens<'i>) -> ModalResult<Statement<'i>> {
    let path: Vec<Spanned<&'i str>> =
        separated(1.., identifier, TokenKind::Period).parse_next(i)?;

    // We know that path must have at least one value, per the limit above
    let start = path.first().unwrap().span;
    let end = path.last().unwrap().span;
    let mut path = path.into_iter().map(|p| p.value).collect::<Vec<_>>();
    let name = path.pop().unwrap();
    Ok(Statement::new(
        Span::encompassing(start, end),
        StatementKind::Import { path, name },
    ))
}

pub(crate) fn switch<'i>(i: &mut Tokens<'i>) -> ModalResult<Statement<'i>> {
    let start = TokenKind::OpenParen.parse_next(i)?.span;
    let target = expression.parse_next(i)?;
    TokenKind::CloseParen.parse_next(i)?;
    let mut elements = vec![];

    TokenKind::OpenBrace.parse_next(i)?;
    skip_newlines(i)?;
    let mut next = peek(any).parse_next(i)?;
    let mut has_default = false;
    while next.kind != TokenKind::CloseBrace {
        match next.kind {
            TokenKind::Keyword(Keyword::Case) => {
                TokenKind::Keyword(Keyword::Case).parse_next(i)?;
                let value = expression
                    .context(StrContext::Label("value"))
                    .parse_next(i)?;
                TokenKind::Colon.parse_next(i)?;
                elements.push(SwitchElement::Case(value));
            }
            TokenKind::Keyword(Keyword::Default) if !has_default => {
                TokenKind::Keyword(Keyword::Default).parse_next(i)?;
                TokenKind::Colon.parse_next(i)?;
                elements.push(SwitchElement::Default);
                has_default = true;
            }
            TokenKind::Newline | TokenKind::Semicolon => {
                any.parse_next(i)?;
            }
            _ => {
                let statement = if has_default {
                    statement
                        .context(StrContext::Expected(StrContextValue::StringLiteral(
                            Keyword::Case.text(),
                        )))
                        .context(StrContext::Expected(StrContextValue::Description(
                            "statement",
                        )))
                        .parse_next(i)?
                } else {
                    statement
                        .context(StrContext::Expected(StrContextValue::StringLiteral(
                            Keyword::Case.text(),
                        )))
                        .context(StrContext::Expected(StrContextValue::Description(
                            "statement",
                        )))
                        .context(StrContext::Expected(StrContextValue::StringLiteral(
                            Keyword::Default.text(),
                        )))
                        .parse_next(i)?
                };
                elements.push(SwitchElement::Statement(statement));
            }
        }
        next = peek(any).parse_next(i)?;
    }
    let end = TokenKind::CloseBrace.parse_next(i)?.span;

    Ok(Statement::new(
        Span::encompassing(start, end),
        StatementKind::Switch { target, elements },
    ))
}

pub(crate) fn try_catch_finally<'i>(i: &mut Tokens<'i>) -> ModalResult<Statement<'i>> {
    let start = TokenKind::OpenBrace.parse_next(i)?.span;
    let try_body = statement_list(true).parse_next(i)?;
    let mut end = TokenKind::CloseBrace.parse_next(i)?.span;
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
        end = TokenKind::CloseBrace.parse_next(i)?.span;

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
        end = TokenKind::CloseBrace.parse_next(i)?.span;
        body
    } else {
        vec![]
    };

    Ok(Statement::new(
        Span::encompassing(start, end),
        StatementKind::Try(TryCatch {
            try_body,
            typed_catches,
            catch_all,
            finally,
        }),
    ))
}

pub(crate) fn function<'i>(i: &mut Tokens<'i>) -> ModalResult<Spanned<Function<'i>>> {
    let signature = function_signature.parse_next(i)?;
    TokenKind::OpenBrace.parse_next(i)?;
    let body = statement_list(true).parse_next(i)?;
    let end = TokenKind::CloseBrace.parse_next(i)?.span;

    Ok(Spanned::new(
        Span::encompassing(signature.span, end),
        Function {
            signature: signature.value,
            body,
        },
    ))
}

pub(crate) fn function_signature<'i>(
    i: &mut Tokens<'i>,
) -> ModalResult<Spanned<FunctionSignature<'i>>> {
    let start = TokenKind::Keyword(Keyword::Function).parse_next(i)?.span;
    let name = opt(identifier).parse_next(i)?;
    TokenKind::OpenParen.parse_next(i)?;
    let args = separated(
        0..,
        (identifier, opt(type_name)).map(|(name, type_name)| FunctionArgument {
            name: name.value,
            type_name,
        }),
        TokenKind::Comma,
    )
    .parse_next(i)?;
    let paren_end = TokenKind::CloseParen
        .context(StrContext::Expected(TokenKind::Comma.expected()))
        .parse_next(i)?
        .span;
    let return_type = opt(type_name).parse_next(i)?;
    let end = return_type.map(|t| t.span).unwrap_or(paren_end);

    Ok(Spanned::new(
        Span::encompassing(start, end),
        FunctionSignature {
            name,
            args,
            return_type,
        },
    ))
}

#[cfg(test)]
mod stmt_tests {
    use super::*;
    use crate::ast::{
        Affix, BinaryOperator, ConstantKind, Expr, ExprKind, SwitchElement, TryCatch, UnaryOperator,
    };
    use crate::lexer::tokens::{Keyword, QuoteKind, Token, TokenKind};
    use crate::parser::tests::build_tokens;
    use std::borrow::Cow;
    use winnow::stream::TokenSlice;

    fn parse_stmt<'i>(tokens: &'i [Token<'i>]) -> ModalResult<Statement<'i>> {
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
            Ok(Statement::new(
                Span::default(),
                StatementKind::Declare(vec![Spanned::new(
                    Span::default(),
                    Declaration {
                        name: "x",
                        value: None,
                        type_name: None,
                    }
                )])
            ))
        );
    }

    #[test]
    fn test_declare_assign_with_type() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Var), "var"),
            (TokenKind::Identifier, "x"),
            (TokenKind::Colon, ":"),
            (TokenKind::Identifier, "String"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::new(
                Span::default(),
                StatementKind::Declare(vec![Spanned::new(
                    Span::default(),
                    Declaration {
                        name: "x",
                        value: None,
                        type_name: Some(Spanned::new(Span::default(), "String")),
                    }
                )])
            ))
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
            Ok(Statement::new(
                Span::default(),
                StatementKind::Declare(vec![Spanned::new(
                    Span::default(),
                    Declaration {
                        name: "x",
                        value: Some(s("hi")),
                        type_name: None,
                    }
                )])
            ))
        );
    }

    #[test]
    fn test_declare_assign_constant_with_type() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Var), "var"),
            (TokenKind::Identifier, "x"),
            (TokenKind::Colon, ":"),
            (TokenKind::Identifier, "String"),
            (TokenKind::Operator(Operator::Assign), "="),
            (TokenKind::String(QuoteKind::Double), "hi"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::new(
                Span::default(),
                StatementKind::Declare(vec![Spanned::new(
                    Span::default(),
                    Declaration {
                        name: "x",
                        value: Some(s("hi")),
                        type_name: Some(Spanned::new(Span::default(), "String")),
                    }
                )])
            ))
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
            Ok(Statement::new(
                Span::default(),
                StatementKind::Declare(vec![Spanned::new(
                    Span::default(),
                    Declaration {
                        name: "x",
                        value: Some(ex(ExprKind::Call {
                            name: Box::new(id("foo")),
                            args: vec![id("a")]
                        })),
                        type_name: None,
                    }
                )])
            ))
        );
    }

    #[test]
    fn test_wraps_expr() {
        let tokens = build_tokens(&[(TokenKind::Identifier, "foo")]);
        let got = parse_stmt(&tokens);
        assert_eq!(
            got,
            Ok(Statement::new(
                Span::default(),
                StatementKind::Expr(id("foo"))
            ))
        );
    }

    #[test]
    fn test_skips_leading_newline() {
        let tokens = build_tokens(&[(TokenKind::Newline, "\n"), (TokenKind::Identifier, "bar")]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::new(
                Span::default(),
                StatementKind::Expr(id("bar"))
            ))
        );
    }

    #[test]
    fn test_return_no_value() {
        let tokens = build_tokens(&[(TokenKind::Keyword(Keyword::Return), "return")]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::new(
                Span::default(),
                StatementKind::Return(vec![])
            ))
        )
    }

    #[test]
    fn test_return_regular_value() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Return), "return"),
            (TokenKind::Identifier, "a"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::new(
                Span::default(),
                StatementKind::Return(vec![id("a")])
            ))
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
            Ok(Statement::new(
                Span::default(),
                StatementKind::Return(vec![id("a"), id("b")])
            ))
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
            Ok(Statement::new(
                Span::default(),
                StatementKind::ForIn {
                    condition: ForCondition::Classic {
                        initialize: Some(Box::new(Statement::new(
                            Span::default(),
                            StatementKind::Declare(vec![Spanned::new(
                                Span::default(),
                                Declaration {
                                    name: "i",
                                    value: Some(id("0")),
                                    type_name: None,
                                }
                            )])
                        ))),
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
                    body: Box::new(Statement::new(
                        Span::default(),
                        StatementKind::Block(vec![Statement::new(
                            Span::default(),
                            StatementKind::Expr(ex(ExprKind::Call {
                                name: Box::new(id("trace")),
                                args: vec![id("i")]
                            }))
                        )])
                    ))
                }
            ))
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
            Ok(Statement::new(
                Span::default(),
                StatementKind::ForIn {
                    condition: ForCondition::Enumerate {
                        variable: "a",
                        declare: false,
                        object: id("b")
                    },
                    body: Box::new(Statement::new(
                        Span::default(),
                        StatementKind::Block(vec![Statement::new(
                            Span::default(),
                            StatementKind::Expr(ex(ExprKind::Call {
                                name: Box::new(id("trace")),
                                args: vec![id("a")]
                            }))
                        )])
                    ))
                }
            ))
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
            Ok(Statement::new(
                Span::default(),
                StatementKind::If {
                    condition: id("a"),
                    yes: Box::new(Statement::new(
                        Span::default(),
                        StatementKind::Block(vec![Statement::new(
                            Span::default(),
                            StatementKind::Expr(ex(ExprKind::Call {
                                name: Box::new(id("trace")),
                                args: vec![id("a")]
                            }))
                        )])
                    )),
                    no: None,
                }
            ))
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
            Ok(Statement::new(
                Span::default(),
                StatementKind::If {
                    condition: id("a"),
                    yes: Box::new(Statement::new(
                        Span::default(),
                        StatementKind::Expr(ex(ExprKind::Call {
                            name: Box::new(id("trace")),
                            args: vec![id("a")]
                        }))
                    )),
                    no: Some(Box::new(Statement::new(
                        Span::default(),
                        StatementKind::Expr(ex(ExprKind::Call {
                            name: Box::new(id("trace")),
                            args: vec![id("b")]
                        }))
                    )))
                }
            ))
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
            Ok(Statement::new(
                Span::default(),
                StatementKind::Throw(vec![ex(ExprKind::Constant(ConstantKind::Identifier("a")))])
            ))
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
            Ok(Statement::new(
                Span::default(),
                StatementKind::Throw(vec![
                    ex(ExprKind::Constant(ConstantKind::Identifier("a"))),
                    ex(ExprKind::Constant(ConstantKind::Identifier("b")))
                ])
            ))
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
            Ok(Statement::new(
                Span::default(),
                StatementKind::Throw(vec![
                    ex(ExprKind::Constant(ConstantKind::Identifier("a"))),
                    ex(ExprKind::Constant(ConstantKind::Identifier("b")))
                ])
            ))
        )
    }

    #[test]
    fn test_break() {
        let tokens = build_tokens(&[(TokenKind::Keyword(Keyword::Break), "break")]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::new(Span::default(), StatementKind::Break))
        )
    }

    #[test]
    fn test_continue() {
        let tokens = build_tokens(&[(TokenKind::Keyword(Keyword::Continue), "continue")]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::new(Span::default(), StatementKind::Continue))
        )
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
            Ok(Statement::new(
                Span::default(),
                StatementKind::Try(TryCatch {
                    try_body: vec![],
                    typed_catches: vec![],
                    catch_all: Some(Catch {
                        name: Spanned::new(Span::default(), "e"),
                        body: vec![]
                    }),
                    finally: vec![]
                })
            ))
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
            Ok(Statement::new(
                Span::default(),
                StatementKind::WaitForFrame {
                    frame: ex(ExprKind::Constant(ConstantKind::Integer(123))),
                    scene: None,
                    if_loaded: Box::new(Statement::new(
                        Span::default(),
                        StatementKind::Block(vec![])
                    )),
                }
            ))
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
            Ok(Statement::new(
                Span::default(),
                StatementKind::WaitForFrame {
                    frame: ex(ExprKind::Constant(ConstantKind::Integer(123))),
                    scene: Some(ex(ExprKind::Constant(ConstantKind::String(Cow::Borrowed(
                        "scene"
                    ))))),
                    if_loaded: Box::new(Statement::new(
                        Span::default(),
                        StatementKind::Return(vec![])
                    )),
                }
            ))
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
            Ok(Statement::new(
                Span::default(),
                StatementKind::TellTarget {
                    target: s("target"),
                    body: Box::new(Statement::new(
                        Span::default(),
                        StatementKind::Block(vec![])
                    ))
                }
            ))
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
            Ok(Statement::new(
                Span::default(),
                StatementKind::While {
                    condition: id("a"),
                    body: Box::new(Statement::new(Span::default(), StatementKind::Continue))
                }
            ))
        )
    }

    #[test]
    fn test_with() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::With), "with"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::CloseParen, ")"),
            (TokenKind::OpenBrace, "{"),
            (TokenKind::Identifier, "_alpha"),
            (TokenKind::Operator(Operator::Assign), "="),
            (TokenKind::Integer, "100"),
            (TokenKind::CloseBrace, "}"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::new(
                Span::default(),
                StatementKind::With {
                    target: id("a"),
                    body: Box::new(Statement::new(
                        Span::default(),
                        StatementKind::Block(vec![Statement::new(
                            Span::default(),
                            StatementKind::Expr(ex(ExprKind::BinaryOperator(
                                BinaryOperator::Assign,
                                Box::new(id("_alpha")),
                                Box::new(ex(ExprKind::Constant(ConstantKind::Integer(100))))
                            )))
                        )])
                    ))
                }
            ))
        )
    }

    #[test]
    fn test_switch() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Switch), "switch"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::CloseParen, ")"),
            (TokenKind::OpenBrace, "{"),
            (TokenKind::Keyword(Keyword::Case), "case"),
            (TokenKind::Integer, "1"),
            (TokenKind::Colon, ":"),
            (TokenKind::Keyword(Keyword::Return), "return"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::Keyword(Keyword::Default), "default"),
            (TokenKind::Colon, ":"),
            (TokenKind::Keyword(Keyword::Throw), "throw"),
            (TokenKind::Identifier, "e"),
            (TokenKind::Semicolon, ";"),
            (TokenKind::CloseBrace, "}"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::new(
                Span::default(),
                StatementKind::Switch {
                    target: id("a"),
                    elements: vec![
                        SwitchElement::Case(ex(ExprKind::Constant(ConstantKind::Integer(1)))),
                        SwitchElement::Statement(Statement::new(
                            Span::default(),
                            StatementKind::Return(vec![])
                        )),
                        SwitchElement::Default,
                        SwitchElement::Statement(Statement::new(
                            Span::default(),
                            StatementKind::Throw(vec![id("e")])
                        )),
                    ]
                }
            ))
        )
    }

    #[test]
    fn test_import() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Import), "import"),
            (TokenKind::Identifier, "foo"),
            (TokenKind::Period, "."),
            (TokenKind::Identifier, "bar"),
            (TokenKind::Period, "."),
            (TokenKind::Identifier, "baz"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::new(
                Span::default(),
                StatementKind::Import {
                    name: "baz",
                    path: vec!["foo", "bar"]
                }
            ))
        )
    }

    #[test]
    fn test_bare_interface() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Interface), "interface"),
            (TokenKind::Identifier, "Foo"),
            (TokenKind::OpenBrace, "{"),
            (TokenKind::CloseBrace, "}"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::new(
                Span::default(),
                StatementKind::Interface {
                    name: Spanned::new(Span::default(), "Foo"),
                    extends: None,
                    body: vec![],
                }
            ))
        )
    }

    #[test]
    fn test_extending_interface() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Interface), "interface"),
            (TokenKind::Identifier, "Foo"),
            (TokenKind::Keyword(Keyword::Extends), "extends"),
            (TokenKind::Identifier, "Bar"),
            (TokenKind::OpenBrace, "{"),
            (TokenKind::CloseBrace, "}"),
        ]);
        assert_eq!(
            parse_stmt(&tokens),
            Ok(Statement::new(
                Span::default(),
                StatementKind::Interface {
                    name: Spanned::new(Span::default(), "Foo"),
                    extends: Some(Spanned::new(Span::default(), "Bar")),
                    body: vec![],
                }
            ))
        )
    }
}

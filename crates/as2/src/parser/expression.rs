use crate::ast::{Affix, BinaryOperator, ConstantKind, Expr, ExprKind, UnaryOperator};
use crate::lexer::operator::Operator;
use crate::lexer::tokens::{Keyword, TokenKind};
use crate::parser::operator::{expr_for_binary_operator, expr_for_unary_operator};
use crate::parser::statement::function;
use crate::parser::{Tokens, identifier, operator, skip_newlines, string};
use rascal_common::span::{Span, Spanned};
use std::borrow::Cow;
use winnow::combinator::{alt, fail, opt, peek, separated};
use winnow::error::ParserError;
use winnow::error::{ContextError, ErrMode, StrContext};
use winnow::token::any;
use winnow::{ModalResult, Parser};

pub(crate) fn rewrite_leftmost_expr<R>(target: &mut Expr, rewriter: R)
where
    R: FnOnce(Expr) -> Expr,
{
    let mut expr: &mut Expr = target;

    loop {
        match expr {
            Expr {
                value: ExprKind::BinaryOperator(_op, left, _right),
                ..
            } => expr = left,
            Expr {
                value: ExprKind::Ternary { condition, .. },
                ..
            } => expr = condition,
            _ => break,
        }
    }

    // Temporarily replaces it with something else, to appease the borrow checker
    *expr = rewriter(std::mem::replace(
        expr,
        Expr {
            span: Span::default(),
            value: ExprKind::Constant(ConstantKind::Integer(0)),
        },
    ));
    let new_expr_span = expr.span;
    target.span = Span::encompassing(target.span, new_expr_span);
}

impl ExprKind<'_> {
    pub(crate) fn can_postfix(&self) -> bool {
        matches!(self, ExprKind::Constant(_))
    }
}

pub(crate) fn expression<'i>(i: &mut Tokens<'i>) -> ModalResult<Expr<'i>> {
    skip_newlines(i)?;
    let token = peek(any).parse_next(i)?;
    let start = token.span;
    match token.kind {
        TokenKind::Operator(Operator::Sub) => {
            TokenKind::Operator(Operator::Sub).parse_next(i)?;
            expression
                .map(|e| expr_for_unary_operator(UnaryOperator::Sub, Box::new(e), start))
                .context(StrContext::Label("expression"))
                .parse_next(i)
        }
        TokenKind::Operator(Operator::Add) => {
            TokenKind::Operator(Operator::Add).parse_next(i)?;
            expression
                .context(StrContext::Label("expression"))
                .parse_next(i)
        }
        TokenKind::Operator(Operator::Increment) => {
            TokenKind::Operator(Operator::Increment).parse_next(i)?;
            expression
                .map(|e| {
                    expr_for_unary_operator(
                        UnaryOperator::Increment(Affix::Prefix),
                        Box::new(e),
                        start,
                    )
                })
                .context(StrContext::Label("expression"))
                .parse_next(i)
        }
        TokenKind::Operator(Operator::Decrement) => {
            TokenKind::Operator(Operator::Decrement).parse_next(i)?;
            expression
                .map(|e| {
                    expr_for_unary_operator(
                        UnaryOperator::Decrement(Affix::Prefix),
                        Box::new(e),
                        start,
                    )
                })
                .context(StrContext::Label("expression"))
                .parse_next(i)
        }
        TokenKind::Operator(Operator::BitNot) => {
            TokenKind::Operator(Operator::BitNot).parse_next(i)?;
            expression
                .map(|e| expr_for_unary_operator(UnaryOperator::BitNot, Box::new(e), start))
                .context(StrContext::Label("expression"))
                .parse_next(i)
        }
        TokenKind::Operator(Operator::LogicalNot) | TokenKind::Keyword(Keyword::Not) => {
            any.parse_next(i)?;
            expression
                .map(|e| expr_for_unary_operator(UnaryOperator::LogicalNot, Box::new(e), start))
                .context(StrContext::Label("expression"))
                .parse_next(i)
        }
        TokenKind::OpenParen => {
            TokenKind::OpenParen.parse_next(i)?;
            let val = expression
                .context(StrContext::Label("expression"))
                .parse_next(i)?;
            let end = TokenKind::CloseParen.parse_next(i)?.span;
            expr_next(Expr::new(
                Span::encompassing(start, end),
                ExprKind::Parenthesis(Box::new(val)),
            ))
            .context(StrContext::Label("expression"))
            .parse_next(i)
        }
        TokenKind::Keyword(Keyword::New) => {
            TokenKind::Keyword(Keyword::New).parse_next(i)?;
            let val = expression.parse_next(i)?;
            let val = if let Expr {
                value: ExprKind::Call { name, args },
                ..
            } = val
            {
                Expr::new(
                    Span::encompassing(start, args.last().map(|e| e.span).unwrap_or(start)),
                    ExprKind::New { name, args },
                )
            } else {
                Expr::new(
                    Span::encompassing(start, val.span),
                    ExprKind::New {
                        name: Box::new(val),
                        args: vec![],
                    },
                )
            };
            expr_next(val)
                .context(StrContext::Label("expression"))
                .parse_next(i)
        }
        TokenKind::Keyword(Keyword::TypeOf) => {
            TokenKind::Keyword(Keyword::TypeOf).parse_next(i)?;
            let expr = if opt(TokenKind::OpenParen).parse_next(i)?.is_some() {
                let values = expr_list.parse_next(i)?;
                let end = TokenKind::CloseParen.parse_next(i)?.span;
                Expr::new(Span::encompassing(start, end), ExprKind::TypeOf(values))
            } else {
                let mut value = expression.parse_next(i)?;
                rewrite_leftmost_expr(&mut value, |expr| {
                    Expr::new(
                        Span::encompassing(start, expr.span),
                        ExprKind::TypeOf(vec![expr]),
                    )
                });
                value
            };
            expr_next(expr)
                .context(StrContext::Label("expression"))
                .parse_next(i)
        }
        TokenKind::Keyword(Keyword::Delete) => {
            TokenKind::Keyword(Keyword::Delete).parse_next(i)?;
            let expr = if opt(TokenKind::OpenParen).parse_next(i)?.is_some() {
                let values = expr_list.parse_next(i)?;
                let end = TokenKind::CloseParen.parse_next(i)?.span;
                Expr::new(Span::encompassing(start, end), ExprKind::Delete(values))
            } else {
                let mut value = expression.parse_next(i)?;
                rewrite_leftmost_expr(&mut value, |expr| {
                    Expr::new(
                        Span::encompassing(start, expr.span),
                        ExprKind::Delete(vec![expr]),
                    )
                });
                value
            };
            expr_next(expr)
                .context(StrContext::Label("expression"))
                .parse_next(i)
        }
        TokenKind::Keyword(Keyword::Void) => {
            TokenKind::Keyword(Keyword::Void).parse_next(i)?;
            let (values, end) = if opt(TokenKind::OpenParen).parse_next(i)?.is_some() {
                let values = expr_list.parse_next(i)?;
                let end = TokenKind::CloseParen.parse_next(i)?.span;
                (values, end)
            } else {
                let value = expression.parse_next(i)?;
                let span = value.span;
                (vec![value], span)
            };
            expr_next(Expr::new(
                Span::encompassing(start, end),
                ExprKind::Void(values),
            ))
            .context(StrContext::Label("expression"))
            .parse_next(i)
        }
        TokenKind::String(_) | TokenKind::Identifier | TokenKind::Float | TokenKind::Integer => {
            let val = constant
                .context(StrContext::Label("constant"))
                .parse_next(i)?;
            expr_next(Expr::new(val.span, ExprKind::Constant(val.value)))
                .context(StrContext::Label("expression"))
                .parse_next(i)
        }
        TokenKind::OpenBrace => {
            TokenKind::OpenBrace.parse_next(i)?;
            let definition = object_definition
                .context(StrContext::Label("object definition"))
                .parse_next(i)?;
            let end = TokenKind::CloseBrace.parse_next(i)?.span;
            expr_next(Expr::new(
                Span::encompassing(start, end),
                ExprKind::InitObject(definition),
            ))
            .context(StrContext::Label("expression"))
            .parse_next(i)
        }
        TokenKind::OpenBracket => {
            TokenKind::OpenBracket.parse_next(i)?;
            let definition = array_definition
                .context(StrContext::Label("array definition"))
                .parse_next(i)?;
            let end = TokenKind::CloseBracket.parse_next(i)?.span;
            expr_next(Expr::new(
                Span::encompassing(start, end),
                ExprKind::InitArray(definition),
            ))
            .context(StrContext::Label("expression"))
            .parse_next(i)
        }
        TokenKind::Keyword(Keyword::Function) => {
            let func = function.parse_next(i)?;
            expr_next(Expr::new(
                Span::encompassing(start, func.span),
                ExprKind::Function(func.value),
            ))
            .context(StrContext::Label("expression"))
            .parse_next(i)
        }
        TokenKind::Keyword(Keyword::Get) => {
            TokenKind::Keyword(Keyword::Get).parse_next(i)?;
            TokenKind::OpenParen.parse_next(i)?;
            let name = expression.parse_next(i)?;
            let end = TokenKind::CloseParen.parse_next(i)?.span;
            expr_next(Expr::new(
                Span::encompassing(start, end),
                ExprKind::GetVariable(Box::new(name)),
            ))
            .context(StrContext::Label("expression"))
            .parse_next(i)
        }
        TokenKind::Keyword(Keyword::Set) => {
            TokenKind::Keyword(Keyword::Set).parse_next(i)?;
            TokenKind::OpenParen.parse_next(i)?;
            let name = expression.parse_next(i)?;
            TokenKind::Comma.parse_next(i)?;
            let value = expression.parse_next(i)?;
            let end = TokenKind::CloseParen.parse_next(i)?.span;
            expr_next(Expr::new(
                Span::encompassing(start, end),
                ExprKind::SetVariable(Box::new(name), Box::new(value)),
            ))
            .context(StrContext::Label("expression"))
            .parse_next(i)
        }
        _ => fail.parse_next(i),
    }
}

fn object_definition<'i>(i: &mut Tokens<'i>) -> ModalResult<Vec<(&'i str, Expr<'i>)>> {
    separated(
        0..,
        (identifier, TokenKind::Colon, expression).map(|(name, _, expr)| (name.value, expr)),
        TokenKind::Comma,
    )
    .parse_next(i)
}

fn array_definition<'i>(i: &mut Tokens<'i>) -> ModalResult<Vec<Expr<'i>>> {
    separated(0.., expression, TokenKind::Comma).parse_next(i)
}

pub(crate) fn type_name<'i>(i: &mut Tokens<'i>) -> ModalResult<Spanned<&'i str>> {
    TokenKind::Colon.parse_next(i)?;
    alt((
        identifier,
        TokenKind::Keyword(Keyword::Void).map(|t| Spanned::new(t.span, "Void")), // It's a valid identifier here
    ))
    .parse_next(i)
}

fn constant<'i>(i: &mut Tokens<'i>) -> ModalResult<Spanned<ConstantKind<'i>>> {
    skip_newlines(i)?;
    let token = peek(any).parse_next(i)?;
    match token.kind {
        TokenKind::String(_) => string
            .map(|s| Spanned::new(s.span, ConstantKind::String(s.value)))
            .context(StrContext::Label("string"))
            .parse_next(i),
        TokenKind::Identifier => identifier
            .map(|s| Spanned::new(s.span, ConstantKind::Identifier(s.value)))
            .context(StrContext::Label("identifier"))
            .parse_next(i),
        TokenKind::Float => float
            .map(|s| Spanned::new(s.span, ConstantKind::Float(s.value)))
            .context(StrContext::Label("float"))
            .parse_next(i),
        TokenKind::Integer => alt((
            integer.map(|s| Spanned::new(s.span, ConstantKind::Integer(s.value))),
            float.map(|s| Spanned::new(s.span, ConstantKind::Float(s.value))),
        ))
        .context(StrContext::Label("integer"))
        .parse_next(i),
        _ => fail.parse_next(i),
    }
}

fn float(i: &mut Tokens<'_>) -> ModalResult<Spanned<f64>> {
    skip_newlines(i)?;
    let token = alt((TokenKind::Float, TokenKind::Integer)).parse_next(i)?;
    let span = token.span;
    Ok(Spanned::new(
        span,
        if let Some(raw) = token.raw.strip_prefix("0x") {
            u32::from_str_radix(raw, 16)
                .map(|i| i as f64)
                .map_err(|_| ParserError::from_input(&raw))
        } else {
            token
                .raw
                .parse::<f64>()
                .map_err(|_| ParserError::from_input(&token.raw))
        }?,
    ))
}

fn integer(i: &mut Tokens<'_>) -> ModalResult<Spanned<i32>> {
    skip_newlines(i)?;
    let token = TokenKind::Integer.parse_next(i)?;
    let span = token.span;
    Ok(Spanned::new(
        span,
        if let Some(raw) = token.raw.strip_prefix("0x") {
            i32::from_str_radix(raw, 16)
        } else {
            token.raw.parse::<i32>()
        }
        .map_err(|_| ParserError::from_input(&token.raw))?,
    ))
}

fn expr_next<'i>(prior: Expr<'i>) -> impl Parser<Tokens<'i>, Expr<'i>, ErrMode<ContextError>> {
    move |i: &mut Tokens<'i>| {
        let prior = prior.clone();
        skip_newlines(i)?;
        if i.is_empty() {
            return Ok(prior);
        }
        let token = peek(any).parse_next(i)?;
        let start = token.span;
        match token.kind {
            TokenKind::OpenParen => {
                TokenKind::OpenParen.parse_next(i)?;
                let args = expr_list
                    .context(StrContext::Label("arguments"))
                    .parse_next(i)?;
                let end = TokenKind::CloseParen.parse_next(i)?.span;
                expr_next(Expr::new(
                    Span::encompassing(prior.span, end),
                    ExprKind::Call {
                        name: Box::new(prior),
                        args,
                    },
                ))
                .parse_next(i)
            }
            TokenKind::Operator(Operator::Increment) if prior.can_postfix() => {
                TokenKind::Operator(Operator::Increment).parse_next(i)?;
                expr_next(Expr::new(
                    start,
                    ExprKind::UnaryOperator(
                        UnaryOperator::Increment(Affix::Postfix),
                        Box::new(prior),
                    ),
                ))
                .parse_next(i)
            }
            TokenKind::Operator(Operator::Decrement) if prior.can_postfix() => {
                TokenKind::Operator(Operator::Decrement).parse_next(i)?;
                expr_next(Expr::new(
                    start,
                    ExprKind::UnaryOperator(
                        UnaryOperator::Decrement(Affix::Postfix),
                        Box::new(prior),
                    ),
                ))
                .parse_next(i)
            }
            TokenKind::Operator(_) => {
                let op = operator::binary_operator.parse_next(i)?;
                expression
                    .parse_next(i)
                    .map(|next| expr_for_binary_operator(op, Box::new(prior), Box::new(next)))
            }
            TokenKind::Period => {
                TokenKind::Period.parse_next(i)?;
                let name = identifier.parse_next(i)?;
                expr_next(Expr::new(
                    Span::encompassing(start, name.span),
                    ExprKind::Field(
                        Box::new(prior),
                        Box::new(Expr::new(
                            name.span,
                            ExprKind::Constant(ConstantKind::String(Cow::Borrowed(name.value))),
                        )),
                    ),
                ))
                .parse_next(i)
            }
            TokenKind::Question => {
                TokenKind::Question.parse_next(i)?;
                let yes = expression.parse_next(i)?;
                TokenKind::Colon.parse_next(i)?;
                let no = expression.parse_next(i)?;
                expr_next(Expr::new(
                    Span::encompassing(prior.span, no.span),
                    ExprKind::Ternary {
                        condition: Box::new(prior),
                        yes: Box::new(yes),
                        no: Box::new(no),
                    },
                ))
                .parse_next(i)
            }
            TokenKind::OpenBracket => {
                TokenKind::OpenBracket.parse_next(i)?;
                let name = expression.parse_next(i)?;
                let end = TokenKind::CloseBracket.parse_next(i)?.span;
                expr_next(Expr::new(
                    Span::encompassing(start, end),
                    ExprKind::Field(Box::new(prior), Box::new(name)),
                ))
                .context(StrContext::Label("expression"))
                .parse_next(i)
            }
            TokenKind::Keyword(keyword) => {
                if let Some(binop) = BinaryOperator::for_keyword(keyword) {
                    any.parse_next(i)?;
                    expression.parse_next(i).map(|next| {
                        expr_for_binary_operator(binop, Box::new(prior), Box::new(next))
                    })
                } else {
                    Ok(prior)
                }
            }
            _ => Ok(prior),
        }
    }
}

pub(crate) fn expr_list<'i>(i: &mut Tokens<'i>) -> ModalResult<Vec<Expr<'i>>> {
    separated(0.., expression, TokenKind::Comma).parse_next(i)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Function, FunctionArgument, Statement, StatementKind};
    use crate::lexer::operator::Operator;
    use crate::lexer::tokens::{QuoteKind, Token, TokenKind};
    use crate::parser::tests::build_tokens;
    use winnow::stream::TokenSlice;

    fn parse_expr<'i>(tokens: &'i [Token<'i>]) -> ModalResult<Expr<'i>> {
        expression(&mut TokenSlice::new(tokens))
    }

    // Helpers to build AST nodes for expectations with default spans
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
    fn test_identifier() {
        let tokens = build_tokens(&[(TokenKind::Identifier, "foo")]);
        assert_eq!(parse_expr(&tokens), Ok(id("foo")));
    }

    #[test]
    fn test_string() {
        let tokens = build_tokens(&[(TokenKind::String(QuoteKind::Double), "hello")]);
        assert_eq!(parse_expr(&tokens), Ok(s("hello")));
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
            Ok(ex(ExprKind::Call {
                name: Box::new(id("foo")),
                args: vec![]
            }))
        );
    }

    #[test]
    fn test_call_two_args() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "foo"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::Comma, ","),
            (TokenKind::String(QuoteKind::Double), "str"),
            (TokenKind::CloseParen, ")"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::Call {
                name: Box::new(id("foo")),
                args: vec![id("a"), s("str")]
            }))
        );
    }

    #[test]
    fn test_binary_add() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::Add), "+"),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::Add,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        );
    }

    #[test]
    fn test_binary_add_assign() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::AddAssign), "+="),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::AddAssign,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        );
    }

    #[test]
    fn test_binary_sub() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::Sub), "-"),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::Sub,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        );
    }

    #[test]
    fn test_binary_sub_assign() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::SubAssign), "-="),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::SubAssign,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        );
    }

    #[test]
    fn test_binary_divide() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::Divide), "/"),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::Divide,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        );
    }

    #[test]
    fn test_binary_divide_assign() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::DivideAssign), "/="),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::DivideAssign,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        );
    }

    #[test]
    fn test_binary_multiply() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::Multiply), "*"),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::Multiply,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        );
    }

    #[test]
    fn test_binary_multiply_assign() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::MultiplyAssign), "*="),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::MultiplyAssign,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        );
    }

    #[test]
    fn test_binary_modulo() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::Modulo), "%"),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::Modulo,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        );
    }

    #[test]
    fn test_binary_bit_not() {
        let tokens = build_tokens(&[
            (TokenKind::Operator(Operator::BitNot), "~"),
            (TokenKind::Identifier, "a"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::UnaryOperator(
                UnaryOperator::BitNot,
                Box::new(id("a"))
            )))
        )
    }

    #[test]
    fn test_binary_bit_and() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::BitAnd), "&"),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::BitAnd,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_binary_bit_or() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::BitOr), "|"),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::BitOr,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_binary_bit_xor() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::BitXor), "^"),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::BitXor,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_binary_bit_shift_left() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::BitShiftLeft), "<<"),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::BitShiftLeft,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_binary_bit_shift_right() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::BitShiftRight), ">>"),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::BitShiftRight,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_binary_bit_shift_right_unsigned() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::BitShiftRightUnsigned), ">>>"),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::BitShiftRightUnsigned,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_binary_muodulo_assign() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::ModuloAssign), "%="),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::ModuloAssign,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        );
    }

    #[test]
    fn test_binary_bit_and_assign() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::BitAndAssign), "&="),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::BitAndAssign,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_binary_bit_or_assign() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::BitOrAssign), "|="),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::BitOrAssign,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_binary_bit_xor_assign() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::BitXorAssign), "^="),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::BitXorAssign,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_binary_bit_shift_left_assign() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::BitShiftLeftAssign), "<<="),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::BitShiftLeftAssign,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_binary_bit_shift_right_assign() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::BitShiftRightAssign), ">>="),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::BitShiftRightAssign,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_binary_bit_shift_right_unsigned_assign() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (
                TokenKind::Operator(Operator::BitShiftRightUnsignedAssign),
                ">>>=",
            ),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::BitShiftRightUnsignedAssign,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_integer() {
        let tokens = build_tokens(&[(TokenKind::Integer, "0123")]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::Constant(ConstantKind::Integer(123))))
        );
    }

    #[test]
    fn test_integer_hex() {
        let tokens = build_tokens(&[(TokenKind::Integer, "0x1A2B3C")]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::Constant(ConstantKind::Integer(0x1A2B3C))))
        );
    }

    #[test]
    fn test_float() {
        let tokens = build_tokens(&[(TokenKind::Float, "012.345")]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::Constant(ConstantKind::Float(12.345))))
        );
    }

    #[test]
    fn test_float_leading_period() {
        let tokens = build_tokens(&[(TokenKind::Float, ".123")]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::Constant(ConstantKind::Float(0.123))))
        );
    }

    #[test]
    fn test_float_trailing_period() {
        let tokens = build_tokens(&[(TokenKind::Float, "123.")]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::Constant(ConstantKind::Float(123.0))))
        );
    }

    #[test]
    fn test_unary_sub() {
        let tokens = build_tokens(&[
            (TokenKind::Operator(Operator::Sub), "-"),
            (TokenKind::Identifier, "a"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::UnaryOperator(
                UnaryOperator::Sub,
                Box::new(id("a"))
            )))
        );
    }

    #[test]
    fn test_unary_sub_in_binary_expression() {
        let tokens = build_tokens(&[
            (TokenKind::Operator(Operator::Sub), "-"),
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::Sub), "-"),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::Sub,
                Box::new(ex(ExprKind::UnaryOperator(
                    UnaryOperator::Sub,
                    Box::new(id("a"))
                ))),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_unary_add_in_binary_expression() {
        let tokens = build_tokens(&[
            (TokenKind::Operator(Operator::Add), "+"),
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::Add), "+"),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::Add,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_unary_add() {
        let tokens = build_tokens(&[
            (TokenKind::Operator(Operator::Add), "+"),
            (TokenKind::Identifier, "a"),
        ]);
        assert_eq!(parse_expr(&tokens), Ok(id("a")));
    }

    #[test]
    fn test_unary_increment() {
        let tokens = build_tokens(&[
            (TokenKind::Operator(Operator::Increment), "++"),
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::Add), "+"),
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::Increment), "++"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::Add,
                Box::new(ex(ExprKind::UnaryOperator(
                    UnaryOperator::Increment(Affix::Prefix),
                    Box::new(id("a"))
                ))),
                Box::new(ex(ExprKind::UnaryOperator(
                    UnaryOperator::Increment(Affix::Postfix),
                    Box::new(id("a"))
                )))
            )))
        );
    }

    #[test]
    fn test_unary_decrement() {
        let tokens = build_tokens(&[
            (TokenKind::Operator(Operator::Decrement), "--"),
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::Add), "+"),
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::Decrement), "--"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::Add,
                Box::new(ex(ExprKind::UnaryOperator(
                    UnaryOperator::Decrement(Affix::Prefix),
                    Box::new(id("a"))
                ))),
                Box::new(ex(ExprKind::UnaryOperator(
                    UnaryOperator::Decrement(Affix::Postfix),
                    Box::new(id("a"))
                )))
            )))
        );
    }

    #[test]
    fn test_comparison_equal() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::Equal), "=="),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::Equal,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_comparison_strict_equal() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::StrictEqual), "==="),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::StrictEqual,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_comparison_not_equal() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::NotEqual), "!="),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::NotEqual,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_comparison_strict_not_equal() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::StrictNotEqual), "!=="),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::StrictNotEqual,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_comparison_less_than() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::LessThan), "<"),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::LessThan,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_comparison_less_than_equal() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::LessThanEqual), "<="),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::LessThanEqual,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_comparison_greater_than() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::GreaterThan), ">"),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::GreaterThan,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_comparison_greater_than_equal() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::GreaterThanEqual), ">="),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::GreaterThanEqual,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_logical_and() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::LogicalAnd), "&&"),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::LogicalAnd,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_logical_or() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::LogicalOr), "||"),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::LogicalOr,
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_logical_not() {
        let tokens = build_tokens(&[
            (TokenKind::Operator(Operator::LogicalNot), "!"),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::UnaryOperator(
                UnaryOperator::LogicalNot,
                Box::new(id("b"))
            )))
        )
    }

    #[test]
    fn test_ternary() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::GreaterThan), ">"),
            (TokenKind::Identifier, "b"),
            (TokenKind::Question, "?"),
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::Add), "+"),
            (TokenKind::Identifier, "b"),
            (TokenKind::Colon, ":"),
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::Sub), "-"),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::Ternary {
                condition: Box::new(ex(ExprKind::BinaryOperator(
                    BinaryOperator::GreaterThan,
                    Box::new(id("a")),
                    Box::new(id("b")),
                ))),
                yes: Box::new(ex(ExprKind::BinaryOperator(
                    BinaryOperator::Add,
                    Box::new(id("a")),
                    Box::new(id("b")),
                ))),
                no: Box::new(ex(ExprKind::BinaryOperator(
                    BinaryOperator::Sub,
                    Box::new(id("a")),
                    Box::new(id("b")),
                ))),
            }))
        )
    }

    #[test]
    fn test_ternary_instanceof() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Keyword(Keyword::InstanceOf), "instanceof"),
            (TokenKind::Identifier, "b"),
            (TokenKind::Question, "?"),
            (TokenKind::Identifier, "a"),
            (TokenKind::Colon, ":"),
            (TokenKind::Identifier, "c"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::Ternary {
                condition: Box::new(ex(ExprKind::BinaryOperator(
                    BinaryOperator::InstanceOf,
                    Box::new(id("a")),
                    Box::new(id("b")),
                ))),
                yes: Box::new(id("a")),
                no: Box::new(id("c")),
            }))
        )
    }

    #[test]
    fn test_parenthesis() {
        let tokens = build_tokens(&[
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::CloseParen, ")"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::Parenthesis(Box::new(id("a")))))
        );
    }

    #[test]
    fn test_empty_object() {
        let tokens = build_tokens(&[(TokenKind::OpenBrace, "{"), (TokenKind::CloseBrace, "}")]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::InitObject(Vec::new())))
        );
    }

    #[test]
    fn test_object() {
        let tokens = build_tokens(&[
            (TokenKind::OpenBrace, "{"),
            (TokenKind::Identifier, "a"),
            (TokenKind::Colon, ":"),
            (TokenKind::Identifier, "b"),
            (TokenKind::Comma, ","),
            (TokenKind::Identifier, "c"),
            (TokenKind::Colon, ":"),
            (TokenKind::Identifier, "d"),
            (TokenKind::Operator(Operator::Add), "+"),
            (TokenKind::Identifier, "e"),
            (TokenKind::CloseBrace, "}"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::InitObject(vec![
                ("a", id("b")),
                (
                    "c",
                    ex(ExprKind::BinaryOperator(
                        BinaryOperator::Add,
                        Box::new(id("d")),
                        Box::new(id("e")),
                    )),
                ),
            ])))
        );
    }

    #[test]
    fn test_field_access() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Period, "."),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::Field(Box::new(id("a")), Box::new(s("b")))))
        )
    }

    #[test]
    fn test_new_keyword() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::New), "new"),
            (TokenKind::Identifier, "a"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "b"),
            (TokenKind::CloseParen, ")"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::New {
                name: Box::new(id("a")),
                args: vec![id("b")],
            }))
        )
    }

    #[test]
    fn test_new_keyword_without_args() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::New), "new"),
            (TokenKind::Identifier, "a"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::New {
                name: Box::new(id("a")),
                args: vec![],
            }))
        )
    }

    #[test]
    fn test_typeof() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::TypeOf), "typeof"),
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::Add), "+"),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::BinaryOperator(
                BinaryOperator::Add,
                Box::new(ex(ExprKind::TypeOf(vec![id("a")]))),
                Box::new(id("b")),
            )))
        )
    }

    #[test]
    fn test_typeof_as_function() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::TypeOf), "typeof"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::Comma, ","),
            (TokenKind::Identifier, "b"),
            (TokenKind::CloseParen, ")"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::TypeOf(vec![id("a"), id("b")])))
        )
    }

    #[test]
    fn test_array_empty() {
        let tokens = build_tokens(&[
            (TokenKind::OpenBracket, "["),
            (TokenKind::Identifier, "a"),
            (TokenKind::CloseBracket, "]"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::InitArray(vec![id("a")])))
        )
    }

    #[test]
    fn test_array_complex() {
        let tokens = build_tokens(&[
            (TokenKind::OpenBracket, "["),
            (TokenKind::Identifier, "a"),
            (TokenKind::Comma, ","),
            (TokenKind::OpenBracket, "["),
            (TokenKind::Identifier, "b"),
            (TokenKind::CloseBracket, "]"),
            (TokenKind::CloseBracket, "]"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::InitArray(vec![
                id("a"),
                ex(ExprKind::InitArray(vec![id("b")])),
            ])))
        )
    }

    #[test]
    fn test_array_access_simple() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::OpenBracket, "["),
            (TokenKind::Identifier, "b"),
            (TokenKind::CloseBracket, "]"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::Field(Box::new(id("a")), Box::new(id("b")))))
        )
    }

    #[test]
    fn test_array_access_complex() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::OpenBracket, "["),
            (TokenKind::Identifier, "b"),
            (TokenKind::OpenParen, "("),
            (TokenKind::CloseParen, ")"),
            (TokenKind::CloseBracket, "]"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::Field(
                Box::new(id("a")),
                Box::new(ex(ExprKind::Call {
                    name: Box::new(id("b")),
                    args: vec![],
                }))
            )))
        )
    }

    #[test]
    fn test_delete_identifier() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Delete), "delete"),
            (TokenKind::Identifier, "a"),
        ]);
        assert_eq!(parse_expr(&tokens), Ok(ex(ExprKind::Delete(vec![id("a")]))))
    }

    #[test]
    fn test_delete_as_function() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Delete), "delete"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::Comma, ","),
            (TokenKind::Identifier, "b"),
            (TokenKind::CloseParen, ")"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::Delete(vec![id("a"), id("b")])))
        )
    }

    #[test]
    fn test_delete_field() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Delete), "delete"),
            (TokenKind::Identifier, "a"),
            (TokenKind::Period, "."),
            (TokenKind::Identifier, "a"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::Delete(vec![ex(ExprKind::Field(
                Box::new(id("a")),
                Box::new(s("a"))
            ))])))
        )
    }

    #[test]
    fn test_void_as_keyword() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Void), "void"),
            (TokenKind::Identifier, "a"),
        ]);
        assert_eq!(parse_expr(&tokens), Ok(ex(ExprKind::Void(vec![id("a")]))))
    }

    #[test]
    fn test_void_as_function() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Void), "void"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::Comma, ","),
            (TokenKind::Identifier, "b"),
            (TokenKind::CloseParen, ")"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::Void(vec![id("a"), id("b")])))
        )
    }

    #[test]
    fn test_function_definition() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Function), "function"),
            (TokenKind::Identifier, "func"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::Comma, ","),
            (TokenKind::Identifier, "b"),
            (TokenKind::Colon, ":"),
            (TokenKind::Identifier, "Number"),
            (TokenKind::CloseParen, ")"),
            (TokenKind::OpenBrace, "{"),
            (TokenKind::Identifier, "trace"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::CloseParen, ")"),
            (TokenKind::CloseBrace, "}"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::Function(Function {
                name: Some("func"),
                args: vec![
                    FunctionArgument {
                        name: "a",
                        type_name: None
                    },
                    FunctionArgument {
                        name: "b",
                        type_name: Some(Spanned::new(Span::default(), "Number"))
                    }
                ],
                body: vec![Statement::new(
                    Span::default(),
                    StatementKind::Expr(ex(ExprKind::Call {
                        name: Box::new(id("trace")),
                        args: vec![id("a")],
                    }))
                )],
                return_type: None,
            })))
        )
    }

    #[test]
    fn test_get() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Get), "get"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::CloseParen, ")"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::GetVariable(Box::new(id("a")))))
        )
    }

    #[test]
    fn test_set() {
        let tokens = build_tokens(&[
            (TokenKind::Keyword(Keyword::Set), "set"),
            (TokenKind::OpenParen, "("),
            (TokenKind::Identifier, "a"),
            (TokenKind::Comma, ","),
            (TokenKind::Identifier, "b"),
            (TokenKind::CloseParen, ")"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(ex(ExprKind::SetVariable(
                Box::new(id("a")),
                Box::new(id("b"))
            )))
        )
    }
}

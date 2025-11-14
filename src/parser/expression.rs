use crate::lexer::operator::Operator;
use crate::lexer::tokens::{Keyword, TokenKind};
use crate::parser::operator::{Affix, BinaryOperator, UnaryOperator};
use crate::parser::{Tokens, identifier, operator, skip_newline, string};
use serde::Serialize;
use winnow::combinator::{alt, fail, peek, separated};
use winnow::error::{ContextError, ErrMode, StrContext};
use winnow::error::{ParserError, StrContextValue};
use winnow::stream::ParseSlice;
use winnow::token::{any, take_while};
use winnow::{ModalResult, Parser};

#[derive(Debug, Clone, Serialize, PartialEq)]
pub(crate) enum Expr {
    Constant(Constant),
    Call {
        name: Box<Expr>,
        args: Vec<Expr>,
    },
    BinaryOperator(BinaryOperator, Box<Expr>, Box<Expr>),
    UnaryOperator(UnaryOperator, Box<Expr>),
    Parenthesis(Box<Expr>),
    Ternary {
        condition: Box<Expr>,
        yes: Box<Expr>,
        no: Box<Expr>,
    },
    InitObject(Vec<(String, Expr)>),
}

impl Expr {
    pub(crate) fn can_postfix(&self) -> bool {
        matches!(self, Expr::Constant(_))
    }
}

#[derive(Debug, Clone, Serialize, PartialEq)]
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
        TokenKind::Operator(Operator::Sub) => {
            TokenKind::Operator(Operator::Sub).parse_next(i)?;
            expression
                .map(|e| Expr::for_unary_operator(UnaryOperator::Sub, Box::new(e)))
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
                    Expr::for_unary_operator(UnaryOperator::Increment(Affix::Prefix), Box::new(e))
                })
                .context(StrContext::Label("expression"))
                .parse_next(i)
        }
        TokenKind::Operator(Operator::Decrement) => {
            TokenKind::Operator(Operator::Decrement).parse_next(i)?;
            expression
                .map(|e| {
                    Expr::for_unary_operator(UnaryOperator::Decrement(Affix::Prefix), Box::new(e))
                })
                .context(StrContext::Label("expression"))
                .parse_next(i)
        }
        TokenKind::Operator(Operator::BitNot) => {
            TokenKind::Operator(Operator::BitNot).parse_next(i)?;
            expression
                .map(|e| Expr::for_unary_operator(UnaryOperator::BitNot, Box::new(e)))
                .context(StrContext::Label("expression"))
                .parse_next(i)
        }
        TokenKind::Operator(Operator::LogicalNot) => {
            TokenKind::Operator(Operator::LogicalNot).parse_next(i)?;
            expression
                .map(|e| Expr::for_unary_operator(UnaryOperator::LogicalNot, Box::new(e)))
                .context(StrContext::Label("expression"))
                .parse_next(i)
        }
        TokenKind::OpenParen => {
            TokenKind::OpenParen.parse_next(i)?;
            let val = expression
                .context(StrContext::Label("expression"))
                .parse_next(i)?;
            TokenKind::CloseParen.parse_next(i)?;
            expr_next(Expr::Parenthesis(Box::new(val)))
                .context(StrContext::Label("expression"))
                .parse_next(i)
        }
        TokenKind::String(_) | TokenKind::Identifier | TokenKind::Float | TokenKind::Integer => {
            let val = constant
                .context(StrContext::Label("constant"))
                .parse_next(i)?;
            expr_next(Expr::Constant(val))
                .context(StrContext::Label("expression"))
                .parse_next(i)
        }
        TokenKind::OpenBrace => {
            TokenKind::OpenBrace.parse_next(i)?;
            let definition = object_definition
                .context(StrContext::Label("object definition"))
                .parse_next(i)?;
            TokenKind::CloseBrace.parse_next(i)?;
            expr_next(Expr::InitObject(definition))
                .context(StrContext::Label("expression"))
                .parse_next(i)
        }
        _ => fail
            .context(StrContext::Expected(StrContextValue::Description("string")))
            .context(StrContext::Expected(StrContextValue::Description(
                "identifier",
            )))
            .context(StrContext::Expected(StrContextValue::Description("float")))
            .context(StrContext::Expected(StrContextValue::Description(
                "integer",
            )))
            .parse_next(i),
    }
}

fn object_definition(i: &mut Tokens<'_>) -> ModalResult<Vec<(String, Expr)>> {
    separated(
        0..,
        (identifier, TokenKind::Colon, expression).map(|(name, _, expr)| (name, expr)),
        TokenKind::Comma,
    )
    .parse_next(i)
}

fn constant(i: &mut Tokens<'_>) -> ModalResult<Constant> {
    take_while(0.., TokenKind::Newline).parse_next(i)?;
    let token = peek(any).parse_next(i)?;
    match token.kind {
        TokenKind::String(_) => string
            .map(Constant::String)
            .context(StrContext::Label("string"))
            .parse_next(i),
        TokenKind::Identifier => identifier
            .map(Constant::Identifier)
            .context(StrContext::Label("identifier"))
            .parse_next(i),
        TokenKind::Float => float
            .map(Constant::Float)
            .context(StrContext::Label("float"))
            .parse_next(i),
        TokenKind::Integer => alt((integer.map(Constant::Integer), float.map(Constant::Float)))
            .context(StrContext::Label("integer"))
            .parse_next(i),
        _ => fail.parse_next(i),
    }
}

fn float(i: &mut Tokens<'_>) -> ModalResult<f64> {
    let raw = alt((TokenKind::Float, TokenKind::Integer))
        .parse_next(i)?
        .raw;
    let value = raw
        .parse_slice()
        .ok_or_else(|| ParserError::from_input(&raw))?;
    Ok(value)
}

fn integer(i: &mut Tokens<'_>) -> ModalResult<i32> {
    let raw = TokenKind::Integer.parse_next(i)?.raw;
    Ok(if let Some(raw) = raw.strip_prefix("0x") {
        i32::from_str_radix(raw, 16)
    } else {
        raw.parse::<i32>()
    }
    .map_err(|_| ParserError::from_input(&raw)))?
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
            TokenKind::Operator(Operator::Increment) if prior.can_postfix() => {
                TokenKind::Operator(Operator::Increment).parse_next(i)?;
                expr_next(Expr::UnaryOperator(
                    UnaryOperator::Increment(Affix::Postfix),
                    Box::new(prior),
                ))
                .parse_next(i)
            }
            TokenKind::Operator(Operator::Decrement) if prior.can_postfix() => {
                TokenKind::Operator(Operator::Decrement).parse_next(i)?;
                expr_next(Expr::UnaryOperator(
                    UnaryOperator::Decrement(Affix::Postfix),
                    Box::new(prior),
                ))
                .parse_next(i)
            }
            TokenKind::Operator(_) => {
                let op = operator::binary_operator.parse_next(i)?;
                expression
                    .parse_next(i)
                    .map(|next| Expr::for_binary_operator(op, Box::new(prior), Box::new(next)))
            }
            TokenKind::Keyword(Keyword::InstanceOf) => {
                TokenKind::Keyword(Keyword::InstanceOf).parse_next(i)?;
                expression.parse_next(i).map(|next| {
                    Expr::for_binary_operator(
                        BinaryOperator::InstanceOf,
                        Box::new(prior),
                        Box::new(next),
                    )
                })
            }
            TokenKind::Question => {
                TokenKind::Question.parse_next(i)?;
                let yes = expression.parse_next(i)?;
                TokenKind::Colon.parse_next(i)?;
                let no = expression.parse_next(i)?;
                expr_next(Expr::Ternary {
                    condition: Box::new(prior),
                    yes: Box::new(yes),
                    no: Box::new(no),
                })
                .parse_next(i)
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
    use crate::lexer::operator::Operator;
    use crate::lexer::tokens::{QuoteKind, Token, TokenKind};
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
        let tokens = build_tokens(&[(TokenKind::String(QuoteKind::Double), "hello")]);
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
            (TokenKind::String(QuoteKind::Double), "str"),
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
            (TokenKind::Operator(Operator::Add), "+"),
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
    fn test_binary_add_assign() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::AddAssign), "+="),
            (TokenKind::Identifier, "b"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(Expr::BinaryOperator(
                BinaryOperator::AddAssign,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::Sub,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::SubAssign,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::Divide,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::DivideAssign,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::Multiply,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::MultiplyAssign,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::Modulo,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::UnaryOperator(
                UnaryOperator::BitNot,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::BitAnd,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::BitOr,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::BitXor,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::BitShiftLeft,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::BitShiftRight,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::BitShiftRightUnsigned,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::ModuloAssign,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::BitAndAssign,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::BitOrAssign,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::BitXorAssign,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::BitShiftLeftAssign,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::BitShiftRightAssign,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::BitShiftRightUnsignedAssign,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
        )
    }

    #[test]
    fn test_binary_right_associative() {
        // a + b + c parses as a + (b + c)
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "a"),
            (TokenKind::Operator(Operator::Add), "+"),
            (TokenKind::Identifier, "b"),
            (TokenKind::Operator(Operator::Add), "+"),
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
    fn test_integer_hex() {
        let tokens = build_tokens(&[(TokenKind::Integer, "0x1A2B3C")]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(Expr::Constant(Constant::Integer(0x1A2B3C)))
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

    #[test]
    fn test_unary_sub() {
        let tokens = build_tokens(&[
            (TokenKind::Operator(Operator::Sub), "-"),
            (TokenKind::Identifier, "a"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(Expr::UnaryOperator(
                UnaryOperator::Sub,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::Sub,
                Box::new(Expr::UnaryOperator(
                    UnaryOperator::Sub,
                    Box::new(Expr::Constant(Constant::Identifier("a".to_string())))
                )),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::Add,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
        )
    }

    #[test]
    fn test_unary_add() {
        let tokens = build_tokens(&[
            (TokenKind::Operator(Operator::Add), "+"),
            (TokenKind::Identifier, "a"),
        ]);
        assert_eq!(
            parse_expr(&tokens),
            Ok(Expr::Constant(Constant::Identifier("a".to_string())))
        );
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::Add,
                Box::new(Expr::UnaryOperator(
                    UnaryOperator::Increment(Affix::Prefix),
                    Box::new(Expr::Constant(Constant::Identifier("a".to_string())))
                )),
                Box::new(Expr::UnaryOperator(
                    UnaryOperator::Increment(Affix::Postfix),
                    Box::new(Expr::Constant(Constant::Identifier("a".to_string())))
                ))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::Add,
                Box::new(Expr::UnaryOperator(
                    UnaryOperator::Decrement(Affix::Prefix),
                    Box::new(Expr::Constant(Constant::Identifier("a".to_string())))
                )),
                Box::new(Expr::UnaryOperator(
                    UnaryOperator::Decrement(Affix::Postfix),
                    Box::new(Expr::Constant(Constant::Identifier("a".to_string())))
                ))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::Equal,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::StrictEqual,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::NotEqual,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::StrictNotEqual,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::LessThan,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::LessThanEqual,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::GreaterThan,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::GreaterThanEqual,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::LogicalAnd,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::BinaryOperator(
                BinaryOperator::LogicalOr,
                Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::UnaryOperator(
                UnaryOperator::LogicalNot,
                Box::new(Expr::Constant(Constant::Identifier("b".to_string())))
            ))
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
            Ok(Expr::Ternary {
                condition: Box::new(Expr::BinaryOperator(
                    BinaryOperator::GreaterThan,
                    Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                    Box::new(Expr::Constant(Constant::Identifier("b".to_string()))),
                )),
                yes: Box::new(Expr::BinaryOperator(
                    BinaryOperator::Add,
                    Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                    Box::new(Expr::Constant(Constant::Identifier("b".to_string()))),
                )),
                no: Box::new(Expr::BinaryOperator(
                    BinaryOperator::Sub,
                    Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                    Box::new(Expr::Constant(Constant::Identifier("b".to_string()))),
                )),
            })
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
            Ok(Expr::Ternary {
                condition: Box::new(Expr::BinaryOperator(
                    BinaryOperator::InstanceOf,
                    Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                    Box::new(Expr::Constant(Constant::Identifier("b".to_string()))),
                )),
                yes: Box::new(Expr::Constant(Constant::Identifier("a".to_string()))),
                no: Box::new(Expr::Constant(Constant::Identifier("c".to_string()))),
            })
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
            Ok(Expr::Parenthesis(Box::new(Expr::Constant(
                Constant::Identifier("a".to_string())
            ))))
        );
    }

    #[test]
    fn test_empty_object() {
        let tokens = build_tokens(&[(TokenKind::OpenBrace, "{"), (TokenKind::CloseBrace, "}")]);
        assert_eq!(parse_expr(&tokens), Ok(Expr::InitObject(Vec::new())));
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
            Ok(Expr::InitObject(vec![
                (
                    "a".to_string(),
                    Expr::Constant(Constant::Identifier("b".to_string()))
                ),
                (
                    "c".to_string(),
                    Expr::BinaryOperator(
                        BinaryOperator::Add,
                        Box::new(Expr::Constant(Constant::Identifier("d".to_string()))),
                        Box::new(Expr::Constant(Constant::Identifier("e".to_string()))),
                    )
                ),
            ]))
        );
    }
}

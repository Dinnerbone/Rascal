use crate::lexer::operator::Operator;
use crate::parser::Tokens;
use crate::source::Span;
use serde::Serialize;
use winnow::Parser;
use winnow::error::{ContextError, ErrMode};
use winnow::token::literal;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize)]
pub struct Token<'a> {
    pub(crate) kind: TokenKind,
    pub(crate) span: Span,
    pub(crate) raw: &'a str,
}

impl<'a> Token<'a> {
    pub(crate) fn new(kind: TokenKind, span: Span, raw: &'a str) -> Self {
        Self { kind, span, raw }
    }
}

impl PartialEq<TokenKind> for Token<'_> {
    fn eq(&self, other: &TokenKind) -> bool {
        self.kind == *other
    }
}

impl winnow::stream::ContainsToken<&'_ Token<'_>> for TokenKind {
    #[inline]
    fn contains_token(&self, token: &'_ Token<'_>) -> bool {
        *self == token.kind
    }
}

impl winnow::stream::ContainsToken<&'_ Token<'_>> for &'_ [TokenKind] {
    #[inline]
    fn contains_token(&self, token: &'_ Token<'_>) -> bool {
        self.contains(&token.kind)
    }
}

impl<const LEN: usize> winnow::stream::ContainsToken<&'_ Token<'_>> for &'_ [TokenKind; LEN] {
    #[inline]
    fn contains_token(&self, token: &'_ Token<'_>) -> bool {
        self.contains(&token.kind)
    }
}

impl<const LEN: usize> winnow::stream::ContainsToken<&'_ Token<'_>> for [TokenKind; LEN] {
    #[inline]
    fn contains_token(&self, token: &'_ Token<'_>) -> bool {
        self.contains(&token.kind)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize)]
pub enum TokenKind {
    Identifier,
    Keyword(Keyword),
    Operator(Operator),
    Semicolon,
    String(QuoteKind),
    OpenParen,
    CloseParen,
    Comma,
    Newline,
    Unknown,
    Integer,
    Float,
    Question,
    Colon,
    OpenBrace,
    CloseBrace,
    Period,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize)]
pub enum QuoteKind {
    Double,
    Single,
}

impl<'i> Parser<Tokens<'i>, &'i Token<'i>, ErrMode<ContextError>> for TokenKind {
    fn parse_next(
        &mut self,
        input: &mut Tokens<'i>,
    ) -> winnow::Result<&'i Token<'i>, ErrMode<ContextError>> {
        literal(*self).parse_next(input).map(|t| &t[0])
    }
}

impl<'i> Parser<Tokens<'i>, &'i Token<'i>, ContextError> for TokenKind {
    fn parse_next(&mut self, input: &mut Tokens<'i>) -> winnow::Result<&'i Token<'i>> {
        literal(*self).parse_next(input).map(|t| &t[0])
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize)]
pub enum Keyword {
    Var,
    InstanceOf,
    New,
    TypeOf,
}

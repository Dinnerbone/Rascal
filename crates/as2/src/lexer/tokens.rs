use crate::lexer::operator::Operator;
use crate::parser::{Tokens, ignore_newlines};
use rascal_common::span::Span;
use serde::Serialize;
use winnow::Parser;
use winnow::error::{ContextError, ErrMode, StrContext, StrContextValue};
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
    OpenBracket,
    CloseBracket,
    PCode,
}

impl TokenKind {
    pub(crate) fn expected(&self) -> StrContextValue {
        match self {
            TokenKind::Identifier => StrContextValue::Description("identifier"),
            TokenKind::Keyword(keyword) => StrContextValue::StringLiteral(keyword.text()),
            TokenKind::Operator(operator) => StrContextValue::StringLiteral(operator.text()),
            TokenKind::Semicolon => StrContextValue::CharLiteral(';'),
            TokenKind::String(_) => StrContextValue::Description("string"),
            TokenKind::OpenParen => StrContextValue::CharLiteral('('),
            TokenKind::CloseParen => StrContextValue::CharLiteral(')'),
            TokenKind::Comma => StrContextValue::CharLiteral(','),
            TokenKind::Newline => StrContextValue::Description("newline"),
            TokenKind::Unknown => StrContextValue::Description("unknown"),
            TokenKind::Integer => StrContextValue::Description("integer"),
            TokenKind::Float => StrContextValue::Description("float"),
            TokenKind::Question => StrContextValue::CharLiteral('?'),
            TokenKind::Colon => StrContextValue::CharLiteral(':'),
            TokenKind::OpenBrace => StrContextValue::CharLiteral('{'),
            TokenKind::CloseBrace => StrContextValue::CharLiteral('}'),
            TokenKind::Period => StrContextValue::CharLiteral('.'),
            TokenKind::OpenBracket => StrContextValue::CharLiteral('['),
            TokenKind::CloseBracket => StrContextValue::CharLiteral(']'),
            TokenKind::PCode => StrContextValue::Description("@PCode"),
        }
    }
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
        ignore_newlines(literal(*self))
            .context(StrContext::Expected(self.expected()))
            .parse_next(input)
            .map(|t| &t[0])
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize)]
pub enum Keyword {
    Var,
    InstanceOf,
    New,
    TypeOf,
    Delete,
    In,
    Void,
    Function,
    Return,
    For,
    If,
    Else,
    Break,
    Continue,
    Throw,
    Try,
    Catch,
    Finally,
    IfFrameLoaded,
    TellTarget,
    Eq,
    Gt,
    Ge,
    Lt,
    Le,
    Ne,
    And,
    Or,
    Not,
    Add,
    While,
    Dynamic,
    Extends,
    Get,
    Implements,
    Interface,
    Private,
    Public,
    Set,
    Static,
    Case,
    Switch,
    Default,
    Class,
    With,
    Import,
}

impl Keyword {
    pub(crate) fn text(&self) -> &'static str {
        match self {
            Keyword::Var => "var",
            Keyword::InstanceOf => "instanceOf",
            Keyword::New => "new",
            Keyword::TypeOf => "typeOf",
            Keyword::Delete => "delete",
            Keyword::In => "in",
            Keyword::Void => "void",
            Keyword::Function => "function",
            Keyword::Return => "return",
            Keyword::For => "for",
            Keyword::If => "if",
            Keyword::Else => "else",
            Keyword::Break => "break",
            Keyword::Continue => "continue",
            Keyword::Throw => "throw",
            Keyword::Try => "try",
            Keyword::Catch => "catch",
            Keyword::Finally => "finally",
            Keyword::IfFrameLoaded => "ifFrameLoaded",
            Keyword::TellTarget => "tellTarget",
            Keyword::Eq => "eq",
            Keyword::Gt => "gt",
            Keyword::Ge => "ge",
            Keyword::Lt => "lt",
            Keyword::Le => "le",
            Keyword::Ne => "ne",
            Keyword::And => "and",
            Keyword::Or => "or",
            Keyword::Not => "not",
            Keyword::Add => "add",
            Keyword::While => "while",
            Keyword::Dynamic => "dynamic",
            Keyword::Extends => "extends",
            Keyword::Get => "get",
            Keyword::Implements => "implements",
            Keyword::Interface => "interface",
            Keyword::Private => "private",
            Keyword::Public => "public",
            Keyword::Set => "set",
            Keyword::Static => "static",
            Keyword::Case => "case",
            Keyword::Switch => "switch",
            Keyword::Default => "default",
            Keyword::Class => "class",
            Keyword::With => "with",
            Keyword::Import => "import",
        }
    }
}

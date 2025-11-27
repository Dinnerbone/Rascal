use crate::parser::Tokens;
use rascal_common::span::Span;
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
    Newline,
    String,
    Float,
    Integer,
    Register(u8),
    Constant(u16),
    ActionName(ActionName),
    Identifier,
    Colon,
    Comma,
    OpenBrace,
    CloseBrace,
    True,
    False,
    Null,
    Undefined,
    Catch,
    Finally,
    Unknown,
}

impl<'i> Parser<Tokens<'i>, &'i Token<'i>, ErrMode<ContextError>> for TokenKind {
    fn parse_next(
        &mut self,
        input: &mut Tokens<'i>,
    ) -> winnow::Result<&'i Token<'i>, ErrMode<ContextError>> {
        literal(*self).parse_next(input).map(|t| &t[0])
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize)]
pub enum ActionName {
    Add,
    Add2,
    AsciiToChar,
    BitAnd,
    BitLShift,
    BitOr,
    BitRShift,
    BitURShift,
    BitXor,
    Call,
    CallFunction,
    CallMethod,
    CastOp,
    CharToAscii,
    CloneSprite,
    ConstantPool,
    Decrement,
    DefineFunction,
    DefineLocal,
    DefineLocal2,
    Delete,
    Delete2,
    Divide,
    EndDrag,
    Enumerate2,
    Equals2,
    GetMember,
    GetProperty,
    GetTime,
    GetUrl,
    GetUrl2,
    GetVariable,
    GotoFrame,
    GotoFrame2,
    GotoLabel,
    Greater,
    If,
    Increment,
    InitArray,
    InitObject,
    InstanceOf,
    Jump,
    Less2,
    MBAsciiToChar,
    MBCharToAscii,
    MBStringExtract,
    MBStringLength,
    Modulo,
    Multiply,
    NewMethod,
    NewObject,
    NextFrame,
    Not,
    Play,
    Pop,
    PrevFrame,
    Push,
    PushDuplicate,
    RandomNumber,
    RemoveSprite,
    Return,
    SetMember,
    SetTarget,
    SetTarget2,
    SetVariable,
    StackSwap,
    StartDrag,
    Stop,
    StopSounds,
    StoreRegister,
    StrictEquals,
    StringAdd,
    StringExtract,
    StringLength,
    Subtract,
    TargetPath,
    Throw,
    ToInteger,
    ToNumber,
    ToString,
    ToggleQuality,
    Trace,
    Try,
    TypeOf,
    WaitForFrame,
    WaitForFrame2,
}

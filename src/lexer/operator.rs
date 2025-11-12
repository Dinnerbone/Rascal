use crate::lexer::Stream;
use crate::lexer::tokens::{Token, TokenKind};
use crate::source::Span;
use serde::Serialize;
use winnow::stream::{AsBStr, Location, Stream as _};

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize)]
pub enum Operator {
    Add,
    Assign,
    AddAssign,
    Sub,
    SubAssign,
    Divide,
    DivideAssign,
    Multiply,
    MultiplyAssign,
    Modulo,
    ModuloAssign,
    Increment,
    Decrement,
    BitAnd,
    BitAndAssign,
    BitOr,
    BitOrAssign,
    BitXor,
    BitXorAssign,
    BitNot,
    BitShiftLeft,
    BitShiftLeftAssign,
    BitShiftRight,
    BitShiftRightAssign,
    BitShiftRightUnsigned,
    BitShiftRightUnsignedAssign,
    Equal,
    StrictEqual,
    NotEqual,
    StrictNotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

pub(crate) fn lex_operator<'a>(stream: &mut Stream<'a>) -> Token<'a> {
    // Operator lookup table: (pattern, operator)
    const OPERATORS: &[(&[u8], Operator)] = &[
        // 4 char operators
        (b">>>=", Operator::BitShiftRightUnsignedAssign),
        // 3 char operators
        (b">>>", Operator::BitShiftRightUnsigned),
        (b">>=", Operator::BitShiftRightAssign),
        (b"<<=", Operator::BitShiftLeftAssign),
        (b"!==", Operator::StrictNotEqual),
        (b"===", Operator::StrictEqual),
        // 2 char operators
        (b"++", Operator::Increment),
        (b"--", Operator::Decrement),
        (b"+=", Operator::AddAssign),
        (b"*=", Operator::MultiplyAssign),
        (b"/=", Operator::DivideAssign),
        (b"-=", Operator::SubAssign),
        (b"%=", Operator::ModuloAssign),
        (b">>", Operator::BitShiftRight),
        (b"<<", Operator::BitShiftLeft),
        (b"&=", Operator::BitAndAssign),
        (b"|=", Operator::BitOrAssign),
        (b"^=", Operator::BitXorAssign),
        (b"<=", Operator::LessThanEqual),
        (b">=", Operator::GreaterThanEqual),
        (b"==", Operator::Equal),
        (b"!=", Operator::NotEqual),
        // 1 char operators
        (b"+", Operator::Add),
        (b"=", Operator::Assign),
        (b"-", Operator::Sub),
        (b"/", Operator::Divide),
        (b"*", Operator::Multiply),
        (b"%", Operator::Modulo),
        (b"&", Operator::BitAnd),
        (b"|", Operator::BitOr),
        (b"~", Operator::BitNot),
        (b"^", Operator::BitXor),
        (b"<", Operator::LessThan),
        (b">", Operator::GreaterThan),
    ];

    let available = stream.eof_offset();

    OPERATORS
        .iter()
        .find_map(|&(pattern, operator)| {
            let len = pattern.len();
            (available >= len && stream.as_bstr().peek_slice(len) == pattern)
                .then(|| lex_ascii_chars(stream, TokenKind::Operator(operator), len))
        })
        .expect("operator must match one of the patterns")
}

fn lex_ascii_chars<'a>(stream: &mut Stream<'a>, kind: TokenKind, len: usize) -> Token<'a> {
    let start = stream.current_token_start();
    let offset = len;
    let raw = stream.next_slice(offset);

    let end = stream.previous_token_end();
    let span = Span::new_unchecked(start, end);
    Token::new(kind, span, raw)
}

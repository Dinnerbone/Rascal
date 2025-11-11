#[cfg(test)]
mod tests;
pub(crate) mod tokens;

use crate::lexer::tokens::{BinaryOperator, Keyword, Token, TokenKind};
use crate::source::Span;
use winnow::stream::{AsBStr, AsChar, FindSlice, Location, Stream as _};

pub(crate) type Stream<'i> = winnow::stream::LocatingSlice<&'i str>;

pub struct Lexer<'i> {
    stream: Stream<'i>,
}

impl<'i> Lexer<'i> {
    pub fn new(input: &'i str) -> Self {
        Self {
            stream: Stream::new(input),
        }
    }

    pub fn into_vec(self) -> Vec<Token<'i>> {
        let capacity = core::cmp::min(self.stream.len(), usize::MAX / size_of::<Token>());
        let mut vec = Vec::with_capacity(capacity);
        vec.extend(self);
        vec
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let peek_byte = self.stream.as_bstr().first()?;
            if let Some(token) = process_token(*peek_byte, &mut self.stream) {
                return Some(token);
            }
        }
    }
}

fn process_token<'a>(peek_byte: u8, stream: &mut Stream<'a>) -> Option<Token<'a>> {
    match peek_byte {
        b' ' | b'\t' => {
            stream.next_slice(1);
            None
        }
        b'/' => lex_comment_or_divide(stream),
        b'(' => Some(lex_ascii_char(stream, TokenKind::OpenParen)),
        b',' => Some(lex_ascii_char(stream, TokenKind::Comma)),
        b')' => Some(lex_ascii_char(stream, TokenKind::CloseParen)),
        b';' => Some(lex_ascii_char(stream, TokenKind::Semicolon)),
        b'=' | b'+' | b'-' | b'*' | b'%' => Some(lex_operator(stream)),
        b'\r' => Some(lex_crlf(stream)),
        b'\n' => Some(lex_ascii_char(stream, TokenKind::Newline)),
        b'"' => Some(lex_basic_string(stream)),
        b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'$' => Some(lex_identifier_or_keyword(stream)),
        b'0'..=b'9' | b'.' => Some(lex_integer_or_float(stream)),
        _ => {
            let start = stream.current_token_start();
            let raw = stream.next_slice(stream.eof_offset());
            let end = stream.previous_token_end();
            Some(Token::new(
                TokenKind::Unknown,
                Span::new_unchecked(start, end),
                raw,
            ))
        }
    }
}

fn lex_comment_or_divide<'a>(stream: &mut Stream<'a>) -> Option<Token<'a>> {
    let next = stream.as_bstr().get(1);
    match next {
        Some(b'/') => {
            skip_line_comment(stream);
            None
        }
        Some(b'*') => {
            skip_block_comment(stream);
            None
        }
        _ => Some(lex_operator(stream)),
    }
}

fn skip_line_comment(stream: &mut Stream<'_>) {
    // Consume the initial '//'
    stream.next_slice(2);
    if let Some(offset) = stream.as_bstr().find_slice(&b"\n"[..]) {
        // Consume everything up to but not including the newline (so the newline is tokenized normally)
        stream.next_slice(offset.end);
    } else {
        // No newline until EOF; consume the rest
        stream.finish();
    }
}

fn skip_block_comment(stream: &mut Stream<'_>) {
    // Consume the initial '/*'
    stream.next_slice(2);
    if let Some(span) = stream.as_bstr().find_slice(&b"*/"[..]) {
        // Consume through the closing '*/'
        let offset = span.end;
        stream.next_slice(offset);
    } else {
        // Unterminated block comment: consume to EOF for error recovery
        stream.finish();
    }
}

fn lex_ascii_char<'a>(stream: &mut Stream<'a>, kind: TokenKind) -> Token<'a> {
    let start = stream.current_token_start();

    let offset = 1; // an ascii character
    let raw = stream.next_slice(offset);

    let end = stream.previous_token_end();
    let span = Span::new_unchecked(start, end);
    Token::new(kind, span, raw)
}

fn lex_ascii_chars<'a>(stream: &mut Stream<'a>, kind: TokenKind, len: usize) -> Token<'a> {
    let start = stream.current_token_start();
    let offset = len;
    let raw = stream.next_slice(offset);

    let end = stream.previous_token_end();
    let span = Span::new_unchecked(start, end);
    Token::new(kind, span, raw)
}

fn lex_integer_or_float<'a>(stream: &mut Stream<'a>) -> Token<'a> {
    let start = stream.current_token_start();
    let start_checkpoint = stream.checkpoint();

    // Special case: if the first two characters are '0x', treat the rest as hex digits
    let is_hex = stream.as_bstr().starts_with(b"0x");
    if is_hex {
        stream.next_slice(2); // skip the '0x'
    }
    let invalid_char: fn(u8) -> bool = if is_hex {
        |b| !b.is_hex_digit()
    } else {
        |b| !b.is_ascii_digit()
    };

    if let Some(offset) = stream.as_bstr().offset_for(invalid_char) {
        stream.next_slice(offset)
    } else {
        stream.finish()
    };

    let kind = if !is_hex && stream.as_bstr().first() == Some(&b'.') {
        stream.next_slice(1); // skip the '.'
        if let Some(offset) = stream.as_bstr().offset_for(invalid_char) {
            stream.next_slice(offset)
        } else {
            stream.finish()
        };
        TokenKind::Float
    } else {
        TokenKind::Integer
    };

    if matches!(stream.as_bstr().first(), Some(b'e' | b'E')) {
        // Optional exponent looks like e+2, E-1, e5, etc
        stream.next_slice(1); // skip the 'e' or 'E'
        if stream.as_bstr().first() == Some(&b'+') || stream.as_bstr().first() == Some(&b'-') {
            stream.next_slice(1); // skip the '+' or '-'
        }
        if let Some(offset) = stream.as_bstr().offset_for(invalid_char) {
            stream.next_slice(offset);
        }
    }

    let end = stream.previous_token_end();
    stream.reset(&start_checkpoint);
    let raw = stream.next_slice(end - start);

    let end = stream.previous_token_end();
    let span = Span::new_unchecked(start, end);
    Token::new(kind, span, raw)
}

fn lex_operator<'a>(stream: &mut Stream<'a>) -> Token<'a> {
    if stream.eof_offset() >= 2 {
        // 2 char operators
        let peek = stream.as_bstr().peek_slice(2);
        if peek == b"+=" {
            return lex_ascii_chars(
                stream,
                TokenKind::BinaryOperator(BinaryOperator::AddAssign),
                2,
            );
        }
    }

    // 1 char operators
    let peek = stream.as_bstr().peek_slice(stream.eof_offset().min(1));
    match peek {
        b"+" => lex_ascii_char(stream, TokenKind::BinaryOperator(BinaryOperator::Add)),
        b"=" => lex_ascii_char(stream, TokenKind::BinaryOperator(BinaryOperator::Assign)),
        b"-" => lex_ascii_char(stream, TokenKind::BinaryOperator(BinaryOperator::Sub)),
        b"/" => lex_ascii_char(stream, TokenKind::BinaryOperator(BinaryOperator::Divide)),
        b"*" => lex_ascii_char(stream, TokenKind::BinaryOperator(BinaryOperator::Multiply)),
        b"%" => lex_ascii_char(stream, TokenKind::BinaryOperator(BinaryOperator::Modulo)),
        _ => unreachable!(), // This is true as long as we have an entry that matches the caller's peeked value
    }
}

fn lex_crlf<'a>(stream: &mut Stream<'a>) -> Token<'a> {
    let start = stream.current_token_start();

    let mut offset = '\r'.len_utf8();
    let has_lf = stream.as_bstr().get(1) == Some(&b'\n');
    if has_lf {
        offset += '\n'.len_utf8();
    }

    let raw = stream.next_slice(offset);
    let end = stream.previous_token_end();
    let span = Span::new_unchecked(start, end);

    Token::new(TokenKind::Newline, span, raw)
}

pub(crate) const QUOTATION_MARK: u8 = b'"';
pub(crate) const ESCAPE: u8 = b'\\';
fn lex_basic_string<'a>(stream: &mut Stream<'a>) -> Token<'a> {
    let start = stream.current_token_start();

    let offset = 1; // QUOTATION_MARK
    stream.next_slice(offset);
    let start_checkpoint = stream.checkpoint();

    loop {
        // newline is present for error recovery
        if let Some(span) = stream.as_bstr().find_slice((QUOTATION_MARK, ESCAPE, b'\n')) {
            let found = stream.as_bstr()[span.start];
            if found == QUOTATION_MARK {
                let offset = span.end;
                stream.next_slice(offset);
                break;
            } else if found == ESCAPE {
                let offset = span.end;
                stream.next_slice(offset);

                let peek = stream.as_bstr().peek_token();
                if let Some(ESCAPE | QUOTATION_MARK) = peek {
                    let offset = 1; // ESCAPE / QUOTATION_MARK
                    stream.next_slice(offset);
                }
                continue;
            } else if found == b'\n' {
                let offset = span.start;
                stream.next_slice(offset);
                break;
            }
            unreachable!("found `{found}`");
        } else {
            stream.finish();
            break;
        }
    }
    let end = stream.previous_token_end();
    stream.reset(&start_checkpoint);
    let raw = stream.next_slice(end - start - 2);
    stream.next_slice(1);

    let span = Span::new_unchecked(start, end);
    Token::new(TokenKind::String, span, raw)
}

fn lex_identifier_or_keyword<'a>(stream: &mut Stream<'a>) -> Token<'a> {
    let start = stream.current_token_start();
    let offset = stream
        .as_bstr()
        .offset_for(|b| !b.is_ascii_alphanumeric() && b != b'_' && b != b'$')
        .unwrap_or(stream.eof_offset());
    let raw = stream.next_slice(offset);

    let end = stream.previous_token_end();
    let span = Span::new_unchecked(start, end);

    let kind = match raw {
        "var" => TokenKind::Keyword(Keyword::Var),
        _ => TokenKind::Identifier,
    };

    Token::new(kind, span, raw)
}

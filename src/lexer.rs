#[cfg(test)]
mod tests;
pub(crate) mod tokens;

use crate::lexer::tokens::{Keyword, Token, TokenKind};
use crate::source::Span;
use winnow::stream::{AsBStr, FindSlice, Location, Stream as _};

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
    Some(match peek_byte {
        b' ' | b'\t' => {
            stream.next_slice(1);
            return None;
        }
        b'(' => lex_ascii_char(stream, TokenKind::OpenParen),
        b')' => lex_ascii_char(stream, TokenKind::CloseParen),
        b';' => lex_ascii_char(stream, TokenKind::Semicolon),
        b'=' => lex_ascii_char(stream, TokenKind::Equals),
        b'\r' => lex_crlf(stream),
        b'\n' => lex_ascii_char(stream, TokenKind::Newline),
        b'"' => lex_basic_string(stream),
        b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'$' => lex_identifier_or_keyword(stream),
        _ => {
            let start = stream.current_token_start();
            let raw = stream.next_slice(stream.eof_offset());
            let end = stream.previous_token_end();
            Token::new(TokenKind::Unknown, Span::new_unchecked(start, end), raw)
        }
    })
}

fn lex_ascii_char<'a>(stream: &mut Stream<'a>, kind: TokenKind) -> Token<'a> {
    let start = stream.current_token_start();

    let offset = 1; // an ascii character
    let raw = stream.next_slice(offset);

    let end = stream.previous_token_end();
    let span = Span::new_unchecked(start, end);
    Token::new(kind, span, raw)
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

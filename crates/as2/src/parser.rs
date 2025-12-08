use crate::lexer::tokens::{QuoteKind, Token, TokenKind};
use rascal_common::span::Spanned;
use std::borrow::Cow;
use winnow::combinator::{alt, repeat};
use winnow::error::{ContextError, ErrMode, ParseError};
use winnow::stream::TokenSlice;
use winnow::token::literal;
use winnow::{ModalResult, Parser};

mod class;
pub(crate) mod document;
mod expression;
mod operator;
mod statement;
#[cfg(test)]
mod tests;

pub(crate) type Tokens<'i> = TokenSlice<'i, Token<'i>>;

use crate::ast::Document;

pub fn parse_document<'i>(
    source: &'i [Token<'i>],
) -> winnow::Result<Document<'i>, ParseError<Tokens<'i>, ContextError>> {
    document::document.parse(TokenSlice::new(source))
}

fn string<'i>(i: &mut Tokens<'i>) -> ModalResult<Spanned<Cow<'i, str>>> {
    skip_newlines(i)?;
    let (kind, raw, span) = alt((
        TokenKind::String(QuoteKind::Double).map(|t| (QuoteKind::Double, t.raw, t.span)),
        TokenKind::String(QuoteKind::Single).map(|t| (QuoteKind::Single, t.raw, t.span)),
    ))
    .parse_next(i)?;
    if !raw.contains("\\") {
        return Ok(Spanned::new(span, Cow::Borrowed(raw)));
    }
    let mut out = String::with_capacity(raw.len());
    let mut iter = raw.chars();
    while let Some(ch) = iter.next() {
        if ch == '\\' {
            match iter.next() {
                Some('n') => out.push('\n'),
                Some('r') => out.push('\r'),
                Some('t') => out.push('\t'),
                Some('\\') => out.push('\\'),
                Some('"') if matches!(kind, QuoteKind::Double) => out.push('"'),
                Some('\'') if matches!(kind, QuoteKind::Single) => out.push('\''),
                Some(other) => {
                    out.push('\\');
                    out.push(other);
                }
                None => {
                    out.push('\\');
                }
            }
        } else {
            out.push(ch);
        }
    }
    Ok(Spanned::new(span, Cow::Owned(out)))
}

fn identifier<'i>(i: &mut Tokens<'i>) -> ModalResult<Spanned<&'i str>> {
    skip_newlines(i)?;
    let token = TokenKind::Identifier.parse_next(i)?;
    Ok(Spanned::new(token.span, token.raw))
}

pub(crate) fn ignore_newlines<'i: 'i, O, P>(
    mut inner: P,
) -> impl Parser<Tokens<'i>, O, ErrMode<ContextError>>
where
    P: Parser<Tokens<'i>, O, ErrMode<ContextError>>,
{
    move |input: &mut Tokens<'i>| {
        skip_newlines(input)?;
        inner.parse_next(input)
    }
}

pub(crate) fn skip_newlines(i: &mut Tokens<'_>) -> ModalResult<()> {
    repeat(0.., literal(TokenKind::Newline))
        .map(|()| ())
        .parse_next(i)
}

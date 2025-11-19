use crate::lexer::tokens::{QuoteKind, Token, TokenKind};
use winnow::combinator::{alt, repeat};
use winnow::error::{ContextError, ErrMode, ParseError};
use winnow::stream::TokenSlice;
use winnow::token::literal;
use winnow::{ModalResult, Parser};

pub(crate) mod document;
mod error;
mod expression;
mod operator;
mod statement;
#[cfg(test)]
mod tests;

pub(crate) type Tokens<'i> = TokenSlice<'i, Token<'i>>;

use crate::ast::Document;
pub use crate::parser::error::ActionScriptError;

pub fn parse_document<'i>(
    source: &'i [Token<'i>],
) -> winnow::Result<Document<'i>, ParseError<Tokens<'i>, ContextError>> {
    document::document.parse(TokenSlice::new(source))
}

fn string(i: &mut Tokens<'_>) -> ModalResult<String> {
    skip_newlines(i)?;
    let (kind, raw) = alt((
        TokenKind::String(QuoteKind::Double).map(|t| (QuoteKind::Double, t.raw)),
        TokenKind::String(QuoteKind::Single).map(|t| (QuoteKind::Single, t.raw)),
    ))
    .parse_next(i)?;
    if !raw.contains("\\") {
        return Ok(raw.to_string());
    }
    let mut result = match kind {
        QuoteKind::Double => raw.replace("\\\"", "\""),
        QuoteKind::Single => raw.replace("\\'", "'"),
    };
    result = result
        .replace("\\n", "\n")
        .replace("\\r", "\r")
        .replace("\\t", "\t")
        .replace("\\\\", "\\");
    Ok(result)
}

fn identifier<'i>(i: &mut Tokens<'i>) -> ModalResult<String> {
    skip_newlines(i)?;
    Ok(TokenKind::Identifier.parse_next(i)?.raw.to_string())
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

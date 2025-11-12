use crate::lexer::tokens::{QuoteKind, Token, TokenKind};
use winnow::combinator::{alt, opt};
use winnow::error::{ContextError, ErrMode, ParseError};
use winnow::stream::TokenSlice;
use winnow::{ModalResult, Parser};

mod document;
mod error;
mod expression;
mod operator;
mod statement;
#[cfg(test)]
mod tests;

pub(crate) type Tokens<'i> = TokenSlice<'i, Token<'i>>;

use crate::parser::document::Document;
pub use crate::parser::error::ActionScriptError;

pub fn parse_document<'a>(
    source: &'a [Token],
) -> winnow::Result<Document, ParseError<Tokens<'a>, ContextError>> {
    document::document.parse(TokenSlice::new(source))
}

fn string(i: &mut Tokens<'_>) -> ModalResult<String> {
    // TODO decode
    Ok(alt((
        TokenKind::String(QuoteKind::Double),
        TokenKind::String(QuoteKind::Single),
    ))
    .parse_next(i)?
    .raw
    .to_string())
}

fn identifier(i: &mut Tokens<'_>) -> ModalResult<String> {
    Ok(TokenKind::Identifier.parse_next(i)?.raw.to_string())
}

fn skip_newline<'i, O, P>(mut inner: P) -> impl Parser<Tokens<'i>, O, ErrMode<ContextError>>
where
    P: Parser<Tokens<'i>, O, ErrMode<ContextError>>,
{
    move |input: &mut Tokens<'i>| {
        opt(TokenKind::Newline).parse_next(input)?;
        inner.parse_next(input)
    }
}

use crate::lexer::tokens::Token;
use winnow::Parser;
use winnow::error::{ContextError, ParseError};
use winnow::stream::TokenSlice;

mod actions;
mod error;
#[cfg(test)]
mod tests;

pub(crate) type Tokens<'i> = TokenSlice<'i, Token<'i>>;

pub use crate::parser::error::PCodeError;
use crate::pcode::Actions;

pub fn parse_actions<'a>(
    source: &'a [Token],
) -> winnow::Result<Actions, ParseError<Tokens<'a>, ContextError>> {
    actions::actions.parse(TokenSlice::new(source))
}

use crate::internal::as2_pcode::lexer::tokens::Token;
use winnow::Parser;
use winnow::error::{ContextError, ParseError};
use winnow::stream::TokenSlice;

mod actions;
mod error;
#[cfg(test)]
mod tests;

pub type Tokens<'i> = TokenSlice<'i, Token<'i>>;

pub use crate::internal::as2_pcode::parser::error::PCodeError;
pub use crate::internal::as2_pcode::pcode::Actions;

pub fn parse_actions<'a>(
    source: &'a [Token],
) -> winnow::Result<Actions, ParseError<Tokens<'a>, ContextError>> {
    actions::actions(false).parse(TokenSlice::new(source))
}

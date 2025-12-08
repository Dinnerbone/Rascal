use crate::ast::{Statement, StatementKind};
use crate::lexer::tokens::{Keyword, TokenKind};
use crate::parser::{Tokens, identifier};
use rascal_common::span::{Span, Spanned};
use winnow::combinator::{opt, separated};
use winnow::{ModalResult, Parser};

pub(crate) fn class<'i>(i: &mut Tokens<'i>) -> ModalResult<Statement<'i>> {
    let name = identifier.parse_next(i)?;
    // It's `extends` then `implements` - the other way around is not supported by Flash
    let extends =
        opt((TokenKind::Keyword(Keyword::Extends), identifier).map(|(_, id)| id)).parse_next(i)?;
    let implements: Option<Vec<Spanned<&'i str>>> = opt((
        TokenKind::Keyword(Keyword::Implements),
        separated(1.., identifier, TokenKind::Comma),
    )
        .map(|(_, id)| id))
    .parse_next(i)?;

    TokenKind::OpenBrace.parse_next(i)?;
    let end = TokenKind::CloseBrace.parse_next(i)?.span;

    Ok(Statement::new(
        Span::encompassing(name.span, end),
        StatementKind::Class {
            name,
            extends,
            implements: implements.unwrap_or_default(),
        },
    ))
}

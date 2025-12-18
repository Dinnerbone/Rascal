use crate::ast::{ClassMember, ClassMemberAttribute, Statement, StatementKind};
use crate::lexer::tokens::{Keyword, TokenKind};
use crate::parser::statement::{declaration, function};
use crate::parser::{Tokens, identifier, skip_newlines};
use rascal_common::span::{Span, Spanned};
use std::collections::HashMap;
use winnow::combinator::{fail, opt, peek, separated};
use winnow::error::{ParserError, StrContext};
use winnow::token::any;
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
    skip_newlines.parse_next(i)?;
    let mut members = vec![];
    let mut attributes = HashMap::new();
    loop {
        let next = peek(any).parse_next(i)?;
        match next.kind {
            TokenKind::CloseBrace => break,
            TokenKind::Semicolon | TokenKind::Newline => {
                any.parse_next(i)?;
            }
            TokenKind::Keyword(Keyword::Function) => {
                members.push((
                    function
                        .map(|f| Spanned::new(f.span, ClassMember::Function(f.value)))
                        .parse_next(i)?,
                    std::mem::take(&mut attributes),
                ));
            }
            TokenKind::Keyword(Keyword::Var) => {
                let start = TokenKind::Keyword(Keyword::Var).parse_next(i)?.span;
                let declaration = declaration.parse_next(i)?;
                members.push((
                    Spanned::new(
                        Span::encompassing(start, declaration.span),
                        ClassMember::Variable(declaration.value),
                    ),
                    std::mem::take(&mut attributes),
                ));
            }
            TokenKind::Keyword(Keyword::Static)
                if !attributes.contains_key(&ClassMemberAttribute::Static) =>
            {
                let span = TokenKind::Keyword(Keyword::Static).parse_next(i)?.span;
                attributes.insert(ClassMemberAttribute::Static, span);
            }
            _ => {
                fail.context(StrContext::Expected(
                    TokenKind::Keyword(Keyword::Function).expected(),
                ))
                .parse_next(i)?;
                unreachable!()
            }
        }
        skip_newlines.parse_next(i)?;
    }
    if !attributes.is_empty() {
        return Err(ParserError::from_input(i));
    }
    let end = TokenKind::CloseBrace.parse_next(i)?.span;

    Ok(Statement::new(
        Span::encompassing(name.span, end),
        StatementKind::Class {
            name,
            extends,
            implements: implements.unwrap_or_default(),
            members,
        },
    ))
}

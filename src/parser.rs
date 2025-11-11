use crate::lexer::tokens::{Token, TokenKind};
use annotate_snippets::renderer::DecorStyle;
use annotate_snippets::{AnnotationKind, Renderer};
use winnow::combinator::opt;
use winnow::error::{ContextError, ErrMode, ParseError};
use winnow::stream::TokenSlice;
use winnow::{ModalResult, Parser};

mod document;
mod expr;
mod statement;
#[cfg(test)]
mod tests;

pub(crate) type Tokens<'i> = TokenSlice<'i, Token<'i>>;

use crate::parser::document::Document;

#[derive(Debug)]
pub struct ActionScriptError<'a> {
    filename: &'a str,
    source: &'a str,
    error: ParseError<Tokens<'a>, ContextError>,
}

impl<'a> ActionScriptError<'a> {
    pub(crate) fn from_parse(
        filename: &'a str,
        source: &'a str,
        error: ParseError<Tokens<'a>, ContextError>,
    ) -> Self {
        Self {
            filename,
            source,
            error,
        }
    }
}

impl std::fmt::Display for ActionScriptError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tokens = self.error.input();
        let mut source = annotate_snippets::Snippet::source(self.source).path(self.filename);
        if let Some(bad_token) = tokens.get(self.error.offset()) {
            source = source
                .annotation(AnnotationKind::Primary.span(bad_token.span.start..bad_token.span.end));
        }
        let report = &[annotate_snippets::Level::ERROR
            .primary_title(self.error.inner().to_string())
            .element(source)];
        let renderer = Renderer::styled().decor_style(DecorStyle::Unicode);
        renderer.render(report).fmt(f)
    }
}

impl std::error::Error for ActionScriptError<'_> {}

pub fn parse_document<'a>(
    source: &'a [Token],
) -> winnow::Result<Document, ParseError<Tokens<'a>, ContextError>> {
    document::document.parse(TokenSlice::new(source))
}

fn string(i: &mut Tokens<'_>) -> ModalResult<String> {
    Ok(TokenKind::String.parse_next(i)?.raw.to_string())
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

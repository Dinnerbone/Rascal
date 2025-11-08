use crate::lexer::tokens::Token;
use annotate_snippets::renderer::DecorStyle;
use annotate_snippets::{AnnotationKind, Renderer};
use winnow::Parser;
use winnow::error::{ContextError, ParseError};
use winnow::stream::TokenSlice;

mod document;
mod expr;
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

use crate::parser::Tokens;
use annotate_snippets::{Annotation, AnnotationKind};
use rascal_common::span::Span;
use winnow::error::{ContextError, ParseError};

#[derive(Debug)]
pub struct ParsingError {
    pub error: String,
    pub span: Span,
}

impl ParsingError {
    pub(crate) fn new(error: String, span: Span) -> Self {
        Self { error, span }
    }

    pub fn annotation<'a>(&'a self) -> Annotation<'a> {
        AnnotationKind::Primary.span(self.span.start..self.span.end)
    }
}

impl From<ParseError<Tokens<'_>, ContextError>> for ParsingError {
    fn from(error: ParseError<Tokens, ContextError>) -> Self {
        Self {
            error: error.inner().to_string(),
            span: error
                .input()
                .get(error.offset())
                .map(|t| t.span)
                .unwrap_or_default(),
        }
    }
}

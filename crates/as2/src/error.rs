use crate::parser::Tokens;
use annotate_snippets::renderer::DecorStyle;
use annotate_snippets::{Annotation, AnnotationKind, Group, Renderer};
use indexmap::IndexMap;
use rascal_common::span::Span;
use winnow::error::{ContextError, ParseError};

#[derive(Debug)]
pub(crate) struct ParsingError {
    error: String,
    span: Span,
}

impl ParsingError {
    pub(crate) fn new(error: String, span: Span) -> Self {
        Self { error, span }
    }

    fn annotation<'a>(&'a self) -> Annotation<'a> {
        AnnotationKind::Primary
            .span(self.span.start..self.span.end)
            .label(&self.error)
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

#[derive(Debug)]
pub(crate) struct ErrorSet {
    files: IndexMap<String, (String, Vec<ParsingError>)>,
    io_errors: Vec<(String, std::io::Error)>,
}

impl ErrorSet {
    pub fn new() -> Self {
        Self {
            files: IndexMap::new(),
            io_errors: vec![],
        }
    }

    pub fn add_parsing_error(&mut self, filename: &str, source: &str, error: ParsingError) {
        self.files
            .entry(filename.to_owned())
            .or_insert_with(|| (source.to_owned(), vec![]))
            .1
            .push(error);
    }

    pub fn add_io_error(&mut self, filename: &str, error: std::io::Error) {
        self.io_errors.push((filename.to_owned(), error));
    }

    pub fn report<'a>(&'a self) -> Vec<Group<'a>> {
        let mut report = vec![];
        for (filename, (source, errors)) in &self.files {
            let mut source = annotate_snippets::Snippet::source(source).path(filename);
            for error in errors {
                source = source.annotation(error.annotation());
            }
            report.push(
                annotate_snippets::Level::ERROR
                    .primary_title("Compile error(s)")
                    .element(source),
            )
        }
        for (filename, error) in &self.io_errors {
            report.push(
                annotate_snippets::Level::ERROR
                    .primary_title(error.to_string())
                    .element(annotate_snippets::Origin::path(filename)),
            )
        }
        report
    }

    pub fn error_unless_empty(self) -> Result<(), Error> {
        if self.io_errors.is_empty() && self.files.is_empty() {
            return Ok(());
        }
        Err(Error(self))
    }
}

#[derive(Debug)]
pub struct Error(pub(crate) ErrorSet);

impl Error {
    pub fn to_string_plain(&self) -> String {
        let report = self.0.report();
        let renderer = Renderer::plain().decor_style(DecorStyle::Unicode);
        renderer.render(&report)
    }

    pub fn to_string_styled(&self) -> String {
        let report = self.0.report();
        let renderer = Renderer::styled().decor_style(DecorStyle::Unicode);
        renderer.render(&report)
    }
}

#[cfg(test)]
impl serde::Serialize for Error {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if cfg!(test) {
            self.to_string_plain().fmt(f)
        } else {
            self.to_string_styled().fmt(f)
        }
    }
}

impl std::error::Error for Error {}

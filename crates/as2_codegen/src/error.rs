use annotate_snippets::renderer::DecorStyle;
use annotate_snippets::{Annotation, AnnotationKind, Renderer};
use ruasc_common::span::Span;

#[derive(Debug)]
pub(crate) struct Error {
    pub(crate) message: &'static str,
    pub(crate) span: Span,
}

impl Error {
    fn annotation(&self) -> Annotation<'static> {
        AnnotationKind::Primary
            .span(self.span.start..self.span.end)
            .label(self.message)
    }
}

#[derive(Debug)]
pub struct CompileError<'i> {
    filename: &'i str,
    source: &'i str,
    errors: Vec<Error>,
}

impl<'i> CompileError<'i> {
    pub(crate) fn from_errors(filename: &'i str, source: &'i str, errors: Vec<Error>) -> Self {
        Self {
            filename,
            source,
            errors,
        }
    }
}

impl std::fmt::Display for CompileError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut source = annotate_snippets::Snippet::source(self.source).path(self.filename);
        for error in &self.errors {
            source = source.annotation(error.annotation());
        }
        let report = &[annotate_snippets::Level::ERROR
            .primary_title("Compile error(s)")
            .element(source)];
        let renderer = if cfg!(test) {
            Renderer::styled().decor_style(DecorStyle::Unicode)
        } else {
            Renderer::plain().decor_style(DecorStyle::Unicode)
        };
        renderer.render(report).fmt(f)
    }
}

impl std::error::Error for CompileError<'_> {}

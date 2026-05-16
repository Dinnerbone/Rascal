use crate::internal::as2::error::ParsingError;
use crate::internal::span::FileId;
use crate::sources::SourceSet;
use annotate_snippets::renderer::DecorStyle;
use annotate_snippets::{Group, Renderer};
use indexmap::IndexMap;

#[derive(Debug)]
pub(crate) struct ErrorSet {
    parsing_errors: IndexMap<Option<FileId>, Vec<ParsingError>>,
    io_errors: Vec<(String, std::io::Error)>,
    misc_errors: Vec<String>,
}

impl ErrorSet {
    pub fn new() -> Self {
        Self {
            parsing_errors: IndexMap::new(),
            io_errors: vec![],
            misc_errors: vec![],
        }
    }

    pub fn add_parsing_error(&mut self, error: ParsingError) {
        self.parsing_errors
            .entry(error.span.file)
            .or_default()
            .push(error);
    }

    pub fn add_io_error(&mut self, filename: &str, error: std::io::Error) {
        self.io_errors.push((filename.to_owned(), error));
    }

    pub fn add_misc_error(&mut self, error: String) {
        self.misc_errors.push(error);
    }

    pub fn report<'a>(&'a self, source_set: &'a SourceSet) -> Vec<Group<'a>> {
        let mut report = vec![];
        for (file_id, errors) in &self.parsing_errors {
            let source_file = file_id.and_then(|id| source_set.get_source(id));

            if let Some(source_file) = source_file {
                let mut source =
                    annotate_snippets::Snippet::source(&source_file.source).path(&source_file.path);
                for error in errors {
                    source = source.annotation(error.annotation());
                }
                report.push(
                    annotate_snippets::Level::ERROR
                        .primary_title("Compile error")
                        .element(source),
                )
            } else {
                let mut group = Group::with_title(
                    annotate_snippets::Level::ERROR.primary_title("Compile error"),
                );
                for error in errors {
                    group = group.element(annotate_snippets::Level::ERROR.message(&error.error));
                }
                report.push(group);
            }
        }

        for (filename, error) in &self.io_errors {
            report.push(
                annotate_snippets::Level::ERROR
                    .primary_title(error.to_string())
                    .element(annotate_snippets::Origin::path(filename)),
            )
        }
        for error in &self.misc_errors {
            report.push(
                annotate_snippets::Level::ERROR
                    .primary_title(error.clone())
                    .element(annotate_snippets::Level::ERROR.message(error.clone())),
            );
        }
        report
    }

    pub fn error_unless_empty(self, source_set: SourceSet) -> Result<(), Error> {
        if self.io_errors.is_empty()
            && self.parsing_errors.is_empty()
            && self.misc_errors.is_empty()
        {
            return Ok(());
        }
        Err(Error {
            error_set: Box::new(self),
            source_set: Box::new(source_set),
        })
    }
}

#[derive(Debug)]
pub struct Error {
    pub(crate) error_set: Box<ErrorSet>,
    pub(crate) source_set: Box<SourceSet>,
}

impl Error {
    pub fn to_string_plain(&self) -> String {
        let report = self.error_set.report(&self.source_set);
        let renderer = Renderer::plain().decor_style(DecorStyle::Unicode);
        renderer.render(&report)
    }

    pub fn to_string_styled(&self) -> String {
        let report = self.error_set.report(&self.source_set);
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

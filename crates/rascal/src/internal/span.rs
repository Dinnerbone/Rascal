use serde::ser::SerializeStruct;
use serde::{Serialize, Serializer};
use std::fmt;
use std::ops::Deref;

pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}

impl<T> Spanned<T> {
    pub fn new(span: Span, value: T) -> Self {
        Self { span, value }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> fmt::Debug for Spanned<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Spanned")
            .field("span", &self.span)
            .field("value", &self.value)
            .finish()
    }
}

impl<T> Clone for Spanned<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        Self {
            span: self.span,
            value: self.value.clone(),
        }
    }
}

impl<T> Copy for Spanned<T> where T: Copy {}

impl<T> Serialize for Spanned<T>
where
    T: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut s = serializer.serialize_struct("Spanned", 2)?;
        s.serialize_field("span", &self.span)?;
        s.serialize_field("value", &self.value)?;
        s.end()
    }
}

impl<T> PartialEq for Spanned<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.span == other.span && self.value == other.value
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Serialize)]
pub struct FileId(pub(crate) usize);

impl FileId {
    pub const fn new(value: usize) -> Self {
        Self(value)
    }
}

impl From<usize> for FileId {
    fn from(value: usize) -> Self {
        Self(value)
    }
}

#[derive(Copy, Clone, Default, PartialEq, Eq, Hash, Serialize)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub file: Option<FileId>,
}

impl Span {
    pub fn new_unchecked(start: usize, end: usize, file: FileId) -> Self {
        Self {
            start,
            end,
            file: Some(file),
        }
    }

    pub fn encompassing(a: Span, b: Span) -> Self {
        if a.file != b.file {
            panic!("Cannot encompass spans from different files");
        }
        let start = a.start.min(b.start);
        let end = a.end.max(b.end);
        let file = a.file;
        Self { start, end, file }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (self.start..self.end).fmt(f)
    }
}

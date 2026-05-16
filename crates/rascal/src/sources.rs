use crate::SourceProvider;
use crate::internal::span::FileId;
use std::fmt::Debug;

#[derive(Debug, Default)]
pub(crate) struct SourceSet {
    sources: Vec<SourceFile>,
}

impl SourceSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_or_load<P: SourceProvider>(
        &mut self,
        path: String,
        provider: &P,
    ) -> Result<&SourceFile, std::io::Error> {
        if let Some(id) = self.sources.iter().position(|source| source.path == path) {
            return Ok(&self.sources[id]);
        }

        let source = provider.load(&path)?;
        let file_id = FileId::new(self.sources.len());
        self.sources.push(SourceFile {
            path,
            file_id,
            source,
        });
        let last = self.sources.len() - 1;
        Ok(&self.sources[last])
    }

    pub fn get_source(&self, file_id: FileId) -> Option<&SourceFile> {
        self.sources.get(file_id.0)
    }
}

pub(crate) struct SourceFile {
    pub path: String,
    pub file_id: FileId,
    pub source: String,
}

impl Debug for SourceFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SourceFile")
            .field("path", &self.path)
            .finish()
    }
}

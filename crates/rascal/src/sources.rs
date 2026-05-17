use crate::SourceProvider;
use crate::internal::span::FileId;
use std::fmt::Debug;
use std::sync::Arc;

#[derive(Debug, Default)]
pub(crate) struct SourceSet {
    sources: Vec<Arc<SourceFile>>,
}

impl SourceSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_or_load<P: SourceProvider + ?Sized>(
        &mut self,
        path: String,
        provider: &P,
    ) -> Result<Arc<SourceFile>, std::io::Error> {
        if let Some(id) = self.sources.iter().position(|source| source.path == path) {
            return Ok(self.sources[id].clone());
        }

        let source = provider.load(&path)?;
        let file_id = FileId::new(self.sources.len());
        let source_file = Arc::new(SourceFile {
            path,
            file_id,
            source,
        });
        self.sources.push(source_file.clone());
        Ok(source_file)
    }

    pub fn get_source(&self, file_id: FileId) -> Option<&Arc<SourceFile>> {
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

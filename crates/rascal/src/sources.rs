use crate::SourceProvider;
use crate::internal::span::FileId;
use std::fmt::Debug;
use std::rc::Rc;

#[derive(Debug, Default)]
pub(crate) struct SourceSet {
    sources: Vec<Rc<SourceFile>>,
}

impl SourceSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_or_load<P: SourceProvider + ?Sized>(
        &mut self,
        path: String,
        provider: &P,
    ) -> Result<Rc<SourceFile>, std::io::Error> {
        if let Some(id) = self.sources.iter().position(|source| source.path == path) {
            return Ok(self.sources[id].clone());
        }

        let source = provider.load(&path)?;
        let file_id = FileId::new(self.sources.len());
        let source_file = Rc::new(SourceFile {
            path,
            file_id,
            source,
        });
        self.sources.push(source_file.clone());
        Ok(source_file)
    }

    pub fn get_source(&self, file_id: FileId) -> Option<&Rc<SourceFile>> {
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

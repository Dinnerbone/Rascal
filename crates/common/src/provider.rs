use std::path::PathBuf;

pub trait SourceProvider {
    fn load(&self, path: &str) -> Result<String, std::io::Error>;

    fn is_file(&self, path: &str) -> bool;
}

pub struct FileSystemSourceProvider {
    roots: Vec<PathBuf>,
}

impl FileSystemSourceProvider {
    pub fn with_root(root: PathBuf) -> Self {
        Self::with_roots(vec![root])
    }

    pub fn with_roots(roots: Vec<PathBuf>) -> Self {
        Self { roots }
    }
}

impl SourceProvider for FileSystemSourceProvider {
    fn load(&self, path: &str) -> Result<String, std::io::Error> {
        for root in &self.roots {
            let actual_path = root.join(path);
            if actual_path.is_file() {
                return std::fs::read_to_string(actual_path);
            }
        }
        Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "File not found",
        ))
    }

    fn is_file(&self, path: &str) -> bool {
        for root in &self.roots {
            let actual_path = root.join(path);
            if actual_path.is_file() {
                return true;
            }
        }
        false
    }
}

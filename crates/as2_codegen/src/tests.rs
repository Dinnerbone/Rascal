use crate::hir_to_pcode;
use rascal_as2::program::{FileSystemSourceProvider, ProgramBuilder};

#[test]
fn test_all_samples() {
    insta::glob!("../../../samples/as2", "**/*.as", |path| {
        let src = std::fs::read_to_string(path).expect("failed to read sample");
        let filename = path.file_name().unwrap().to_string_lossy();
        let mut builder = ProgramBuilder::new(FileSystemSourceProvider::with_root(
            path.parent().unwrap().to_owned(),
        ));
        builder.add_script(&filename);
        let parsed = builder.build().unwrap();
        let result = match hir_to_pcode(&filename, &src, &parsed.initial_script) {
            Ok(v) => v.to_string(),
            Err(e) => e.to_string(),
        };
        insta::assert_snapshot!(result);
    });
}

#[test]
fn test_fail_samples() {
    insta::glob!("../../../samples/as2_errors", "**/*.as", |path| {
        let src = std::fs::read_to_string(path).expect("failed to read sample");
        let filename = path.file_name().unwrap().to_string_lossy();
        let mut builder = ProgramBuilder::new(FileSystemSourceProvider::with_root(
            path.parent().unwrap().to_owned(),
        ));
        builder.add_script(&filename);
        let parsed = builder.build().unwrap();
        let result = match hir_to_pcode(&filename, &src, &parsed.initial_script) {
            Ok(v) => v.to_string(),
            Err(e) => e.to_string(),
        };
        insta::assert_snapshot!(result);
    });
}

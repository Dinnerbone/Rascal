use crate::hir_to_pcode;
use rascal_as2::program::{FileSystemSourceProvider, ProgramBuilder};

#[test]
fn test_all_samples() {
    insta::glob!("../../../samples/as2", "**/*.as", |path| {
        let filename = path.file_name().unwrap().to_string_lossy();
        let mut builder = ProgramBuilder::new(FileSystemSourceProvider::with_root(
            path.parent().unwrap().to_owned(),
        ));
        builder.add_script(&filename);
        let parsed = builder.build().unwrap();
        let result = hir_to_pcode(&parsed.initial_script).to_string();
        insta::assert_snapshot!(result);
    });
}

#[test]
fn test_fail_samples() {
    insta::glob!("../../../samples/as2_errors", "**/*.as", |path| {
        let filename = path.file_name().unwrap().to_string_lossy();
        let mut builder = ProgramBuilder::new(FileSystemSourceProvider::with_root(
            path.parent().unwrap().to_owned(),
        ));
        builder.add_script(&filename);
        let result = match builder.build() {
            Ok(parsed) => hir_to_pcode(&parsed.initial_script).to_string(),
            Err(e) => e.to_string_plain(),
        };
        insta::assert_snapshot!(result);
    });
}

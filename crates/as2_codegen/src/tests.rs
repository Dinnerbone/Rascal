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
        let result = hir_to_pcode(&parsed);
        insta::assert_yaml_snapshot!(result);
    });
    insta::glob!("../../../samples/as2_classes", "*.as", |path| {
        let filename = path.file_name().unwrap().to_string_lossy();
        let mut builder = ProgramBuilder::new(FileSystemSourceProvider::with_root(
            path.parent().unwrap().to_owned(),
        ));
        builder.add_class(filename.strip_suffix(".as").unwrap());
        let parsed = builder.build().unwrap();
        let result = hir_to_pcode(&parsed);
        insta::assert_yaml_snapshot!(result);
    });
}

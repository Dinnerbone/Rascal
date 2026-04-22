use crate::ProgramBuilder;
use rascal_common::provider::FileSystemSourceProvider;

#[test]
fn test_all_samples() {
    insta::glob!("../../../samples/as2", "**/*.as", |path| {
        let filename = path.file_name().unwrap().to_string_lossy();
        let mut builder = ProgramBuilder::new(FileSystemSourceProvider::with_root(
            path.parent().unwrap().to_owned(),
        ));
        builder.add_script(&filename);
        let parsed = builder.build().unwrap();
        let result = parsed.compile(15);
        insta::assert_yaml_snapshot!(result);
    });
    insta::glob!("../../../samples/as2_classes", "*.as", |path| {
        let filename = path.file_name().unwrap().to_string_lossy();
        let mut builder = ProgramBuilder::new(FileSystemSourceProvider::with_root(
            path.parent().unwrap().to_owned(),
        ));
        builder.add_class(filename.strip_suffix(".as").unwrap());
        let parsed = builder.build().unwrap();
        let result = parsed.compile(15);
        insta::assert_yaml_snapshot!(result);
    });
}

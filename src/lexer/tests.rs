use crate::lexer::Lexer;

#[test]
fn test_all_samples() {
    insta::glob!(
        concat!(env!("CARGO_MANIFEST_DIR"), "/samples"),
        "**/*.as",
        |path| {
            let src = std::fs::read_to_string(path).expect("failed to read sample");
            let tokens = Lexer::new(&src).into_vec();
            insta::assert_yaml_snapshot!(tokens);
        }
    );
}

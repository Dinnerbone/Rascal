use crate::lexer::Lexer;
use crate::parser::parse_document;

#[test]
fn test_all_samples() {
    insta::glob!(
        concat!(env!("CARGO_MANIFEST_DIR"), "/samples"),
        "**/*.as",
        |path| {
            let src = std::fs::read_to_string(path).expect("failed to read sample");
            let tokens = Lexer::new(&src).into_vec();
            let parsed = parse_document(&tokens).map_err(|e| format!("{e:#?}"));
            insta::assert_yaml_snapshot!(parsed);
        }
    );
}

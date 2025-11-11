use super::*;
use crate::lexer::Lexer;
use crate::lexer::tokens::Token;

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

#[test]
fn test_line_comment() {
    let input = "// this is a line comment\n";
    assert_eq!(Lexer::new(input).into_vec(), Vec::<Token>::new());
}

#[test]
fn test_empty_line_comment() {
    let input = "//";
    assert_eq!(Lexer::new(input).into_vec(), Vec::<Token>::new());
}

#[test]
fn test_block_comment() {
    let input = "/* this is a block comment */";
    assert_eq!(Lexer::new(input).into_vec(), Vec::<Token>::new());
}

#[test]
fn test_non_ending_block_comment() {
    let input = "/*";
    assert_eq!(Lexer::new(input).into_vec(), Vec::<Token>::new());
}

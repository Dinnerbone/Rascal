use crate::lexer::Lexer;
use crate::lexer::tokens::{BinaryOperator, Keyword, Token, TokenKind};

fn kinds(input: &str) -> Vec<TokenKind> {
    Lexer::new(input)
        .into_vec()
        .into_iter()
        .map(|t| t.kind)
        .collect()
}

fn raws(input: &str) -> Vec<String> {
    Lexer::new(input)
        .into_vec()
        .into_iter()
        .map(|t| t.raw.to_string())
        .collect()
}

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

#[test]
fn test_identifiers_and_keyword() {
    assert_eq!(
        kinds("var x _abc $d Var"),
        vec![
            TokenKind::Keyword(Keyword::Var),
            TokenKind::Identifier,
            TokenKind::Identifier,
            TokenKind::Identifier,
            TokenKind::Identifier, // 'Var' is not the keyword (case-sensitive)
        ]
    );
}

#[test]
fn test_operators_single_and_compound() {
    assert_eq!(
        kinds("+ = += ++"),
        vec![
            TokenKind::BinaryOperator(BinaryOperator::Add),
            TokenKind::BinaryOperator(BinaryOperator::Assign),
            TokenKind::BinaryOperator(BinaryOperator::AddAssign),
            TokenKind::BinaryOperator(BinaryOperator::Add),
            TokenKind::BinaryOperator(BinaryOperator::Add),
        ]
    );
}

#[test]
fn test_punctuation_tokens() {
    assert_eq!(
        kinds("(,) );"),
        vec![
            TokenKind::OpenParen,
            TokenKind::Comma,
            TokenKind::CloseParen,
            TokenKind::CloseParen,
            TokenKind::Semicolon,
        ]
    );
}

#[test]
fn test_newlines_variants() {
    assert_eq!(kinds("\n"), vec![TokenKind::Newline]);
    assert_eq!(kinds("\r\n"), vec![TokenKind::Newline]);
    assert_eq!(kinds("\r"), vec![TokenKind::Newline]);
}

#[test]
fn test_string_literal_and_escapes() {
    assert_eq!(kinds("\"hello\""), vec![TokenKind::String]);
    assert_eq!(raws("\"hello\""), vec!["hello".to_string()]);

    let s = "\"a\\\"b\\\\c\""; // "a\"b\\c"
    assert_eq!(kinds(s), vec![TokenKind::String]);
    assert_eq!(raws(s), vec!["a\\\"b\\\\c".to_string()]);
}

#[test]
fn test_mixed_sequence() {
    let input = "var x+=y,($z);\r\n";
    assert_eq!(
        kinds(input),
        vec![
            TokenKind::Keyword(Keyword::Var),
            TokenKind::Identifier,
            TokenKind::BinaryOperator(BinaryOperator::AddAssign),
            TokenKind::Identifier,
            TokenKind::Comma,
            TokenKind::OpenParen,
            TokenKind::Identifier,
            TokenKind::CloseParen,
            TokenKind::Semicolon,
            TokenKind::Newline,
        ]
    );
}

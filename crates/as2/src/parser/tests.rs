use crate::lexer::Lexer;
use crate::lexer::tokens::{Token, TokenKind};
use crate::parser::parse_document;
use rascal_common::span::Span;

pub(crate) fn build_tokens<'i>(spec: &'i [(TokenKind, &'i str)]) -> Vec<Token<'i>> {
    spec.iter()
        .map(|(k, raw)| Token::new(*k, Span::default(), raw))
        .collect()
}

#[test]
fn test_all_samples() {
    insta::glob!("../../../../samples/as2", "**/*.as", |path| {
        let src = std::fs::read_to_string(path).expect("failed to read sample");
        let tokens = Lexer::new(&src).into_vec();
        let parsed = parse_document(&tokens).map_err(|e| format!("{e:#?}"));
        insta::assert_yaml_snapshot!(parsed);
    });
}

use crate::lexer::Lexer;
use crate::lexer::tokens::{Token, TokenKind};
use crate::parser::parse_document;
use crate::source::Span;

pub(crate) fn build_tokens<'i>(spec: &'i [(TokenKind, &'i str)]) -> Vec<Token<'i>> {
    let mut pos = 0usize;
    spec.iter()
        .map(|(k, raw)| {
            let start = pos;
            pos += raw.len().max(1);
            let end = pos;
            Token::new(*k, Span::new_unchecked(start, end), raw)
        })
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

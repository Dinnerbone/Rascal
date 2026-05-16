use crate::internal::as2_pcode::lexer::Lexer;
use crate::internal::as2_pcode::lexer::tokens::{Token, TokenKind};
use crate::internal::as2_pcode::parser::parse_actions;
use crate::internal::as2_pcode::pcode::Actions;
use crate::internal::span::{FileId, Span};

pub(crate) fn build_tokens<'a>(spec: &'a [(TokenKind, &'a str)]) -> Vec<Token<'a>> {
    let mut pos = 0usize;
    spec.iter()
        .map(|(k, raw)| {
            let start = pos;
            pos += raw.len().max(1);
            let end = pos;
            Token::new(*k, Span::new_unchecked(start, end, FileId::new(1)), raw)
        })
        .collect()
}

#[test]
fn test_all_samples() {
    insta::glob!(
        "../../../../../../samples/as2_pcode",
        "**/*.pcode",
        |path| {
            let src = std::fs::read_to_string(path).expect("failed to read sample");
            let tokens = Lexer::new(&src, FileId::new(1)).into_vec();
            let parsed = parse_actions(&tokens).map_err(|e| format!("{e:#?}"));
            insta::assert_yaml_snapshot!(parsed);
            insta::assert_snapshot!("printed", parsed.unwrap_or_else(|_| Actions::empty()));
        }
    );
}

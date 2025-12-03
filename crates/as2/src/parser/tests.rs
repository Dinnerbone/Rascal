use crate::lexer::tokens::{Token, TokenKind};
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
        let actionscript = crate::ActionScript::new("test.as", &src);
        let parsed = actionscript.to_ast().map_err(|e| format!("{e:#?}"));
        insta::assert_yaml_snapshot!(parsed);
    });
}

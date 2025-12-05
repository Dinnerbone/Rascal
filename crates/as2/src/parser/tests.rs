use crate::lexer::tokens::{Token, TokenKind};
use rascal_common::span::Span;

pub(crate) fn build_tokens<'i>(spec: &'i [(TokenKind, &'i str)]) -> Vec<Token<'i>> {
    spec.iter()
        .map(|(k, raw)| Token::new(*k, Span::default(), raw))
        .collect()
}

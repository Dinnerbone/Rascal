use crate::internal::as2::lexer::tokens::{Token, TokenKind};
use crate::internal::span::Span;

pub(crate) fn build_tokens<'i>(spec: &'i [(TokenKind, &'i str)]) -> Vec<Token<'i>> {
    spec.iter()
        .map(|(k, raw)| Token::new(*k, Span::default(), raw))
        .collect()
}

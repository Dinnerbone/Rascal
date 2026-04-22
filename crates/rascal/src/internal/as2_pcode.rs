use crate::internal::as2_pcode::lexer::Lexer;
use crate::internal::as2_pcode::lexer::tokens::Token;
use crate::internal::as2_pcode::parser::PCodeError;
pub use crate::internal::as2_pcode::pcode::Action;
pub use crate::internal::as2_pcode::pcode::Actions;
pub use crate::internal::as2_pcode::pcode::CatchTarget;
pub use crate::internal::as2_pcode::pcode::PushValue;

mod lexer;
mod parser;
mod pcode;

pub struct PCode<'a> {
    filename: &'a str,
    source: &'a str,
    tokens: Vec<Token<'a>>,
}

impl<'a> PCode<'a> {
    pub fn new(filename: &'a str, source: &'a str) -> Self {
        let tokens = Lexer::new(source).into_vec();
        Self {
            filename,
            source,
            tokens,
        }
    }

    pub fn to_actions(&'a self) -> Result<Actions, PCodeError<'a>> {
        parser::parse_actions(&self.tokens)
            .map_err(|e| PCodeError::from_parse(self.filename, self.source, e))
    }
}

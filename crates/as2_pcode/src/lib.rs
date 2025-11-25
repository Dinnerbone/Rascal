use crate::lexer::Lexer;
use crate::lexer::tokens::Token;
use crate::parser::PCodeError;
pub use crate::pcode::Action;
pub use crate::pcode::Actions;
pub use crate::pcode::CatchTarget;
pub use crate::pcode::PushValue;

mod lexer;
mod parser;
mod pcode;
mod swf;

pub use swf::pcode_to_swf;

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

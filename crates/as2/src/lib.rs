use crate::lexer::Lexer;
use crate::lexer::tokens::Token;
use crate::parser::ActionScriptError;

mod ast;
pub mod hir;
mod lexer;
mod parser;
mod resolver;

use crate::resolver::resolve_hir;
pub use hir::Document;

pub struct ActionScript<'a> {
    filename: &'a str,
    source: &'a str,
    tokens: Vec<Token<'a>>,
}

impl<'a> ActionScript<'a> {
    pub fn new(filename: &'a str, source: &'a str) -> Self {
        let tokens = Lexer::new(source).into_vec();
        Self {
            filename,
            source,
            tokens,
        }
    }

    pub fn to_ast(&'a self) -> Result<Document<'a>, ActionScriptError<'a>> {
        let ast = parser::parse_document(&self.tokens)
            .map_err(|e| ActionScriptError::from_parse(self.filename, self.source, e))?;
        Ok(resolve_hir(ast))
    }
}

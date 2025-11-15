use crate::codegen::builder::CodeBuilder;
use crate::codegen::statement::gen_statements;
use crate::lexer::Lexer;
use crate::lexer::tokens::Token;
use crate::parser::PCodeError;
use crate::pcode::Actions;
use ruasc_as2::Document;

mod codegen;
mod lexer;
mod parser;
mod pcode;
mod span;

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

pub fn ast_to_pcode(source: &Document) -> Actions {
    let mut builder = CodeBuilder::new();
    gen_statements(&mut builder, &source.statements);
    builder.into_actions()
}

use crate::parser::statement::{Statement, statement};
use crate::parser::Tokens;
use serde::Serialize;
use winnow::combinator::{eof, repeat_till};
use winnow::error::StrContext;
use winnow::{ModalResult, Parser};

#[derive(Debug, Serialize)]
pub struct Document {
    #[allow(dead_code)]
    statements: Vec<Statement>,
}

pub fn document(tokens: &mut Tokens<'_>) -> ModalResult<Document> {
    let (statements, _) = repeat_till(0.., statement.context(StrContext::Label("statement")), eof)
        .parse_next(tokens)?;
    Ok(Document { statements })
}

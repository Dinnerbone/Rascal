use crate::parser::Tokens;
use crate::parser::statement::{Statement, statement_list};
use serde::Serialize;
use winnow::{ModalResult, Parser};

#[derive(Debug, Serialize)]
pub struct Document {
    #[allow(dead_code)]
    statements: Vec<Statement>,
}

pub fn document(tokens: &mut Tokens<'_>) -> ModalResult<Document> {
    let statements = statement_list(false).parse_next(tokens)?;
    Ok(Document { statements })
}

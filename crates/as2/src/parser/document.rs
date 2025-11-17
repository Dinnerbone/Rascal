use crate::ast::Document;
use crate::parser::Tokens;
use crate::parser::statement::statement_list;
use winnow::{ModalResult, Parser};

pub fn document(tokens: &mut Tokens<'_>) -> ModalResult<Document> {
    let statements = statement_list(false).parse_next(tokens)?;
    Ok(Document { statements })
}

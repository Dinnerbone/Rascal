use crate::internal::as2::ast::Document;
use crate::internal::as2::parser::Tokens;
use crate::internal::as2::parser::statement::statement_list;
use winnow::{ModalResult, Parser};

pub fn document<'i>(tokens: &mut Tokens<'i>) -> ModalResult<Document<'i>> {
    let statements = statement_list(false).parse_next(tokens)?;
    Ok(Document { statements })
}

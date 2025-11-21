use crate::builder::CodeBuilder;
use crate::constants::Constants;
use crate::error::CompileError;
use crate::statement::gen_statements;
use ruasc_as2::Document;
use ruasc_as2_pcode::{Action, Actions};

mod access;
mod builder;
mod constants;
mod error;
mod special_functions;
mod statement;
#[cfg(test)]
mod tests;

pub fn ast_to_pcode<'a>(
    filename: &'a str,
    source: &'a str,
    ast: &Document<'a>,
) -> Result<Actions, CompileError<'a>> {
    let mut constants = Constants::empty();
    let mut builder = CodeBuilder::new(&mut constants);
    builder.action(Action::ConstantPool(vec![])); // Reserve space for the constant pool
    gen_statements(&mut builder, &ast.statements);
    let (mut actions, errors) = builder.into_actions();
    actions.replace_action(0, Action::ConstantPool(constants.into_iter().collect()));

    if errors.is_empty() {
        Ok(actions)
    } else {
        Err(CompileError::from_errors(filename, source, errors))
    }
}

use crate::builder::CodeBuilder;
use crate::constants::Constants;
use crate::statement::gen_statements;
use ruasc_as2::Document;
use ruasc_as2_pcode::{Action, Actions};

mod access;
mod builder;
mod constants;
mod statement;
#[cfg(test)]
mod tests;

pub fn ast_to_pcode(source: &Document) -> Actions {
    let mut constants = Constants::empty();
    let mut builder = CodeBuilder::new(&mut constants);
    builder.action(Action::ConstantPool(vec![])); // Reserve space for the constant pool
    gen_statements(&mut builder, &source.statements);
    let mut actions = builder.into_actions();
    actions.replace_action(0, Action::ConstantPool(constants.into_iter().collect()));

    actions
}

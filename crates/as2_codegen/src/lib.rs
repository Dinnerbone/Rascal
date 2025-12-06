use crate::builder::CodeBuilder;
use crate::context::ScriptContext;
use crate::statement::gen_statements;
use rascal_as2::hir::StatementKind;
use rascal_as2_pcode::{Action, Actions};

mod access;
mod builder;
mod constants;
mod context;
mod special_properties;
mod statement;
#[cfg(test)]
mod tests;

pub fn hir_to_pcode(statements: &[StatementKind]) -> Actions {
    let mut context = ScriptContext::new();
    let mut builder = CodeBuilder::new();
    builder.action(Action::ConstantPool(vec![])); // Reserve space for the constant pool
    gen_statements(&mut context, &mut builder, statements);
    let mut actions = builder.into_actions();
    actions.replace_action(
        0,
        Action::ConstantPool(context.constants.into_iter().collect()),
    );

    actions
}

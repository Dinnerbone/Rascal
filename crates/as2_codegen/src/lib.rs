use crate::builder::CodeBuilder;
use crate::context::ScriptContext;
use crate::statement::gen_statements;
use rascal_as2::hir::{Interface, StatementKind};
use rascal_as2::program::Program;
use rascal_as2_pcode::{Action, Actions, CompiledProgram};

mod access;
mod builder;
mod constants;
mod context;
mod special_properties;
mod statement;
#[cfg(test)]
mod tests;

pub fn hir_to_pcode(program: &Program) -> CompiledProgram {
    let initializer = if program.initial_script.is_empty() {
        None
    } else {
        Some(script_to_actions(&program.initial_script))
    };
    let mut extra_modules = vec![];
    for interface in &program.interfaces {
        let actions = interface_to_actions(interface);
        extra_modules.push((interface.name.to_owned(), actions));
    }
    CompiledProgram {
        initializer,
        extra_modules,
    }
}

fn script_to_actions(statements: &[StatementKind]) -> Actions {
    generate_actions(|context, builder| {
        gen_statements(context, builder, statements);
    })
}

fn interface_to_actions(interface: &Interface) -> Actions {
    generate_actions(|context, builder| {
        // First check that it doesn't already exist
        let end = context.create_label();
        builder.push("_global");
        builder.action(Action::GetVariable);
        builder.push(interface.name.as_str());
        builder.action(Action::GetMember);
        builder.action(Action::Not);
        builder.action(Action::Not);
        builder.action(Action::If(end.clone()));

        // Actually define the interface (it's actually just an empty function)
        builder.push("_global");
        builder.action(Action::GetVariable);
        builder.push(interface.name.as_str());
        builder.action(Action::DefineFunction {
            name: "".to_string(),
            params: vec![],
            actions: Actions::empty(),
        });
        builder.action(Action::SetMember);

        // If we extend something, set that association too
        // TODO: This doesn't handle paths yet ("foo.bar.Baz")
        if let Some(extends) = &interface.extends {
            builder.push("_global");
            builder.action(Action::GetVariable);
            builder.push(extends.as_str());
            builder.action(Action::GetMember);
            builder.push(1);
            builder.push("_global");
            builder.action(Action::GetVariable);
            builder.push(interface.name.as_str());
            builder.action(Action::GetMember);
            builder.action_with_stack_delta(Action::ImplementsOp, -3);
        }

        // Done
        builder.mark_label(end);
        builder.action(Action::Pop); // This is one pop too many - but that's fine, it's what Flash does
    })
}

fn generate_actions<F>(f: F) -> Actions
where
    F: FnOnce(&mut ScriptContext, &mut CodeBuilder),
{
    let mut context = ScriptContext::new();
    let mut builder = CodeBuilder::new();
    builder.action(Action::ConstantPool(vec![])); // Reserve space for the constant pool
    f(&mut context, &mut builder);
    let mut actions = builder.into_actions();
    actions.replace_action(
        0,
        Action::ConstantPool(context.constants.into_iter().collect()),
    );

    actions
}

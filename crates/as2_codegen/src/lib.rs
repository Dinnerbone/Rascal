use crate::builder::CodeBuilder;
use crate::context::ScriptContext;
use crate::statement::gen_statements;
use rascal_as2::hir::{Class, Interface, StatementKind};
use rascal_as2::program::Program;
use rascal_as2_pcode::{Action, Actions, CompiledProgram, PushValue};

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
    for class in &program.classes {
        let actions = class_to_actions(class);
        extra_modules.push((class.name.to_owned(), actions));
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

fn class_to_actions(class: &Class) -> Actions {
    generate_actions(|context, builder| {
        // First check that it doesn't already exist
        let end = context.create_label();
        builder.push("_global");
        builder.action(Action::GetVariable);
        builder.push(class.name.as_str());
        builder.action(Action::GetMember);
        builder.action(Action::Not);
        builder.action(Action::Not);
        builder.action(Action::If(end.clone()));

        // Define the constructor (that's the class object, kinda)
        builder.push("_global");
        builder.action(Action::GetVariable);
        builder.push(class.name.as_str());
        let mut constructor = CodeBuilder::new();
        // TODO this should be done for HIR
        if class.extends.is_some() {
            constructor.push(0);
            constructor.push("super");
            constructor.action_with_stack_delta(Action::CallFunction, -2);
            constructor.action(Action::Pop);
        }
        builder.action(Action::DefineFunction {
            name: "".to_string(),
            params: vec![],
            actions: constructor.into_actions(),
        });
        builder.action(Action::StoreRegister(1));
        builder.action(Action::SetMember);

        // If we extend something, set that association too
        // TODO: This doesn't handle paths yet ("foo.bar.Baz")
        if let Some(extends) = &class.extends {
            builder.push("_global");
            builder.action(Action::GetVariable);
            builder.push(class.name.as_str());
            builder.action(Action::GetMember);
            builder.push(extends.as_str());
            builder.action(Action::GetVariable); // Not sure why this isn't _global.name, but Flash :D
            builder.action(Action::Extends);
        }

        // Set up the prototype
        builder.push(PushValue::Register(1));
        builder.push("prototype");
        builder.action(Action::GetMember);
        builder.action(Action::StoreRegister(2));
        builder.action(Action::Pop);

        // If we implement some interfaces, set those associations too
        // TODO: This doesn't handle paths yet ("foo.bar.Baz")
        if !class.implements.is_empty() {
            for name in &class.implements {
                builder.push("_global");
                builder.action(Action::GetVariable);
                builder.push(name.as_str());
                builder.action(Action::GetMember);
            }
            builder.push(class.implements.len() as i32);
            builder.push("_global");
            builder.action(Action::GetVariable);
            builder.push(class.name.as_str());
            builder.action(Action::GetMember);
            builder
                .action_with_stack_delta(Action::ImplementsOp, -2 - class.implements.len() as i32);
        }

        // Make the prototype not enumerable
        builder.push(1);
        builder.push(PushValue::Null);
        builder.push("_global");
        builder.action(Action::GetVariable);
        builder.push(class.name.as_str());
        builder.action(Action::GetMember);
        builder.push("prototype");
        builder.action(Action::GetMember);
        builder.push(3);
        builder.push("ASSetPropFlags");
        builder.action_with_stack_delta(Action::CallFunction, -5);

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

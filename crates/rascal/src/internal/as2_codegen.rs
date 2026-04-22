use crate::internal::as2::hir::{Class, Interface, StatementKind};
use crate::internal::as2_codegen::builder::CodeBuilder;
use crate::internal::as2_codegen::context::ScriptContext;
use crate::internal::as2_codegen::statement::{gen_expr, gen_function, gen_statements};
use crate::internal::as2_pcode::{Action, Actions, PushValue};

mod access;
mod builder;
mod constants;
mod context;
mod special_properties;
mod statement;

pub fn script_to_actions(statements: &[StatementKind]) -> Actions {
    generate_actions(|context, builder| {
        gen_statements(context, builder, statements);
    })
}

pub fn interface_to_actions(interface: &Interface) -> Actions {
    generate_actions(|context, builder| {
        let segments: Vec<&str> = interface.name.split(".").collect();
        for i in 1..=segments.len() {
            create_if_not_exists(context, builder, &segments[0..i], |_context, builder| {
                if i < segments.len() {
                    builder.push(0);
                    builder.push("Object");
                    builder.action(Action::NewObject);
                    builder.action(Action::SetMember);
                } else {
                    // Actually define the interface (it's actually just an empty function)
                    builder.action(Action::DefineFunction {
                        name: "".to_string(),
                        params: vec![],
                        actions: Actions::empty(),
                    });
                    builder.action(Action::SetMember);

                    // If we extend something, set that association too
                    if let Some(extends) = &interface.extends {
                        get_type_path(builder, extends, true);
                        builder.push(1);
                        get_type_path(builder, &interface.name, true);
                        builder.action_with_stack_delta(Action::ImplementsOp, -3);
                    }
                }
            })
        }
    })
}

fn create_if_not_exists<F>(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    path: &[&str],
    generator: F,
) where
    F: FnOnce(&mut ScriptContext, &mut CodeBuilder),
{
    let end = context.create_label();
    builder.push("_global");
    builder.action(Action::GetVariable);
    for segment in path {
        builder.push(*segment);
        builder.action(Action::GetMember);
    }
    builder.action(Action::Not);
    builder.action(Action::Not);
    builder.action(Action::If(end.clone()));

    builder.push("_global");
    builder.action(Action::GetVariable);
    if path.len() > 1 {
        for segment in &path[0..path.len() - 1] {
            builder.push(*segment);
            builder.action(Action::GetMember);
        }
    }
    builder.push(path[path.len() - 1]);

    generator(context, builder);

    builder.mark_label(end);
    builder.action(Action::Pop);
}

pub fn class_to_actions(class: &Class) -> Actions {
    generate_actions(|context, builder| {
        let segments: Vec<&str> = class.name.split(".").collect();
        for i in 1..=segments.len() {
            create_if_not_exists(context, builder, &segments[0..i], |context, builder| {
                if i < segments.len() {
                    builder.push(0);
                    builder.push("Object");
                    builder.action(Action::NewObject);
                    builder.action(Action::SetMember);
                } else {
                    // Define the constructor (that's the class object, kinda)
                    gen_function(context, builder, &class.constructor, false);
                    builder.action(Action::StoreRegister(1));
                    builder.action(Action::SetMember);

                    // If we extend something, set that association too
                    // TODO: This doesn't handle paths yet ("foo.bar.Baz")
                    if let Some(extends) = &class.extends {
                        get_type_path(builder, &class.name, false);
                        get_type_path(builder, extends, false);
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
                            get_type_path(builder, name, true);
                        }
                        builder.push(class.implements.len() as i32);
                        get_type_path(builder, &class.name, true);
                        builder.action_with_stack_delta(
                            Action::ImplementsOp,
                            -2 - class.implements.len() as i32,
                        );
                    }

                    // Functions!
                    for (name, method) in &class.functions {
                        if method.is_static {
                            builder.push(PushValue::Register(1));
                        } else {
                            builder.push(PushValue::Register(2));
                        }
                        builder.push(name.as_str());
                        gen_function(context, builder, &method.function, false);
                        builder.action(Action::SetMember);
                    }

                    // Fields!
                    for (name, field) in &class.fields {
                        if let Some(value) = &field.value {
                            if field.is_static {
                                builder.push(PushValue::Register(1));
                            } else {
                                builder.push(PushValue::Register(2));
                            }
                            builder.push(name.as_str());
                            gen_expr(context, builder, value, false);
                            builder.action(Action::SetMember);
                        }
                    }

                    // Getter/setter virtual properties!
                    for (name, virtual_property) in &class.virtual_properties {
                        let owner = if virtual_property.is_static {
                            PushValue::Register(1)
                        } else {
                            PushValue::Register(2)
                        };
                        if let Some(setter) = &virtual_property.setter {
                            builder.push(owner.clone());
                            builder.push(setter.as_str());
                            builder.action(Action::GetMember);
                        } else {
                            builder.action(Action::DefineFunction {
                                name: "".to_string(),
                                params: vec![],
                                actions: Actions::empty(),
                            })
                        }
                        if let Some(getter) = &virtual_property.getter {
                            builder.push(owner.clone());
                            builder.push(getter.as_str());
                            builder.action(Action::GetMember);
                        } else {
                            builder.action(Action::DefineFunction {
                                name: "".to_string(),
                                params: vec![],
                                actions: Actions::empty(),
                            })
                        }
                        builder.push(name.as_str());
                        builder.push(3);
                        builder.push(owner);
                        builder.push("addProperty");
                        builder.action_with_stack_delta(Action::CallMethod, -5);
                    }

                    // Make the prototype not enumerable
                    builder.push(1);
                    builder.push(PushValue::Null);
                    get_type_path(builder, &class.name, false);
                    builder.push("prototype");
                    builder.action(Action::GetMember);
                    builder.push(3);
                    builder.push("ASSetPropFlags");
                    builder.action_with_stack_delta(Action::CallFunction, -5);
                }
            })
        }
    })
}

fn get_type_path(builder: &mut CodeBuilder, type_name: &str, from_global: bool) {
    let segments: Vec<&str> = type_name.split(".").collect();
    let mut first = true;
    if from_global {
        builder.push("_global");
        builder.action(Action::GetVariable);
        first = false;
    }
    for segment in segments {
        builder.push(segment);
        if first {
            builder.action(Action::GetVariable);
            first = false;
        } else {
            builder.action(Action::GetMember);
        }
    }
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

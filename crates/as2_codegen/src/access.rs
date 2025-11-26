use crate::builder::CodeBuilder;
use crate::context::ScriptContext;
use crate::statement::gen_expr;
use rascal_as2::ast::{ConstantKind, Expr, ExprKind};
use rascal_as2_pcode::{Action, PushValue};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VariableAccess {
    Variable,
    Object,
    Direct,
}

impl VariableAccess {
    pub fn for_identifier(
        context: &mut ScriptContext,
        builder: &mut CodeBuilder,
        name: &str,
    ) -> Self {
        match name {
            "true" => {
                builder.push(true);
                VariableAccess::Direct
            }
            "false" => {
                builder.push(false);
                VariableAccess::Direct
            }
            "null" => {
                builder.push(PushValue::Null);
                VariableAccess::Direct
            }
            "undefined" => {
                builder.push(PushValue::Undefined);
                VariableAccess::Direct
            }
            name => {
                let value = context.constants.add(name);
                builder.push(value);
                VariableAccess::Variable
            }
        }
    }

    pub fn for_constant(
        context: &mut ScriptContext,
        builder: &mut CodeBuilder,
        constant: &ConstantKind,
    ) -> Self {
        match constant {
            ConstantKind::String(str) => {
                let value = context.constants.add(str);
                builder.push(value);
                VariableAccess::Direct
            }
            ConstantKind::Identifier(identifier) => {
                Self::for_identifier(context, builder, identifier)
            }
            ConstantKind::Float(value) => {
                builder.push(*value);
                VariableAccess::Direct
            }
            ConstantKind::Integer(value) => {
                builder.push(*value);
                VariableAccess::Direct
            }
        }
    }

    pub fn for_expr(context: &mut ScriptContext, builder: &mut CodeBuilder, expr: &Expr) -> Self {
        match expr {
            Expr {
                value: ExprKind::Constant(constant),
                ..
            } => Self::for_constant(context, builder, constant),
            Expr {
                value: ExprKind::Field(object, field),
                ..
            } => {
                gen_expr(context, builder, object, false);
                gen_expr(context, builder, field, false);
                VariableAccess::Object
            }
            _ => {
                gen_expr(context, builder, expr, false);
                VariableAccess::Direct
            }
        }
    }

    pub fn get_value(&self, builder: &mut CodeBuilder) {
        match self {
            VariableAccess::Variable => {
                builder.action(Action::GetVariable);
            }
            VariableAccess::Object => {
                builder.action(Action::GetMember);
            }
            VariableAccess::Direct => {}
        }
    }

    pub fn set_value(&self, builder: &mut CodeBuilder) {
        match self {
            VariableAccess::Variable => {
                builder.action(Action::SetVariable);
            }
            VariableAccess::Object => {
                builder.action(Action::SetMember);
            }
            VariableAccess::Direct => {
                unimplemented!("Cannot set a direct value")
            }
        }
    }

    pub fn get_and_set_value(&self, builder: &mut CodeBuilder) {
        match self {
            VariableAccess::Variable => {
                builder.action(Action::StoreRegister(0));
                builder.action(Action::SetVariable);
                builder.push(PushValue::Register(0));
            }
            VariableAccess::Object => {
                builder.action(Action::StoreRegister(0));
                builder.action(Action::SetMember);
                builder.push(PushValue::Register(0));
            }
            VariableAccess::Direct => {
                unimplemented!("Cannot get-and-set a direct value")
            }
        }
    }

    pub fn delete(&self, builder: &mut CodeBuilder) {
        match self {
            VariableAccess::Variable => {
                builder.action(Action::Delete2);
            }
            VariableAccess::Object => {
                builder.action(Action::Delete);
            }
            VariableAccess::Direct => {
                builder.action(Action::Delete2);
            }
        }
    }

    pub fn call_new(&self, builder: &mut CodeBuilder, num_args: i32) {
        match self {
            VariableAccess::Variable => {
                builder.action_with_stack_delta(Action::NewObject, -num_args - 1);
            }
            VariableAccess::Object => {
                builder.action_with_stack_delta(Action::NewMethod, -num_args - 2);
            }
            VariableAccess::Direct => {
                builder.action_with_stack_delta(Action::NewMethod, -num_args - 2);
            }
        }
    }

    pub fn call(&self, builder: &mut CodeBuilder, num_args: i32) {
        match self {
            VariableAccess::Variable => {
                builder.action_with_stack_delta(Action::CallFunction, -num_args - 1);
            }
            VariableAccess::Object => {
                builder.action_with_stack_delta(Action::CallMethod, -num_args - 2);
            }
            VariableAccess::Direct => {
                builder.action_with_stack_delta(Action::CallFunction, -num_args - 1);
            }
        }
    }
}

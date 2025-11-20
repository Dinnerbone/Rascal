use crate::builder::CodeBuilder;
use crate::statement::gen_expr;
use ruasc_as2::ast::{Constant, ExprKind};
use ruasc_as2_pcode::{Action, PushValue};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VariableAccess {
    Variable,
    Object,
    Direct,
}

impl VariableAccess {
    pub fn for_identifier(builder: &mut CodeBuilder, name: &str) -> Self {
        match name {
            "true" => {
                builder.action(Action::Push(vec![PushValue::True]));
                VariableAccess::Direct
            }
            "false" => {
                builder.action(Action::Push(vec![PushValue::False]));
                VariableAccess::Direct
            }
            "null" => {
                builder.action(Action::Push(vec![PushValue::Null]));
                VariableAccess::Direct
            }
            "undefined" => {
                builder.action(Action::Push(vec![PushValue::Undefined]));
                VariableAccess::Direct
            }
            name => {
                let value = builder.constants_mut().add(name);
                builder.action(Action::Push(vec![value]));
                VariableAccess::Variable
            }
        }
    }

    pub fn for_constant(builder: &mut CodeBuilder, constant: &Constant) -> Self {
        match constant {
            Constant::String(str) => {
                let value = builder.constants_mut().add(str);
                builder.action(Action::Push(vec![value]));
                VariableAccess::Direct
            }
            Constant::Identifier(identifier) => Self::for_identifier(builder, identifier),
            Constant::Float(value) => {
                builder.action(Action::Push(vec![PushValue::Float(*value)]));
                VariableAccess::Direct
            }
            Constant::Integer(value) => {
                builder.action(Action::Push(vec![PushValue::Integer(*value)]));
                VariableAccess::Direct
            }
        }
    }

    pub fn for_expr(builder: &mut CodeBuilder, expr: &ExprKind) -> Self {
        match expr {
            ExprKind::Constant(constant) => Self::for_constant(builder, constant),
            ExprKind::Field(object, field) => {
                gen_expr(builder, object, false);
                gen_expr(builder, field, false);
                VariableAccess::Object
            }
            _ => {
                gen_expr(builder, expr, false);
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
                builder.action(Action::Push(vec![PushValue::Register(0)]));
            }
            VariableAccess::Object => {
                builder.action(Action::StoreRegister(0));
                builder.action(Action::SetMember);
                builder.action(Action::Push(vec![PushValue::Register(0)]));
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

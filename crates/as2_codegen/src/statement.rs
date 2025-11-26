use crate::access::VariableAccess;
use crate::builder::CodeBuilder;
use crate::context::ScriptContext;
use crate::special_functions::gen_special_call;
use ruasc_as2::ast::{
    Affix, BinaryOperator, ConstantKind, Declaration, Expr, ExprKind, ForCondition, Function,
    StatementKind, UnaryOperator,
};
use ruasc_as2_pcode::{Action, PushValue};
use ruasc_common::span::Span;

pub(crate) fn gen_statements(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    statements: &[StatementKind],
) {
    let mut hoisted = vec![];
    let mut regular = vec![];
    for statement in statements {
        if matches!(
            statement,
            StatementKind::Expr(Expr {
                value: ExprKind::Function(Function { name: Some(_), .. }),
                ..
            })
        ) {
            hoisted.push(statement);
        } else {
            regular.push(statement);
        }
    }
    for statement in hoisted {
        gen_statement(context, builder, statement);
    }
    for statement in regular {
        gen_statement(context, builder, statement);
    }
}

pub(crate) fn gen_statement(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    statement: &StatementKind,
) {
    match statement {
        StatementKind::Declare(declarations) => {
            gen_declarations(context, builder, declarations);
        }
        StatementKind::Return(exprs) => {
            for expr in exprs {
                gen_expr(context, builder, expr, false);
            }
            if exprs.is_empty() {
                builder.push(PushValue::Undefined);
            }
            builder.action(Action::Return);
        }
        StatementKind::Throw(exprs) => {
            for expr in exprs {
                gen_expr(context, builder, expr, false);
            }
            if exprs.is_empty() {
                builder.push(PushValue::Undefined);
            }
            builder.action(Action::Throw);
        }
        StatementKind::Expr(expr) => {
            let stack_size = builder.stack_size();
            gen_expr(context, builder, expr, true);
            builder.truncate_stack(stack_size);
        }
        StatementKind::Block(statements) => {
            let stack_size = builder.stack_size();
            gen_statements(context, builder, statements);
            builder.truncate_stack(stack_size);
        }
        StatementKind::ForIn { condition, body } => gen_for_loop(context, builder, condition, body),
        StatementKind::If { condition, yes, no } => gen_if(context, builder, condition, yes, no),
        StatementKind::Break => {
            if let Some(label) = builder.break_label() {
                builder.action(Action::Jump(label));
            }
        }
        StatementKind::Continue => {
            if let Some(label) = builder.continue_label() {
                builder.action(Action::Jump(label));
            }
        }
    }
}

fn gen_for_loop(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    condition: &ForCondition,
    body: &StatementKind,
) {
    let start_stack_size = builder.stack_size();
    let end_label = builder.create_label();
    let continue_label = builder.create_label();
    let old_break = builder.set_break_label(Some(end_label.clone()));
    let old_continue = builder.set_continue_label(Some(continue_label.clone()));

    match condition {
        ForCondition::Enumerate {
            variable,
            declare,
            object,
        } => {
            gen_expr(context, builder, object, false);
            builder.action(Action::Enumerate2);
            builder.mark_label(continue_label.clone());
            builder.action(Action::StoreRegister(0));
            builder.push(PushValue::Null);
            builder.action(Action::Equals2);
            builder.action(Action::If(end_label.clone()));

            if *declare {
                let value = context.constants.add(variable);
                builder.push(value);
                builder.push(PushValue::Register(0));
                builder.action(Action::DefineLocal);
            } else {
                let access = VariableAccess::for_identifier(context, builder, variable);
                builder.push(PushValue::Register(0));
                access.set_value(builder);
            }

            gen_statement(context, builder, body);
            builder.action(Action::Jump(continue_label));
        }
        ForCondition::Classic {
            initialize,
            condition,
            update,
        } => {
            if let Some(initialize) = initialize {
                gen_statement(context, builder, initialize);
            }
            let start_label = builder.create_label();
            builder.mark_label(start_label.clone());
            if !condition.is_empty() {
                let last_cond = condition.len() - 1;
                for (i, expr) in condition.iter().enumerate() {
                    let stack_size = builder.stack_size();
                    gen_expr(context, builder, expr, i != last_cond);
                    if i != last_cond {
                        builder.truncate_stack(stack_size);
                    }
                }
                builder.action(Action::Not);
                builder.action(Action::If(end_label.clone()));
            }
            gen_statement(context, builder, body);
            builder.mark_label(continue_label.clone());
            for expr in update {
                let stack_size = builder.stack_size();
                gen_expr(context, builder, expr, true);
                builder.truncate_stack(stack_size);
            }
            builder.action(Action::Jump(start_label));
        }
    }

    builder.truncate_stack(start_stack_size);
    builder.mark_label(end_label);
    builder.set_break_label(old_break);
    builder.set_continue_label(old_continue);
}

fn gen_if(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    condition: &Expr,
    yes: &StatementKind,
    no: &Option<Box<StatementKind>>,
) {
    let end_label = builder.create_label();
    gen_expr(context, builder, condition, false);
    builder.action(Action::Not);

    if let Some(no) = no {
        let no_label = builder.create_label();
        builder.action(Action::If(no_label.clone()));
        gen_statement(context, builder, yes);
        builder.action(Action::Jump(end_label.clone()));
        builder.mark_label(no_label);
        gen_statement(context, builder, no);
    } else {
        builder.action(Action::If(end_label.clone()));
        gen_statement(context, builder, yes);
    }

    builder.mark_label(end_label);
}

fn gen_ternary(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    condition: &Expr,
    yes: &Expr,
    no: &Expr,
) {
    let end_label = builder.create_label();
    let no_label = builder.create_label();

    gen_expr(context, builder, condition, false);
    builder.action(Action::Not);
    builder.action(Action::If(no_label.clone()));
    gen_expr(context, builder, yes, false);
    builder.action(Action::Jump(end_label.clone()));
    builder.mark_label(no_label);
    gen_expr(context, builder, no, false);
    builder.mark_label(end_label);

    // -1 because we'll have assumed the stack grew twice, but the player will skip one of the expressions
    builder.assume_stack_delta(-1);
}

fn gen_declarations(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    declarations: &[Declaration],
) {
    for declaration in declarations {
        let value = context.constants.add(declaration.name);
        builder.push(value);
        if let Some(value) = &declaration.value {
            gen_expr(context, builder, value, false);
            builder.action(Action::DefineLocal);
        } else {
            builder.action(Action::DefineLocal2);
        }
    }
}

pub fn gen_expr(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    expr: &Expr,
    will_discard_result: bool,
) {
    let (span, kind) = (expr.span, &expr.value);
    match kind {
        ExprKind::Constant(constant) => {
            VariableAccess::for_constant(context, builder, constant).get_value(builder)
        }
        ExprKind::Call { name, args } => gen_call(context, builder, span, name, args),
        ExprKind::New { name, args } => gen_new(context, builder, name, args),
        ExprKind::BinaryOperator(op, left, right) => {
            gen_binary_op(context, builder, *op, left, right, will_discard_result)
        }
        ExprKind::UnaryOperator(op, exr) => {
            gen_unary_op(context, builder, *op, exr, will_discard_result)
        }
        ExprKind::Parenthesis(expr) => gen_expr(context, builder, expr, will_discard_result),
        ExprKind::Ternary { condition, yes, no } => {
            gen_ternary(context, builder, condition, yes, no)
        }
        ExprKind::InitObject(values) => gen_init_object(context, builder, values),
        ExprKind::InitArray(values) => gen_init_array(context, builder, values),
        ExprKind::Field(_, _) => {
            let access = VariableAccess::for_expr(context, builder, expr);
            access.get_value(builder);
        }
        ExprKind::TypeOf(exprs) => {
            let stack_size = builder.stack_size();
            let last = exprs.len() - 1;
            for (i, expr) in exprs.iter().enumerate() {
                builder.truncate_stack(stack_size);
                gen_expr(context, builder, expr, i != last);
            }
            builder.action(Action::TypeOf);
        }
        ExprKind::Delete(exprs) => {
            let mut access: Option<VariableAccess> = None;
            for expr in exprs {
                if let Some(access) = &mut access {
                    access.get_value(builder);
                    builder.action(Action::Pop);
                }
                access = Some(VariableAccess::for_expr(context, builder, expr));
            }
            if let Some(access) = access {
                access.delete(builder);
            }
        }
        ExprKind::Void(_) => {}
        ExprKind::Function(function) => gen_function(context, builder, function),
    }
}

fn gen_function(context: &mut ScriptContext, builder: &mut CodeBuilder, function: &Function) {
    let mut fun_builder = CodeBuilder::new();
    gen_statements(context, &mut fun_builder, &function.body);
    let (actions, errors) = fun_builder.into_actions();
    builder.add_errors(errors);
    builder.action(Action::DefineFunction {
        name: function.name.map(ToOwned::to_owned).unwrap_or_default(),
        params: function.args.iter().map(|s| s.to_string()).collect(),
        actions,
    })
}

fn gen_init_object(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    values: &[(&str, Expr)],
) {
    let num_fields = values.len() as i32;
    for (key, value) in values.iter().rev() {
        let push_value = context.constants.add(key);
        builder.push(push_value);
        gen_expr(context, builder, value, false);
    }
    builder.push(num_fields);
    builder.action_with_stack_delta(Action::InitObject, -num_fields * 2);
}

fn gen_init_array(context: &mut ScriptContext, builder: &mut CodeBuilder, values: &[Expr]) {
    let num_values = values.len() as i32;
    for value in values.iter().rev() {
        gen_expr(context, builder, value, false);
    }
    builder.push(num_values);
    builder.action_with_stack_delta(Action::InitArray, -num_values);
}

fn gen_unary_op(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    op: UnaryOperator,
    expr: &Expr,
    will_discard_result: bool,
) {
    let adjust_in_place =
        |context: &mut ScriptContext, builder: &mut CodeBuilder, action, affix| {
            if !will_discard_result && affix == Affix::Postfix {
                // Record the old value onto the stack
                VariableAccess::for_expr(context, builder, expr).get_value(builder);
            }

            // Keep the variable on the stack without retrieving its value
            let _ = VariableAccess::for_expr(context, builder, expr);

            let access = VariableAccess::for_expr(context, builder, expr);
            access.get_value(builder);
            builder.action(action);
            if !will_discard_result && affix == Affix::Prefix {
                access.get_and_set_value(builder);
            } else {
                access.set_value(builder);
            }
        };

    match op {
        UnaryOperator::Sub => match expr {
            Expr {
                value: ExprKind::Constant(ConstantKind::Integer(value)),
                ..
            } => {
                VariableAccess::for_constant(context, builder, &ConstantKind::Integer(-*value));
            }
            Expr {
                value: ExprKind::Constant(ConstantKind::Float(value)),
                ..
            } => {
                VariableAccess::for_constant(context, builder, &ConstantKind::Float(-*value));
            }
            _ => {
                VariableAccess::for_constant(context, builder, &ConstantKind::Integer(0));
                gen_expr(context, builder, expr, false);
                builder.action(Action::Subtract);
            }
        },
        UnaryOperator::BitNot => {
            gen_expr(context, builder, expr, false);
            builder.push(-1);
            builder.action(Action::BitXor);
        }
        UnaryOperator::Increment(affix) => {
            adjust_in_place(context, builder, Action::Increment, affix)
        }
        UnaryOperator::Decrement(affix) => {
            adjust_in_place(context, builder, Action::Decrement, affix)
        }
        UnaryOperator::LogicalNot => {
            gen_expr(context, builder, expr, false);
            builder.action(Action::Not);
        }
    }
}

fn gen_binary_op(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    op: BinaryOperator,
    left: &Expr,
    right: &Expr,
    will_discard_result: bool, // if we can optimise slightly by not deliberately pushing the result onto the stack
) {
    let trivial = |context: &mut ScriptContext, builder: &mut CodeBuilder, action: Action| {
        gen_expr(context, builder, left, false);
        gen_expr(context, builder, right, false);
        builder.action(action);
    };
    let trivial_assign =
        |context: &mut ScriptContext, builder: &mut CodeBuilder, action: Action| {
            let access = VariableAccess::for_expr(context, builder, left);
            trivial(context, builder, action);
            if will_discard_result {
                access.set_value(builder);
            } else {
                access.get_and_set_value(builder);
            }
        };
    match op {
        BinaryOperator::Add => trivial(context, builder, Action::Add2),
        BinaryOperator::Assign => {
            let access = VariableAccess::for_expr(context, builder, left);
            gen_expr(context, builder, right, false);
            if will_discard_result {
                access.set_value(builder);
            } else {
                access.get_and_set_value(builder);
            }
        }
        BinaryOperator::AddAssign => trivial_assign(context, builder, Action::Add2),
        BinaryOperator::Sub => trivial(context, builder, Action::Subtract),
        BinaryOperator::SubAssign => trivial_assign(context, builder, Action::Subtract),
        BinaryOperator::Divide => trivial(context, builder, Action::Divide),
        BinaryOperator::DivideAssign => trivial_assign(context, builder, Action::Divide),
        BinaryOperator::Multiply => trivial(context, builder, Action::Multiply),
        BinaryOperator::MultiplyAssign => trivial_assign(context, builder, Action::Multiply),
        BinaryOperator::Modulo => trivial(context, builder, Action::Modulo),
        BinaryOperator::ModuloAssign => trivial_assign(context, builder, Action::Modulo),
        BinaryOperator::BitAnd => trivial(context, builder, Action::BitAnd),
        BinaryOperator::BitAndAssign => trivial_assign(context, builder, Action::BitAnd),
        BinaryOperator::BitOr => trivial(context, builder, Action::BitOr),
        BinaryOperator::BitOrAssign => trivial_assign(context, builder, Action::BitOr),
        BinaryOperator::BitXor => trivial(context, builder, Action::BitXor),
        BinaryOperator::BitXorAssign => trivial_assign(context, builder, Action::BitXor),
        BinaryOperator::BitShiftLeft => trivial(context, builder, Action::BitLShift),
        BinaryOperator::BitShiftLeftAssign => trivial_assign(context, builder, Action::BitLShift),
        BinaryOperator::BitShiftRight => trivial(context, builder, Action::BitRShift),
        BinaryOperator::BitShiftRightAssign => trivial_assign(context, builder, Action::BitRShift),
        BinaryOperator::BitShiftRightUnsigned => trivial(context, builder, Action::BitURShift),
        BinaryOperator::BitShiftRightUnsignedAssign => {
            trivial_assign(context, builder, Action::BitURShift)
        }
        BinaryOperator::Equal => trivial(context, builder, Action::Equals2),
        BinaryOperator::StrictEqual => trivial(context, builder, Action::StrictEquals),
        BinaryOperator::NotEqual => {
            trivial(context, builder, Action::Equals2);
            builder.action(Action::Not)
        }
        BinaryOperator::StrictNotEqual => {
            trivial(context, builder, Action::StrictEquals);
            builder.action(Action::Not)
        }
        BinaryOperator::LessThan => trivial(context, builder, Action::Less2),
        BinaryOperator::LessThanEqual => {
            trivial(context, builder, Action::Greater);
            builder.action(Action::Not)
        }
        BinaryOperator::GreaterThan => trivial(context, builder, Action::Greater),
        BinaryOperator::GreaterThanEqual => {
            trivial(context, builder, Action::Less2);
            builder.action(Action::Not)
        }
        BinaryOperator::LogicalAnd => {
            let end_label = builder.create_label();
            gen_expr(context, builder, left, false);
            builder.action(Action::PushDuplicate);
            builder.action(Action::Not);
            builder.action(Action::If(end_label.clone()));
            builder.action(Action::Pop);
            gen_expr(context, builder, right, false);
            builder.mark_label(end_label);
        }
        BinaryOperator::LogicalOr => {
            let end_label = builder.create_label();
            gen_expr(context, builder, left, false);
            builder.action(Action::PushDuplicate);
            builder.action(Action::If(end_label.clone()));
            builder.action(Action::Pop);
            gen_expr(context, builder, right, false);
            builder.mark_label(end_label);
        }
        BinaryOperator::InstanceOf => trivial(context, builder, Action::InstanceOf),
    }
}

fn gen_call(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    span: Span,
    name: &Expr,
    args: &[Expr],
) {
    if let Expr {
        value: ExprKind::Constant(ConstantKind::Identifier(identifier)),
        ..
    } = name
        && gen_special_call(context, builder, span, identifier, args)
    {
        return;
    }

    for arg in args.iter().rev() {
        gen_expr(context, builder, arg, false);
    }
    let num_args = args.len() as i32;
    builder.push(num_args);
    VariableAccess::for_expr(context, builder, name).call(builder, num_args);
}

fn gen_new(context: &mut ScriptContext, builder: &mut CodeBuilder, name: &Expr, args: &[Expr]) {
    for arg in args.iter().rev() {
        gen_expr(context, builder, arg, false);
    }
    let num_args = args.len() as i32;
    builder.push(num_args);
    let access = VariableAccess::for_expr(context, builder, name);
    access.call_new(builder, num_args);
}

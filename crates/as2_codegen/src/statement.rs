use crate::access::VariableAccess;
use crate::builder::CodeBuilder;
use crate::special_functions::gen_special_call;
use ruasc_as2::ast::{
    Affix, BinaryOperator, ConstantKind, Declaration, Expr, ExprKind, ForCondition, Function,
    StatementKind, UnaryOperator,
};
use ruasc_as2_pcode::{Action, PushValue};
use ruasc_common::span::Span;

pub(crate) fn gen_statements(builder: &mut CodeBuilder, statements: &[StatementKind]) {
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
        gen_statement(builder, statement);
    }
    for statement in regular {
        gen_statement(builder, statement);
    }
}

pub(crate) fn gen_statement(builder: &mut CodeBuilder, statement: &StatementKind) {
    match statement {
        StatementKind::Declare(declarations) => {
            gen_declarations(builder, declarations);
        }
        StatementKind::Return(exprs) => {
            for expr in exprs {
                gen_expr(builder, expr, false);
            }
            if exprs.is_empty() {
                builder.push(PushValue::Undefined);
            }
            builder.action(Action::Return);
        }
        StatementKind::Expr(expr) => {
            let stack_size = builder.stack_size();
            gen_expr(builder, expr, true);
            builder.truncate_stack(stack_size);
        }
        StatementKind::Block(statements) => {
            let stack_size = builder.stack_size();
            gen_statements(builder, statements);
            builder.truncate_stack(stack_size);
        }
        StatementKind::ForIn { condition, body } => gen_for_loop(builder, condition, body),
        StatementKind::If { condition, yes, no } => gen_if(builder, condition, yes, no),
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

fn gen_for_loop(builder: &mut CodeBuilder, condition: &ForCondition, body: &StatementKind) {
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
            gen_expr(builder, object, false);
            builder.action(Action::Enumerate2);
            builder.mark_label(continue_label.clone());
            builder.action(Action::StoreRegister(0));
            builder.push(PushValue::Null);
            builder.action(Action::Equals2);
            builder.action(Action::If(end_label.clone()));

            if *declare {
                let value = builder.constants_mut().add(variable);
                builder.push(value);
                builder.push(PushValue::Register(0));
                builder.action(Action::DefineLocal);
            } else {
                let access = VariableAccess::for_identifier(builder, variable);
                builder.push(PushValue::Register(0));
                access.set_value(builder);
            }

            gen_statement(builder, body);
            builder.action(Action::Jump(continue_label));
        }
        ForCondition::Classic {
            initialize,
            condition,
            update,
        } => {
            if let Some(initialize) = initialize {
                gen_statement(builder, initialize);
            }
            let start_label = builder.create_label();
            builder.mark_label(start_label.clone());
            if !condition.is_empty() {
                let last_cond = condition.len() - 1;
                for (i, expr) in condition.iter().enumerate() {
                    let stack_size = builder.stack_size();
                    gen_expr(builder, expr, i != last_cond);
                    if i != last_cond {
                        builder.truncate_stack(stack_size);
                    }
                }
                builder.action(Action::Not);
                builder.action(Action::If(end_label.clone()));
            }
            gen_statement(builder, body);
            builder.mark_label(continue_label.clone());
            for expr in update {
                let stack_size = builder.stack_size();
                gen_expr(builder, expr, true);
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
    builder: &mut CodeBuilder,
    condition: &Expr,
    yes: &StatementKind,
    no: &Option<Box<StatementKind>>,
) {
    let end_label = builder.create_label();
    gen_expr(builder, condition, false);
    builder.action(Action::Not);

    if let Some(no) = no {
        let no_label = builder.create_label();
        builder.action(Action::If(no_label.clone()));
        gen_statement(builder, yes);
        builder.action(Action::Jump(end_label.clone()));
        builder.mark_label(no_label);
        gen_statement(builder, no);
    } else {
        builder.action(Action::If(end_label.clone()));
        gen_statement(builder, yes);
    }

    builder.mark_label(end_label);
}

fn gen_ternary(builder: &mut CodeBuilder, condition: &Expr, yes: &Expr, no: &Expr) {
    let end_label = builder.create_label();
    let no_label = builder.create_label();

    gen_expr(builder, condition, false);
    builder.action(Action::Not);
    builder.action(Action::If(no_label.clone()));
    gen_expr(builder, yes, false);
    builder.action(Action::Jump(end_label.clone()));
    builder.mark_label(no_label);
    gen_expr(builder, no, false);
    builder.mark_label(end_label);

    // -1 because we'll have assumed the stack grew twice, but the player will skip one of the expressions
    builder.assume_stack_delta(-1);
}

fn gen_declarations(builder: &mut CodeBuilder, declarations: &[Declaration]) {
    for declaration in declarations {
        let value = builder.constants_mut().add(declaration.name);
        builder.push(value);
        if let Some(value) = &declaration.value {
            gen_expr(builder, value, false);
            builder.action(Action::DefineLocal);
        } else {
            builder.action(Action::DefineLocal2);
        }
    }
}

pub fn gen_expr(builder: &mut CodeBuilder, expr: &Expr, will_discard_result: bool) {
    let (span, kind) = (expr.span, &expr.value);
    match kind {
        ExprKind::Constant(constant) => {
            VariableAccess::for_constant(builder, constant).get_value(builder)
        }
        ExprKind::Call { name, args } => gen_call(builder, span, name, args),
        ExprKind::New { name, args } => gen_new(builder, name, args),
        ExprKind::BinaryOperator(op, left, right) => {
            gen_binary_op(builder, *op, left, right, will_discard_result)
        }
        ExprKind::UnaryOperator(op, exr) => gen_unary_op(builder, *op, exr, will_discard_result),
        ExprKind::Parenthesis(expr) => gen_expr(builder, expr, will_discard_result),
        ExprKind::Ternary { condition, yes, no } => gen_ternary(builder, condition, yes, no),
        ExprKind::InitObject(values) => gen_init_object(builder, values),
        ExprKind::InitArray(values) => gen_init_array(builder, values),
        ExprKind::Field(_, _) => {
            let access = VariableAccess::for_expr(builder, expr);
            access.get_value(builder);
        }
        ExprKind::TypeOf(exprs) => {
            let stack_size = builder.stack_size();
            let last = exprs.len() - 1;
            for (i, expr) in exprs.iter().enumerate() {
                builder.truncate_stack(stack_size);
                gen_expr(builder, expr, i != last);
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
                access = Some(VariableAccess::for_expr(builder, expr));
            }
            if let Some(access) = access {
                access.delete(builder);
            }
        }
        ExprKind::Void(_) => {}
        ExprKind::Function(function) => gen_function(builder, function),
    }
}

fn gen_function(builder: &mut CodeBuilder, function: &Function) {
    let mut fun_builder = CodeBuilder::new(builder.constants_mut());
    gen_statements(&mut fun_builder, &function.body);
    let (actions, errors) = fun_builder.into_actions();
    builder.add_errors(errors);
    builder.action(Action::DefineFunction {
        name: function.name.map(ToOwned::to_owned).unwrap_or_default(),
        params: function.args.iter().map(|s| s.to_string()).collect(),
        actions,
    })
}

fn gen_init_object(builder: &mut CodeBuilder, values: &[(&str, Expr)]) {
    let num_fields = values.len() as i32;
    for (key, value) in values.iter().rev() {
        let push_value = builder.constants_mut().add(key);
        builder.push(push_value);
        gen_expr(builder, value, false);
    }
    builder.push(num_fields);
    builder.action_with_stack_delta(Action::InitObject, -num_fields * 2);
}

fn gen_init_array(builder: &mut CodeBuilder, values: &[Expr]) {
    let num_values = values.len() as i32;
    for value in values.iter().rev() {
        gen_expr(builder, value, false);
    }
    builder.push(num_values);
    builder.action_with_stack_delta(Action::InitArray, -num_values);
}

fn gen_unary_op(
    builder: &mut CodeBuilder,
    op: UnaryOperator,
    expr: &Expr,
    will_discard_result: bool,
) {
    let adjust_in_place = |builder: &mut CodeBuilder, action, affix| {
        if !will_discard_result && affix == Affix::Postfix {
            // Record the old value onto the stack
            VariableAccess::for_expr(builder, expr).get_value(builder);
        }

        // Keep the variable on the stack without retrieving its value
        let _ = VariableAccess::for_expr(builder, expr);

        let access = VariableAccess::for_expr(builder, expr);
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
                VariableAccess::for_constant(builder, &ConstantKind::Integer(-*value));
            }
            Expr {
                value: ExprKind::Constant(ConstantKind::Float(value)),
                ..
            } => {
                VariableAccess::for_constant(builder, &ConstantKind::Float(-*value));
            }
            _ => {
                VariableAccess::for_constant(builder, &ConstantKind::Integer(0));
                gen_expr(builder, expr, false);
                builder.action(Action::Subtract);
            }
        },
        UnaryOperator::BitNot => {
            gen_expr(builder, expr, false);
            builder.push(-1);
            builder.action(Action::BitXor);
        }
        UnaryOperator::Increment(affix) => adjust_in_place(builder, Action::Increment, affix),
        UnaryOperator::Decrement(affix) => adjust_in_place(builder, Action::Decrement, affix),
        UnaryOperator::LogicalNot => {
            gen_expr(builder, expr, false);
            builder.action(Action::Not);
        }
    }
}

fn gen_binary_op(
    builder: &mut CodeBuilder,
    op: BinaryOperator,
    left: &Expr,
    right: &Expr,
    will_discard_result: bool, // if we can optimise slightly by not deliberately pushing the result onto the stack
) {
    let trivial = |builder: &mut CodeBuilder, action: Action| {
        gen_expr(builder, left, false);
        gen_expr(builder, right, false);
        builder.action(action);
    };
    let trivial_assign = |builder: &mut CodeBuilder, action: Action| {
        let access = VariableAccess::for_expr(builder, left);
        trivial(builder, action);
        if will_discard_result {
            access.set_value(builder);
        } else {
            access.get_and_set_value(builder);
        }
    };
    match op {
        BinaryOperator::Add => trivial(builder, Action::Add2),
        BinaryOperator::Assign => {
            let access = VariableAccess::for_expr(builder, left);
            gen_expr(builder, right, false);
            if will_discard_result {
                access.set_value(builder);
            } else {
                access.get_and_set_value(builder);
            }
        }
        BinaryOperator::AddAssign => trivial_assign(builder, Action::Add2),
        BinaryOperator::Sub => trivial(builder, Action::Subtract),
        BinaryOperator::SubAssign => trivial_assign(builder, Action::Subtract),
        BinaryOperator::Divide => trivial(builder, Action::Divide),
        BinaryOperator::DivideAssign => trivial_assign(builder, Action::Divide),
        BinaryOperator::Multiply => trivial(builder, Action::Multiply),
        BinaryOperator::MultiplyAssign => trivial_assign(builder, Action::Multiply),
        BinaryOperator::Modulo => trivial(builder, Action::Modulo),
        BinaryOperator::ModuloAssign => trivial_assign(builder, Action::Modulo),
        BinaryOperator::BitAnd => trivial(builder, Action::BitAnd),
        BinaryOperator::BitAndAssign => trivial_assign(builder, Action::BitAnd),
        BinaryOperator::BitOr => trivial(builder, Action::BitOr),
        BinaryOperator::BitOrAssign => trivial_assign(builder, Action::BitOr),
        BinaryOperator::BitXor => trivial(builder, Action::BitXor),
        BinaryOperator::BitXorAssign => trivial_assign(builder, Action::BitXor),
        BinaryOperator::BitShiftLeft => trivial(builder, Action::BitLShift),
        BinaryOperator::BitShiftLeftAssign => trivial_assign(builder, Action::BitLShift),
        BinaryOperator::BitShiftRight => trivial(builder, Action::BitRShift),
        BinaryOperator::BitShiftRightAssign => trivial_assign(builder, Action::BitRShift),
        BinaryOperator::BitShiftRightUnsigned => trivial(builder, Action::BitURShift),
        BinaryOperator::BitShiftRightUnsignedAssign => trivial_assign(builder, Action::BitURShift),
        BinaryOperator::Equal => trivial(builder, Action::Equals2),
        BinaryOperator::StrictEqual => trivial(builder, Action::StrictEquals),
        BinaryOperator::NotEqual => {
            trivial(builder, Action::Equals2);
            builder.action(Action::Not)
        }
        BinaryOperator::StrictNotEqual => {
            trivial(builder, Action::StrictEquals);
            builder.action(Action::Not)
        }
        BinaryOperator::LessThan => trivial(builder, Action::Less2),
        BinaryOperator::LessThanEqual => {
            trivial(builder, Action::Greater);
            builder.action(Action::Not)
        }
        BinaryOperator::GreaterThan => trivial(builder, Action::Greater),
        BinaryOperator::GreaterThanEqual => {
            trivial(builder, Action::Less2);
            builder.action(Action::Not)
        }
        BinaryOperator::LogicalAnd => {
            let end_label = builder.create_label();
            gen_expr(builder, left, false);
            builder.action(Action::PushDuplicate);
            builder.action(Action::Not);
            builder.action(Action::If(end_label.clone()));
            builder.action(Action::Pop);
            gen_expr(builder, right, false);
            builder.mark_label(end_label);
        }
        BinaryOperator::LogicalOr => {
            let end_label = builder.create_label();
            gen_expr(builder, left, false);
            builder.action(Action::PushDuplicate);
            builder.action(Action::If(end_label.clone()));
            builder.action(Action::Pop);
            gen_expr(builder, right, false);
            builder.mark_label(end_label);
        }
        BinaryOperator::InstanceOf => trivial(builder, Action::InstanceOf),
    }
}

fn gen_call(builder: &mut CodeBuilder, span: Span, name: &Expr, args: &[Expr]) {
    if let Expr {
        value: ExprKind::Constant(ConstantKind::Identifier(identifier)),
        ..
    } = name
        && gen_special_call(builder, span, identifier, args)
    {
        return;
    }

    for arg in args.iter().rev() {
        gen_expr(builder, arg, false);
    }
    let num_args = args.len() as i32;
    builder.push(num_args);
    VariableAccess::for_expr(builder, name).call(builder, num_args);
}

fn gen_new(builder: &mut CodeBuilder, name: &Expr, args: &[Expr]) {
    for arg in args.iter().rev() {
        gen_expr(builder, arg, false);
    }
    let num_args = args.len() as i32;
    builder.push(num_args);
    let access = VariableAccess::for_expr(builder, name);
    access.call_new(builder, num_args);
}

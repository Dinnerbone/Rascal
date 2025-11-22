use crate::builder::CodeBuilder;
use crate::statement::gen_expr;
use ruasc_as2::ast::Expr;
use ruasc_as2_pcode::Action;
use ruasc_common::span::Span;

pub(crate) fn gen_special_call(
    builder: &mut CodeBuilder,
    span: Span,
    name: &str,
    args: &[Expr],
) -> bool {
    match name {
        "call" => fn_call(builder, span, args),
        "chr" => fn_chr(builder, span, args),
        "getTimer" => fn_get_timer(builder, span, args),
        "trace" => fn_trace(builder, span, args),
        "random" => fn_random(builder, span, args),
        _ => return false,
    };
    true
}

fn fn_call(builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() == 1 {
        gen_expr(builder, &args[0], false);
        builder.action(Action::Call);
    } else {
        builder.error("Wrong number of parameters; call requires exactly 1.", span);
    }
}

fn fn_chr(builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() == 1 {
        gen_expr(builder, &args[0], false);
        builder.action(Action::AsciiToChar);
    } else {
        builder.error("Wrong number of parameters; chr requires exactly 1.", span);
    }
}

fn fn_get_timer(builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.is_empty() {
        builder.action(Action::GetTime);
    } else {
        builder.error(
            "Wrong number of parameters; getTimer requires exactly 0.",
            span,
        );
    }
}

fn fn_trace(builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() == 1 {
        gen_expr(builder, &args[0], false);
        builder.action(Action::Trace);
    } else {
        builder.error(
            "Wrong number of parameters; trace requires exactly 1.",
            span,
        );
    }
}

fn fn_random(builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() == 1 {
        gen_expr(builder, &args[0], false);
        builder.action(Action::RandomNumber);
    } else {
        builder.error(
            "Wrong number of parameters; random requires exactly 1.",
            span,
        );
    }
}

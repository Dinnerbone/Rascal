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
        "trace" => fn_trace(builder, span, args),
        "random" => fn_random(builder, span, args),
        _ => return false,
    };
    true
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

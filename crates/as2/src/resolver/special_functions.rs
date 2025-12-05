use crate::resolver::{ModuleContext, resolve_expr};
use crate::{ast, hir};
use rascal_common::span::Span;

pub(crate) fn resolve_special_call(
    context: &mut ModuleContext,
    span: Span,
    name: &str,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    match name.to_ascii_lowercase().as_str() {
        "eval" => fn_eval(context, span, args),
        _ => None,
    }
}

fn fn_eval(context: &mut ModuleContext, span: Span, args: &[ast::Expr]) -> Option<hir::ExprKind> {
    if args.len() == 1 {
        let name = resolve_expr(context, args[0].clone());
        Some(hir::ExprKind::GetVariable(Box::new(name)))
    } else {
        context.error("Wrong number of parameters; eval requires exactly 1.", span);
        None
    }
}

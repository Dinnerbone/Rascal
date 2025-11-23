use crate::builder::CodeBuilder;
use crate::statement::gen_expr;
use ruasc_as2::ast::{ConstantKind, Expr, ExprKind};
use ruasc_as2_pcode::Action::GetUrl;
use ruasc_as2_pcode::{Action, PushValue};
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
        "getURL" => fn_get_url(builder, span, args),
        "getVersion" => fn_get_version(builder, span, args),
        "gotoAndPlay" => fn_goto_and_play(builder, span, args),
        "gotoAndStop" => fn_goto_and_stop(builder, span, args),
        "int" => fn_int(builder, span, args),
        "length" => fn_length(builder, span, args),
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

fn fn_get_version(builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.is_empty() {
        builder.action(Action::Push(vec![PushValue::String(
            "/:$version".to_string(),
        )]));
        builder.action(Action::GetVariable);
    } else {
        builder.error(
            "Wrong number of parameters; getVersion requires exactly 0.",
            span,
        );
    }
}

fn fn_goto_and_play(builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    let frame = if args.len() == 1 {
        // gotoAndPlay(frame)
        &args[0]
    } else if args.len() == 2 {
        // gotoAndPlay(scene, frame)
        // Scenes aren't a concept in a standalone compiler though, so the correct behaviour is to validate and then ignore the scene parameter
        // (That's what Flash does if it's a scene it doesn't recognise)
        if !matches!(args[0].value, ExprKind::Constant(ConstantKind::String(_))) {
            builder.error("Scene name must be quoted string.", args[0].span);
            return;
        }
        &args[1]
    } else {
        builder.error(
            "Wrong number of parameters; gotoAndPlay requires between 1 and 2.",
            span,
        );
        return;
    };

    match &frame.value {
        ExprKind::Constant(ConstantKind::String(label)) => {
            builder.action(Action::GotoLabel(label.to_string()));
            builder.action(Action::Play);
        }
        ExprKind::Constant(ConstantKind::Integer(frame_number)) => {
            let mut frame_number = *frame_number;
            if frame_number != 0 {
                frame_number = frame_number.wrapping_sub(1);
            }
            builder.action(Action::GotoFrame((frame_number & 0xFFFF) as u16));
            builder.action(Action::Play);
        }
        _ => {
            gen_expr(builder, frame, false);
            builder.action(Action::GotoFrame2 {
                scene_bias: 0,
                play: true,
            });
        }
    }
}

fn fn_goto_and_stop(builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    let frame = if args.len() == 1 {
        // gotoAndStop(frame)
        &args[0]
    } else if args.len() == 2 {
        // gotoAndStop(scene, frame)
        // Scenes aren't a concept in a standalone compiler though, so the correct behaviour is to validate and then ignore the scene parameter
        // (That's what Flash does if it's a scene it doesn't recognise)
        if !matches!(args[0].value, ExprKind::Constant(ConstantKind::String(_))) {
            builder.error("Scene name must be quoted string.", args[0].span);
            return;
        }
        &args[1]
    } else {
        builder.error(
            "Wrong number of parameters; gotoAndStop requires between 1 and 2.",
            span,
        );
        return;
    };

    match &frame.value {
        ExprKind::Constant(ConstantKind::String(label)) => {
            builder.action(Action::GotoLabel(label.to_string()));
        }
        ExprKind::Constant(ConstantKind::Integer(frame_number)) => {
            let mut frame_number = *frame_number;
            if frame_number != 0 {
                frame_number = frame_number.wrapping_sub(1);
            }
            builder.action(Action::GotoFrame((frame_number & 0xFFFF) as u16));
        }
        _ => {
            gen_expr(builder, frame, false);
            builder.action(Action::GotoFrame2 {
                scene_bias: 0,
                play: false,
            });
        }
    }
}

fn fn_get_url(builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.is_empty() || args.len() > 3 {
        builder.error(
            "Wrong number of parameters; getURL requires between 1 and 3.",
            span,
        );
        return;
    }
    let url = &args[0]; // The only guaranteed argument
    let target = args.get(1);
    let method = args.get(2);

    // First we see if getUrl is viable - it requires a string url and (maybe) a string target, but no method
    if method.is_none()
        && let Expr {
            value: ExprKind::Constant(ConstantKind::String(url)),
            ..
        } = url
    {
        if let Some(Expr {
            value: ExprKind::Constant(ConstantKind::String(target)),
            ..
        }) = target
        {
            builder.action(GetUrl {
                url: url.to_string(),
                target: target.to_string(),
            });
            return;
        } else if target.is_none() {
            builder.action(GetUrl {
                url: url.to_string(),
                target: "".to_string(),
            });
            return;
        }
    }

    // We can't use the "classic" getUrl, so let's use the stack
    gen_expr(builder, url, false);
    if let Some(target) = target {
        gen_expr(builder, target, false);
    } else {
        builder.action(Action::Push(vec![PushValue::String("".to_string())]));
    }
    let method = get_method(builder, method);
    builder.action(Action::GetUrl2 {
        load_variables: false,
        load_target: false,
        method,
    });
}

fn get_method(builder: &mut CodeBuilder, method: Option<&Expr>) -> u8 {
    let Some(method) = method else {
        return 0;
    };
    let span = method.span;
    let Expr {
        value: ExprKind::Constant(ConstantKind::String(method)),
        ..
    } = method
    else {
        builder.error("Method name must be GET or POST.", span);
        return 0;
    };
    match method.to_ascii_lowercase().as_str() {
        "get" => 1,
        "post" => 2,
        _ => {
            builder.error("Method name must be GET or POST.", span);
            0
        }
    }
}

fn fn_int(builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() == 1 {
        gen_expr(builder, &args[0], false);
        builder.action(Action::ToInteger);
    } else {
        builder.error("Wrong number of parameters; int requires exactly 1.", span);
    }
}

fn fn_length(builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() == 1 {
        gen_expr(builder, &args[0], false);
        builder.action(Action::StringLength);
    } else {
        builder.error(
            "Wrong number of parameters; length requires exactly 1.",
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

use crate::builder::CodeBuilder;
use crate::context::ScriptContext;
use crate::statement::gen_expr;
use rascal_as2::ast::{ConstantKind, Expr, ExprKind};
use rascal_as2_pcode::Action;
use rascal_as2_pcode::Action::GetUrl;
use rascal_common::span::Span;

pub(crate) fn gen_special_call(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    span: Span,
    name: &str,
    args: &[Expr],
) -> bool {
    match name.to_ascii_lowercase().as_str() {
        "call" => fn_call(context, builder, span, args),
        "chr" => fn_chr(context, builder, span, args),
        "duplicatemovieclip" => fn_duplicate_movie_clip(context, builder, span, args),
        "eval" => fn_eval(context, builder, span, args),
        "fscommand" => fn_fscommand(context, builder, span, args),
        "gettimer" => fn_get_timer(builder, span, args),
        "geturl" => fn_get_url(context, builder, span, args),
        "getversion" => fn_get_version(builder, span, args),
        "gotoandplay" => fn_goto_and_play(context, builder, span, args),
        "gotoandstop" => fn_goto_and_stop(context, builder, span, args),
        "int" => fn_int(context, builder, span, args),
        "length" => fn_length(context, builder, span, args),
        "loadmovie" => fn_load_movie(context, builder, span, args),
        "loadmovienum" => fn_load_movie_num(context, builder, span, args),
        "loadvariables" => fn_load_variables(context, builder, span, args),
        "loadvariablesnum" => fn_load_variables_num(context, builder, span, args),
        "mbchr" => fn_mbchr(context, builder, span, args),
        "mblength" => fn_mblength(context, builder, span, args),
        "mbord" => fn_mbord(context, builder, span, args),
        "mbsubstring" => fn_mbsubstring(context, builder, span, args),
        "nextframe" => fn_next_frame(builder, span, args),
        "nextscene" => fn_next_scene(builder, span, args),
        "number" => fn_number(context, builder, span, args),
        "ord" => fn_ord(context, builder, span, args),
        "play" => fn_play(builder, span, args),
        "prevframe" => fn_prev_frame(builder, span, args),
        "prevscene" => fn_prev_scene(builder, span, args),
        "print" => fn_print(context, builder, span, args),
        "printasbitmap" => fn_print_as_bitmap(context, builder, span, args),
        "printasbitmapnum" => fn_print_as_bitmap_num(context, builder, span, args),
        "printnum" => fn_print_num(context, builder, span, args),
        "removemovieclip" => fn_remove_movie_clio(context, builder, span, args),
        "startdrag" => fn_start_drag(context, builder, span, args),
        "stop" => fn_stop(builder, span, args),
        "stopallsounds" => fn_stop_all_sounds(builder, span, args),
        "stopdrag" => fn_stop_drag(builder, span, args),
        "string" => fn_string(context, builder, span, args),
        "substring" => fn_substring(context, builder, span, args),
        "targetpath" => fn_target_path(context, builder, span, args),
        "togglehighquality" => fn_toggle_high_quality(builder, span, args),
        "trace" => fn_trace(context, builder, span, args),
        "random" => fn_random(context, builder, span, args),
        "unloadmovie" => fn_unload_movie(context, builder, span, args),
        "unloadmovienum" => fn_unload_movie_num(context, builder, span, args),
        _ => return false,
    };
    true
}

fn fn_call(context: &mut ScriptContext, builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() == 1 {
        gen_expr(context, builder, &args[0], false);
        builder.action(Action::Call);
    } else {
        builder.error("Wrong number of parameters; call requires exactly 1.", span);
    }
}

fn fn_eval(context: &mut ScriptContext, builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() == 1 {
        gen_expr(context, builder, &args[0], false);
        builder.action(Action::GetVariable);
    } else {
        builder.error("Wrong number of parameters; eval requires exactly 1.", span);
    }
}

fn fn_chr(context: &mut ScriptContext, builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() == 1 {
        gen_expr(context, builder, &args[0], false);
        builder.action(Action::AsciiToChar);
    } else {
        builder.error("Wrong number of parameters; chr requires exactly 1.", span);
    }
}

fn fn_duplicate_movie_clip(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    span: Span,
    args: &[Expr],
) {
    if args.len() == 3 {
        gen_expr(context, builder, &args[0], false);
        gen_expr(context, builder, &args[1], false);
        builder.push(16384);
        gen_expr(context, builder, &args[2], false);
        builder.action(Action::Add2);
        builder.action(Action::CloneSprite);
    } else {
        builder.error(
            "Wrong number of parameters; duplicateMovieClip requires exactly 3.",
            span,
        );
    }
}

fn fn_mbchr(context: &mut ScriptContext, builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() == 1 {
        gen_expr(context, builder, &args[0], false);
        builder.action(Action::MBAsciiToChar);
    } else {
        builder.error(
            "Wrong number of parameters; mbchr requires exactly 1.",
            span,
        );
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
        builder.push("/:$version");
        builder.action(Action::GetVariable);
    } else {
        builder.error(
            "Wrong number of parameters; getVersion requires exactly 0.",
            span,
        );
    }
}

fn fn_goto_and_play(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    span: Span,
    args: &[Expr],
) {
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
            gen_expr(context, builder, frame, false);
            builder.action(Action::GotoFrame2 {
                scene_bias: 0,
                play: true,
            });
        }
    }
}

fn fn_goto_and_stop(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    span: Span,
    args: &[Expr],
) {
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
            gen_expr(context, builder, frame, false);
            builder.action(Action::GotoFrame2 {
                scene_bias: 0,
                play: false,
            });
        }
    }
}

fn fn_get_url(context: &mut ScriptContext, builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
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
    gen_expr(context, builder, url, false);
    if let Some(target) = target {
        gen_expr(context, builder, target, false);
    } else {
        builder.push("");
    }
    let method = get_method(builder, method);
    builder.action(Action::GetUrl2 {
        load_variables: false,
        load_target: false,
        method,
    });
}

fn fn_load_movie(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    span: Span,
    args: &[Expr],
) {
    if args.len() < 2 || args.len() > 3 {
        builder.error(
            "Wrong number of parameters; loadMovie requires between 2 and 3.",
            span,
        );
        return;
    }

    gen_expr(context, builder, &args[0], false);
    gen_expr(context, builder, &args[1], false);
    let method = get_method(builder, args.get(2));
    builder.action(Action::GetUrl2 {
        load_variables: false,
        load_target: true,
        method,
    });
}

fn fn_load_movie_num(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    span: Span,
    args: &[Expr],
) {
    if args.len() < 2 || args.len() > 3 {
        builder.error(
            "Wrong number of parameters; loadMovieNum requires between 2 and 3.",
            span,
        );
        return;
    }
    let url = &args[0];
    let target = &args[1];
    let method = args.get(2);

    // First we see if getUrl is viable - it requires a string url and a constant target, but no method
    if method.is_none()
        && let Expr {
            value: ExprKind::Constant(ConstantKind::String(url)),
            ..
        } = url
    {
        match &target.value {
            ExprKind::Constant(ConstantKind::Integer(target)) => {
                builder.action(GetUrl {
                    url: url.to_string(),
                    target: format!("_level{}", target),
                });
                return;
            }
            ExprKind::Constant(ConstantKind::String(target)) => {
                builder.action(GetUrl {
                    url: url.to_string(),
                    target: format!("_level{}", target),
                });
                return;
            }
            _ => {}
        }
    }

    // We can't use the "classic" getUrl, so let's use the stack
    gen_expr(context, builder, url, false);

    match &target.value {
        ExprKind::Constant(ConstantKind::Integer(target)) => {
            let str = format!("_level{}", target);
            let value = context.constants.add(&str);
            builder.push(value);
        }
        ExprKind::Constant(ConstantKind::String(target)) => {
            let str = format!("_level{}", target);
            let value = context.constants.add(&str);
            builder.push(value);
        }
        _ => {
            let value = context.constants.add("_level");
            builder.push(value);
            gen_expr(context, builder, &args[1], false);
            builder.action(Action::StringAdd);
        }
    }

    let method = get_method(builder, method);
    builder.action(Action::GetUrl2 {
        load_variables: false,
        load_target: false,
        method,
    });
}

fn fn_load_variables(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    span: Span,
    args: &[Expr],
) {
    if args.len() < 2 || args.len() > 3 {
        builder.error(
            "Wrong number of parameters; loadVariables requires between 2 and 3.",
            span,
        );
        return;
    }

    gen_expr(context, builder, &args[0], false);
    gen_expr(context, builder, &args[1], false);
    let method = get_method(builder, args.get(2));
    builder.action(Action::GetUrl2 {
        load_variables: true,
        load_target: true,
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

fn get_bounds_type(builder: &mut CodeBuilder, name: Option<&Expr>) -> &'static str {
    let Some(name) = name else {
        return "";
    };
    let span = name.span;
    let Expr {
        value: ExprKind::Constant(ConstantKind::String(name)),
        ..
    } = name
    else {
        builder.error("Bounds type must be bmovie, bframe or bmax.", span);
        return "";
    };
    match name.to_ascii_lowercase().as_str() {
        "bmax" => "#bmax",
        "bframe" => "#bframe",
        "bmovie" => "",
        _ => {
            builder.error("Bounds type must be bmovie, bframe or bmax.", span);
            ""
        }
    }
}

fn fn_load_variables_num(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    span: Span,
    args: &[Expr],
) {
    if args.len() < 2 || args.len() > 3 {
        builder.error(
            "Wrong number of parameters; loadVariablesNum requires between 2 and 3.",
            span,
        );
        return;
    }
    let url = &args[0];
    let target = &args[1];
    let method = args.get(2);
    gen_expr(context, builder, url, false);

    match &target.value {
        ExprKind::Constant(ConstantKind::Integer(target)) => {
            let str = format!("_level{}", target);
            let value = context.constants.add(&str);
            builder.push(value);
        }
        ExprKind::Constant(ConstantKind::String(target)) => {
            let str = format!("_level{}", target);
            let value = context.constants.add(&str);
            builder.push(value);
        }
        _ => {
            let value = context.constants.add("_level");
            builder.push(value);
            gen_expr(context, builder, &args[1], false);
            builder.action(Action::StringAdd);
        }
    }

    let method = get_method(builder, method);
    builder.action(Action::GetUrl2 {
        load_variables: true,
        load_target: false,
        method,
    });
}

fn fn_int(context: &mut ScriptContext, builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() == 1 {
        gen_expr(context, builder, &args[0], false);
        builder.action(Action::ToInteger);
    } else {
        builder.error("Wrong number of parameters; int requires exactly 1.", span);
    }
}

fn fn_length(context: &mut ScriptContext, builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() == 1 {
        gen_expr(context, builder, &args[0], false);
        builder.action(Action::StringLength);
    } else {
        builder.error(
            "Wrong number of parameters; length requires exactly 1.",
            span,
        );
    }
}

fn fn_mblength(context: &mut ScriptContext, builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() == 1 {
        gen_expr(context, builder, &args[0], false);
        builder.action(Action::MBStringLength);
    } else {
        builder.error(
            "Wrong number of parameters; mblength requires exactly 1.",
            span,
        );
    }
}

fn fn_mbord(context: &mut ScriptContext, builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() == 1 {
        gen_expr(context, builder, &args[0], false);
        builder.action(Action::MBCharToAscii);
    } else {
        builder.error(
            "Wrong number of parameters; mbord requires exactly 1.",
            span,
        );
    }
}

fn fn_mbsubstring(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    span: Span,
    args: &[Expr],
) {
    if args.len() < 2 || args.len() > 3 {
        builder.error(
            "Wrong number of parameters; mbsubstring requires between 2 and 3.",
            span,
        );
        return;
    }
    gen_expr(context, builder, &args[0], false);
    gen_expr(context, builder, &args[1], false);
    if let Some(length) = args.get(2) {
        gen_expr(context, builder, length, false);
    } else {
        builder.push(-1);
    }
    builder.action(Action::MBStringExtract);
}

fn fn_substring(context: &mut ScriptContext, builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() < 2 || args.len() > 3 {
        builder.error(
            "Wrong number of parameters; substring requires between 2 and 3.",
            span,
        );
        return;
    }
    gen_expr(context, builder, &args[0], false);
    gen_expr(context, builder, &args[1], false);
    if let Some(length) = args.get(2) {
        gen_expr(context, builder, length, false);
    } else {
        builder.push(-1);
    }
    builder.action(Action::StringExtract);
}

fn fn_next_frame(builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.is_empty() {
        builder.action(Action::NextFrame);
    } else {
        builder.error(
            "Wrong number of parameters; nextFrame requires exactly 0.",
            span,
        );
    }
}

fn fn_prev_frame(builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.is_empty() {
        builder.action(Action::PrevFrame);
    } else {
        builder.error(
            "Wrong number of parameters; prevFrame requires exactly 0.",
            span,
        );
    }
}

fn fn_prev_scene(builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.is_empty() {
        // Scenes don't exist in a standalone compiler, and Flash's behaviour is to skip to frame 0 in this case
        builder.action(Action::GotoFrame(0));
    } else {
        builder.error(
            "Wrong number of parameters; prevScene requires exactly 0.",
            span,
        );
    }
}

fn fn_next_scene(builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.is_empty() {
        // Scenes don't exist in a standalone compiler, and Flash's behaviour is to skip to frame 0 in this case
        builder.action(Action::GotoFrame(0));
    } else {
        builder.error(
            "Wrong number of parameters; nextScene requires exactly 0.",
            span,
        );
    }
}

fn fn_number(context: &mut ScriptContext, builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() == 1 {
        gen_expr(context, builder, &args[0], false);
        builder.action(Action::ToNumber);
    } else {
        builder.error(
            "Wrong number of parameters; number requires exactly 1.",
            span,
        );
    }
}

fn fn_ord(context: &mut ScriptContext, builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() == 1 {
        gen_expr(context, builder, &args[0], false);
        builder.action(Action::CharToAscii);
    } else {
        builder.error("Wrong number of parameters; ord requires exactly 1.", span);
    }
}

fn fn_play(builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.is_empty() {
        builder.action(Action::Play);
    } else {
        builder.error("Wrong number of parameters; play requires exactly 0.", span);
    }
}

fn fn_fscommand(context: &mut ScriptContext, builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.is_empty() || args.len() > 2 {
        builder.error(
            "Wrong number of parameters; FSCommand requires between 1 and 2.",
            span,
        );
        return;
    }

    if let ExprKind::Constant(ConstantKind::String(command)) = &args[0].value {
        let url = format!("FSCommand:{}", command);
        if args.len() == 1 {
            builder.action(Action::GetUrl {
                url,
                target: "".to_string(),
            });
            return;
        } else if let ExprKind::Constant(ConstantKind::String(arg)) = &args[1].value {
            builder.action(Action::GetUrl {
                url,
                target: arg.to_string(),
            });
            return;
        }
    }

    match &args[0].value {
        ExprKind::Constant(ConstantKind::String(target)) => {
            let str = format!("FSCommand:{}", target);
            let value = context.constants.add(&str);
            builder.push(value);
        }
        _ => {
            let value = context.constants.add("FSCommand:");
            builder.push(value);
            gen_expr(context, builder, &args[0], false);
            builder.action(Action::StringAdd);
        }
    }
    if args.len() == 2 {
        gen_expr(context, builder, &args[1], false);
    } else {
        builder.push("");
    }
    builder.action(Action::GetUrl2 {
        load_target: false,
        load_variables: false,
        method: 0,
    });
}

fn fn_print(context: &mut ScriptContext, builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() == 2 {
        let url = format!("print:{}", get_bounds_type(builder, args.get(1)));
        let value = context.constants.add(&url);
        builder.push(value);
        gen_expr(context, builder, &args[0], false);
        builder.action(Action::GetUrl2 {
            load_target: false,
            load_variables: false,
            method: 0,
        });
    } else {
        builder.error(
            "Wrong number of parameters; print requires exactly 2.",
            span,
        );
    }
}

fn fn_print_as_bitmap(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    span: Span,
    args: &[Expr],
) {
    if args.len() == 2 {
        let url = format!("printasbitmap:{}", get_bounds_type(builder, args.get(1)));
        let value = context.constants.add(&url);
        builder.push(value);
        gen_expr(context, builder, &args[0], false);
        builder.action(Action::GetUrl2 {
            load_target: false,
            load_variables: false,
            method: 0,
        });
    } else {
        builder.error(
            "Wrong number of parameters; printAsBitmap requires exactly 2.",
            span,
        );
    }
}

fn fn_print_as_bitmap_num(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    span: Span,
    args: &[Expr],
) {
    if args.len() == 2 {
        let url = format!("printasbitmap:{}", get_bounds_type(builder, args.get(1)));
        let value = context.constants.add(&url);
        builder.push(value);

        match &args[0].value {
            ExprKind::Constant(ConstantKind::Integer(target)) => {
                let str = format!("_level{}", target);
                let value = context.constants.add(&str);
                builder.push(value);
            }
            ExprKind::Constant(ConstantKind::String(target)) => {
                let str = format!("_level{}", target);
                let value = context.constants.add(&str);
                builder.push(value);
            }
            _ => {
                let value = context.constants.add("_level");
                builder.push(value);
                gen_expr(context, builder, &args[0], false);
                builder.action(Action::StringAdd);
            }
        }

        builder.action(Action::GetUrl2 {
            load_target: false,
            load_variables: false,
            method: 0,
        });
    } else {
        builder.error(
            "Wrong number of parameters; printAsBitmapNum requires exactly 2.",
            span,
        );
    }
}

fn fn_print_num(context: &mut ScriptContext, builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() == 2 {
        let url = format!("print:{}", get_bounds_type(builder, args.get(1)));
        let value = context.constants.add(&url);
        builder.push(value);

        match &args[0].value {
            ExprKind::Constant(ConstantKind::Integer(target)) => {
                let str = format!("_level{}", target);
                let value = context.constants.add(&str);
                builder.push(value);
            }
            ExprKind::Constant(ConstantKind::String(target)) => {
                let str = format!("_level{}", target);
                let value = context.constants.add(&str);
                builder.push(value);
            }
            _ => {
                let value = context.constants.add("_level");
                builder.push(value);
                gen_expr(context, builder, &args[0], false);
                builder.action(Action::StringAdd);
            }
        }

        builder.action(Action::GetUrl2 {
            load_target: false,
            load_variables: false,
            method: 0,
        });
    } else {
        builder.error(
            "Wrong number of parameters; printNum requires exactly 2.",
            span,
        );
    }
}

fn fn_start_drag(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    span: Span,
    args: &[Expr],
) {
    if args.is_empty() || args.len() > 6 {
        builder.error(
            "Wrong number of parameters; startDrag requires between 1 and 6.",
            span,
        );
        return;
    }
    let target = &args[0]; // Only guaranteed argument
    let lock = match args.get(1) {
        Some(Expr {
            value: ExprKind::Constant(ConstantKind::Identifier("true")),
            ..
        }) => 1,
        Some(Expr {
            value: ExprKind::Constant(ConstantKind::Identifier("false")),
            ..
        }) => 0,
        None => 0,
        Some(other) => {
            builder.error("Lock center parameter must be true or false", other.span);
            return;
        }
    };

    if args.len() == 6 {
        #[expect(clippy::needless_range_loop)]
        for i in 2..6 {
            gen_expr(context, builder, &args[i], false);
        }
        builder.push(1);
        builder.push(lock);
        gen_expr(context, builder, target, false);
        builder.action_with_stack_delta(Action::StartDrag, -7);
    } else if args.len() < 3 {
        builder.push(0);
        builder.push(lock);
        gen_expr(context, builder, target, false);
        builder.action_with_stack_delta(Action::StartDrag, -3);
    } else {
        builder.error(
            "startDrag requires 1 (target), 2 (target+lock) or 6 (target+lock+constraint) parameters.",
            span,
        );
    }
}

fn fn_stop(builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.is_empty() {
        builder.action(Action::Stop);
    } else {
        builder.error("Wrong number of parameters; stop requires exactly 0.", span);
    }
}

fn fn_stop_all_sounds(builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.is_empty() {
        builder.action(Action::StopSounds);
    } else {
        builder.error(
            "Wrong number of parameters; stopAllSounds requires exactly 0.",
            span,
        );
    }
}

fn fn_stop_drag(builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.is_empty() {
        builder.action(Action::EndDrag);
    } else {
        builder.error(
            "Wrong number of parameters; stopDrag requires exactly 0.",
            span,
        );
    }
}

fn fn_string(context: &mut ScriptContext, builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() == 1 {
        gen_expr(context, builder, &args[0], false);
        builder.action(Action::ToString);
    } else {
        builder.error(
            "Wrong number of parameters; String requires exactly 1.",
            span,
        );
    }
}

fn fn_target_path(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    span: Span,
    args: &[Expr],
) {
    if args.len() == 1 {
        gen_expr(context, builder, &args[0], false);
        builder.action(Action::TargetPath);
    } else {
        builder.error(
            "Wrong number of parameters; targetPath requires exactly 1.",
            span,
        );
    }
}

fn fn_toggle_high_quality(builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.is_empty() {
        builder.action(Action::ToggleQuality);
    } else {
        builder.error(
            "Wrong number of parameters; toggleHighQuality requires exactly 0.",
            span,
        );
    }
}

fn fn_trace(context: &mut ScriptContext, builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() == 1 {
        gen_expr(context, builder, &args[0], false);
        builder.action(Action::Trace);
    } else {
        builder.error(
            "Wrong number of parameters; trace requires exactly 1.",
            span,
        );
    }
}

fn fn_remove_movie_clio(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    span: Span,
    args: &[Expr],
) {
    if args.len() == 1 {
        gen_expr(context, builder, &args[0], false);
        builder.action(Action::RemoveSprite);
    } else {
        builder.error(
            "Wrong number of parameters; removeMovieClip requires exactly 1.",
            span,
        );
    }
}

fn fn_random(context: &mut ScriptContext, builder: &mut CodeBuilder, span: Span, args: &[Expr]) {
    if args.len() == 1 {
        gen_expr(context, builder, &args[0], false);
        builder.action(Action::RandomNumber);
    } else {
        builder.error(
            "Wrong number of parameters; random requires exactly 1.",
            span,
        );
    }
}

fn fn_unload_movie(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    span: Span,
    args: &[Expr],
) {
    if args.len() == 1 {
        let url = context.constants.add("");
        builder.push(url);
        gen_expr(context, builder, &args[0], false);
        builder.action(Action::GetUrl2 {
            load_target: true,
            load_variables: false,
            method: 0,
        });
    } else {
        builder.error(
            "Wrong number of parameters; unloadMovie requires exactly 1.",
            span,
        );
    }
}

fn fn_unload_movie_num(
    context: &mut ScriptContext,
    builder: &mut CodeBuilder,
    span: Span,
    args: &[Expr],
) {
    if args.len() != 1 {
        builder.error(
            "Wrong number of parameters; unloadMovieNum requires exactly 1.",
            span,
        );
        return;
    }

    // First we see if getUrl is viable - it requires a constant target
    match &args[0].value {
        ExprKind::Constant(ConstantKind::Integer(target)) => {
            builder.action(GetUrl {
                url: "".to_string(),
                target: format!("_level{}", target),
            });
        }
        ExprKind::Constant(ConstantKind::String(target)) => {
            builder.action(GetUrl {
                url: "".to_string(),
                target: format!("_level{}", target),
            });
        }
        _ => {
            let url = context.constants.add("");
            builder.push(url);
            let value = context.constants.add("_level");
            builder.push(value);
            gen_expr(context, builder, &args[0], false);
            builder.action(Action::StringAdd);
            builder.action(Action::GetUrl2 {
                load_target: false,
                load_variables: false,
                method: 0,
            });
        }
    }
}

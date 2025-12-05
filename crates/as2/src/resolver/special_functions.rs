use crate::resolver::special_properties::get_special_property;
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
        "call" => fn_call_frame(context, span, args),
        "chr" => fn_chr(context, span, args),
        "duplicatemovieclip" => fn_duplicate_movie_clip(context, span, args),
        "eval" => fn_eval(context, span, args),
        "fscommand" => fn_fscommand(context, span, args),
        "getproperty" => fn_get_property(context, span, args),
        "gettimer" => fn_get_timer(context, span, args),
        "geturl" => fn_get_url(context, span, args),
        "getversion" => fn_get_version(context, span, args),
        "gotoandplay" => fn_goto_and_play(context, span, args),
        "gotoandstop" => fn_goto_and_stop(context, span, args),
        "int" => fn_int(context, span, args),
        "length" => fn_length(context, span, args),
        "loadmovie" => fn_load_movie(context, span, args),
        "loadmovienum" => fn_load_movie_num(context, span, args),
        "loadvariables" => fn_load_variables(context, span, args),
        "loadvariablesnum" => fn_load_variables_num(context, span, args),
        "mbchr" => fn_mbchr(context, span, args),
        "mblength" => fn_mblength(context, span, args),
        "mbord" => fn_mbord(context, span, args),
        "mbsubstring" => fn_mbsubstring(context, span, args),
        "nextframe" => fn_next_frame(context, span, args),
        "nextscene" => fn_next_scene(context, span, args),
        "number" => fn_number(context, span, args),
        "ord" => fn_ord(context, span, args),
        "play" => fn_play(context, span, args),
        "prevframe" => fn_prev_frame(context, span, args),
        "prevscene" => fn_prev_scene(context, span, args),
        "print" => fn_print(context, span, args),
        "printasbitmap" => fn_print_as_bitmap(context, span, args),
        "printasbitmapnum" => fn_print_as_bitmap_num(context, span, args),
        "printnum" => fn_print_num(context, span, args),
        "random" => fn_random(context, span, args),
        "removemovieclip" => fn_remove_movie_clio(context, span, args),
        "setproperty" => fn_set_property(context, span, args),
        "startdrag" => fn_start_drag(context, span, args),
        "stop" => fn_stop(context, span, args),
        "stopallsounds" => fn_stop_all_sounds(context, span, args),
        "stopdrag" => fn_stop_drag(context, span, args),
        "string" => fn_string(context, span, args),
        "substring" => fn_substring(context, span, args),
        "targetpath" => fn_target_path(context, span, args),
        "togglehighquality" => fn_toggle_high_quality(context, span, args),
        "trace" => fn_trace(context, span, args),
        "unloadmovie" => fn_unload_movie(context, span, args),
        "unloadmovienum" => fn_unload_movie_num(context, span, args),
        _ => None,
    }
}

fn fn_chr(context: &mut ModuleContext, span: Span, args: &[ast::Expr]) -> Option<hir::ExprKind> {
    if args.len() == 1 {
        let value = resolve_expr(context, &args[0]);
        Some(hir::ExprKind::AsciiToChar(Box::new(value)))
    } else {
        context.error("Wrong number of parameters; chr requires exactly 1.", span);
        None
    }
}

fn fn_call_frame(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.len() == 1 {
        let frame = resolve_expr(context, &args[0]);
        Some(hir::ExprKind::CallFrame(Box::new(frame)))
    } else {
        context.error("Wrong number of parameters; call requires exactly 1.", span);
        None
    }
}

fn fn_eval(context: &mut ModuleContext, span: Span, args: &[ast::Expr]) -> Option<hir::ExprKind> {
    if args.len() == 1 {
        let name = resolve_expr(context, &args[0]);
        Some(hir::ExprKind::GetVariable(Box::new(name)))
    } else {
        context.error("Wrong number of parameters; eval requires exactly 1.", span);
        None
    }
}

fn fn_duplicate_movie_clip(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.len() == 3 {
        let source = resolve_expr(context, &args[0]);
        let target = resolve_expr(context, &args[1]);
        let depth = resolve_expr(context, &args[2]);
        Some(hir::ExprKind::DuplicateMovieClip {
            source: Box::new(source),
            target: Box::new(target),
            depth: Box::new(depth),
        })
    } else {
        context.error(
            "Wrong number of parameters; duplicateMovieClip requires exactly 3.",
            span,
        );
        None
    }
}

fn fn_mbchr(context: &mut ModuleContext, span: Span, args: &[ast::Expr]) -> Option<hir::ExprKind> {
    if args.len() == 1 {
        let name = resolve_expr(context, &args[0]);
        Some(hir::ExprKind::MBAsciiToChar(Box::new(name)))
    } else {
        context.error(
            "Wrong number of parameters; mbchr requires exactly 1.",
            span,
        );
        None
    }
}

fn fn_get_timer(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.is_empty() {
        Some(hir::ExprKind::GetTime)
    } else {
        context.error(
            "Wrong number of parameters; getTimer requires exactly 0.",
            span,
        );
        None
    }
}

fn fn_get_version(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.is_empty() {
        Some(hir::ExprKind::GetVariable(Box::new(hir::Expr::new(
            span,
            hir::ExprKind::Constant(hir::ConstantKind::String("/:$version".to_string())),
        ))))
    } else {
        context.error(
            "Wrong number of parameters; getVersion requires exactly 0.",
            span,
        );
        None
    }
}

fn fn_goto_and_play(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    let frame = if args.len() == 1 {
        // gotoAndPlay(frame)
        &args[0]
    } else if args.len() == 2 {
        // gotoAndPlay(scene, frame)
        // Scenes aren't a concept in a standalone compiler though, so the correct behaviour is to validate and then ignore the scene parameter
        // (That's what Flash does if it's a scene it doesn't recognise)
        if !matches!(
            args[0].value,
            ast::ExprKind::Constant(ast::ConstantKind::String(_))
        ) {
            context.error("Scene name must be quoted string.", args[0].span);
            return None;
        }
        &args[1]
    } else {
        context.error(
            "Wrong number of parameters; gotoAndPlay requires between 1 and 2.",
            span,
        );
        return None;
    };

    Some(hir::ExprKind::GotoFrame(
        Box::new(resolve_expr(context, frame)),
        true,
    ))
}

fn fn_goto_and_stop(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    let frame = if args.len() == 1 {
        // gotoAndStop(frame)
        &args[0]
    } else if args.len() == 2 {
        // gotoAndStop(scene, frame)
        // Scenes aren't a concept in a standalone compiler though, so the correct behaviour is to validate and then ignore the scene parameter
        // (That's what Flash does if it's a scene it doesn't recognise)
        if !matches!(
            args[0].value,
            ast::ExprKind::Constant(ast::ConstantKind::String(_))
        ) {
            context.error("Scene name must be quoted string.", args[0].span);
            return None;
        }
        &args[1]
    } else {
        context.error(
            "Wrong number of parameters; gotoAndStop requires between 1 and 2.",
            span,
        );
        return None;
    };

    Some(hir::ExprKind::GotoFrame(
        Box::new(resolve_expr(context, frame)),
        false,
    ))
}

fn fn_get_url(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    let (url, target, method) = if args.len() == 1 {
        let url = resolve_expr(context, &args[0]);
        (
            url,
            hir::Expr::new(
                span,
                hir::ExprKind::Constant(hir::ConstantKind::String("".to_string())),
            ),
            hir::GetUrlMethod::None,
        )
    } else if args.len() == 2 {
        let url = resolve_expr(context, &args[0]);
        let target = resolve_expr(context, &args[1]);
        (url, target, hir::GetUrlMethod::None)
    } else if args.len() == 3 {
        let url = resolve_expr(context, &args[0]);
        let target = resolve_expr(context, &args[1]);
        let method = get_method(context, &args[2])?;
        (url, target, method)
    } else {
        context.error(
            "Wrong number of parameters; getURL requires between 1 and 3.",
            span,
        );
        return None;
    };
    Some(hir::ExprKind::GetUrl {
        target: Box::new(target),
        url: Box::new(url),
        load_variables: false,
        load_target: false,
        method,
    })
}

fn fn_load_movie(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    let (url, target, method) = if args.len() == 2 {
        let url = resolve_expr(context, &args[0]);
        let target = resolve_expr(context, &args[1]);
        (url, target, hir::GetUrlMethod::None)
    } else if args.len() == 3 {
        let url = resolve_expr(context, &args[0]);
        let target = resolve_expr(context, &args[1]);
        let method = get_method(context, &args[2])?;
        (url, target, method)
    } else {
        context.error(
            "Wrong number of parameters; loadMovie requires between 2 and 3.",
            span,
        );
        return None;
    };
    Some(hir::ExprKind::GetUrl {
        target: Box::new(target),
        url: Box::new(url),
        load_variables: false,
        load_target: true,
        method,
    })
}

fn fn_load_movie_num(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    let (url, target, method) = if args.len() == 2 {
        let url = resolve_expr(context, &args[0]);
        let target = resolve_expr(context, &args[1]);
        (url, target, hir::GetUrlMethod::None)
    } else if args.len() == 3 {
        let url = resolve_expr(context, &args[0]);
        let target = resolve_expr(context, &args[1]);
        let method = get_method(context, &args[2])?;
        (url, target, method)
    } else {
        context.error(
            "Wrong number of parameters; loadMovieNum requires between 2 and 3.",
            span,
        );
        return None;
    };
    Some(hir::ExprKind::GetUrl {
        target: Box::new(hir::Expr::new(
            span,
            hir::ExprKind::BinaryOperator(
                hir::BinaryOperator::StringAdd,
                Box::new(hir::Expr::new(
                    span,
                    hir::ExprKind::Constant(hir::ConstantKind::String("_level".to_string())),
                )),
                Box::new(target),
            ),
        )),
        url: Box::new(url),
        load_variables: false,
        load_target: false,
        method,
    })
}

fn fn_load_variables(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    let (url, target, method) = if args.len() == 2 {
        let url = resolve_expr(context, &args[0]);
        let target = resolve_expr(context, &args[1]);
        (url, target, hir::GetUrlMethod::None)
    } else if args.len() == 3 {
        let url = resolve_expr(context, &args[0]);
        let target = resolve_expr(context, &args[1]);
        let method = get_method(context, &args[2])?;
        (url, target, method)
    } else {
        context.error(
            "Wrong number of parameters; loadVariables requires between 2 and 3.",
            span,
        );
        return None;
    };
    Some(hir::ExprKind::GetUrl {
        target: Box::new(target),
        url: Box::new(url),
        load_variables: true,
        load_target: true,
        method,
    })
}

fn get_method(context: &mut ModuleContext, method: &ast::Expr) -> Option<hir::GetUrlMethod> {
    let span = method.span;
    let ast::Expr {
        value: ast::ExprKind::Constant(ast::ConstantKind::String(method)),
        ..
    } = method
    else {
        context.error("Method name must be GET or POST.", span);
        return None;
    };
    match method.to_ascii_lowercase().as_str() {
        "get" => Some(hir::GetUrlMethod::Get),
        "post" => Some(hir::GetUrlMethod::Post),
        _ => {
            context.error("Method name must be GET or POST.", span);
            None
        }
    }
}

fn get_bounds_type(context: &mut ModuleContext, name: &ast::Expr) -> Option<&'static str> {
    let span = name.span;
    let ast::Expr {
        value: ast::ExprKind::Constant(ast::ConstantKind::String(name)),
        ..
    } = name
    else {
        context.error("Bounds type must be bmovie, bframe or bmax.", span);
        return None;
    };
    match name.to_ascii_lowercase().as_str() {
        "bmax" => Some("#bmax"),
        "bframe" => Some("#bframe"),
        "bmovie" => Some(""),
        _ => {
            context.error("Bounds type must be bmovie, bframe or bmax.", span);
            None
        }
    }
}

fn fn_load_variables_num(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    let (url, target, method) = if args.len() == 2 {
        let url = resolve_expr(context, &args[0]);
        let target = resolve_expr(context, &args[1]);
        (url, target, hir::GetUrlMethod::None)
    } else if args.len() == 3 {
        let url = resolve_expr(context, &args[0]);
        let target = resolve_expr(context, &args[1]);
        let method = get_method(context, &args[2])?;
        (url, target, method)
    } else {
        context.error(
            "Wrong number of parameters; loadVariablesNum requires between 2 and 3.",
            span,
        );
        return None;
    };
    Some(hir::ExprKind::GetUrl {
        target: Box::new(hir::Expr::new(
            span,
            hir::ExprKind::BinaryOperator(
                hir::BinaryOperator::StringAdd,
                Box::new(hir::Expr::new(
                    span,
                    hir::ExprKind::Constant(hir::ConstantKind::String("_level".to_string())),
                )),
                Box::new(target),
            ),
        )),
        url: Box::new(url),
        load_variables: true,
        load_target: false,
        method,
    })
}

fn fn_int(context: &mut ModuleContext, span: Span, args: &[ast::Expr]) -> Option<hir::ExprKind> {
    if args.len() == 1 {
        let value = resolve_expr(context, &args[0]);
        Some(hir::ExprKind::CastToInteger(Box::new(value)))
    } else {
        context.error("Wrong number of parameters; int requires exactly 1.", span);
        None
    }
}

fn fn_length(context: &mut ModuleContext, span: Span, args: &[ast::Expr]) -> Option<hir::ExprKind> {
    if args.len() == 1 {
        let value = resolve_expr(context, &args[0]);
        Some(hir::ExprKind::StringLength(Box::new(value)))
    } else {
        context.error(
            "Wrong number of parameters; length requires exactly 1.",
            span,
        );
        None
    }
}

fn fn_mblength(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.len() == 1 {
        let value = resolve_expr(context, &args[0]);
        Some(hir::ExprKind::MBStringLength(Box::new(value)))
    } else {
        context.error(
            "Wrong number of parameters; mblength requires exactly 1.",
            span,
        );
        None
    }
}

fn fn_mbord(context: &mut ModuleContext, span: Span, args: &[ast::Expr]) -> Option<hir::ExprKind> {
    if args.len() == 1 {
        let value = resolve_expr(context, &args[0]);
        Some(hir::ExprKind::MBCharToAscii(Box::new(value)))
    } else {
        context.error(
            "Wrong number of parameters; mbord requires exactly 1.",
            span,
        );
        None
    }
}

fn fn_mbsubstring(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.len() < 2 || args.len() > 3 {
        context.error(
            "Wrong number of parameters; mbsubstring requires between 2 and 3.",
            span,
        );
        return None;
    }
    let string = resolve_expr(context, &args[0]);
    let start = resolve_expr(context, &args[0]);
    let length = if let Some(length) = args.get(2) {
        resolve_expr(context, length)
    } else {
        hir::Expr::new(
            span,
            hir::ExprKind::Constant(hir::ConstantKind::Integer(-1)),
        )
    };
    Some(hir::ExprKind::MBSubstring {
        string: Box::new(string),
        start: Box::new(start),
        length: Box::new(length),
    })
}

fn fn_substring(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.len() < 2 || args.len() > 3 {
        context.error(
            "Wrong number of parameters; substring requires between 2 and 3.",
            span,
        );
        return None;
    }
    let string = resolve_expr(context, &args[0]);
    let start = resolve_expr(context, &args[0]);
    let length = if let Some(length) = args.get(2) {
        resolve_expr(context, length)
    } else {
        hir::Expr::new(
            span,
            hir::ExprKind::Constant(hir::ConstantKind::Integer(-1)),
        )
    };
    Some(hir::ExprKind::Substring {
        string: Box::new(string),
        start: Box::new(start),
        length: Box::new(length),
    })
}

fn fn_next_frame(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.is_empty() {
        Some(hir::ExprKind::NextFrame)
    } else {
        context.error(
            "Wrong number of parameters; nextFrame requires exactly 0.",
            span,
        );
        None
    }
}

fn fn_prev_frame(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.is_empty() {
        Some(hir::ExprKind::PreviousFrame)
    } else {
        context.error(
            "Wrong number of parameters; prevFrame requires exactly 0.",
            span,
        );
        None
    }
}

fn fn_prev_scene(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.is_empty() {
        // Scenes don't exist in a standalone compiler, and Flash's behaviour is to skip to frame 0 in this case
        Some(hir::ExprKind::GotoFrame(
            Box::new(hir::Expr::new(
                span,
                hir::ExprKind::Constant(hir::ConstantKind::Integer(0)),
            )),
            false,
        ))
    } else {
        context.error(
            "Wrong number of parameters; prevScene requires exactly 0.",
            span,
        );
        None
    }
}

fn fn_next_scene(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.is_empty() {
        // Scenes don't exist in a standalone compiler, and Flash's behaviour is to skip to frame 0 in this case
        Some(hir::ExprKind::GotoFrame(
            Box::new(hir::Expr::new(
                span,
                hir::ExprKind::Constant(hir::ConstantKind::Integer(0)),
            )),
            false,
        ))
    } else {
        context.error(
            "Wrong number of parameters; nextScene requires exactly 0.",
            span,
        );
        None
    }
}

fn fn_number(context: &mut ModuleContext, span: Span, args: &[ast::Expr]) -> Option<hir::ExprKind> {
    if args.len() == 1 {
        let value = resolve_expr(context, &args[0]);
        Some(hir::ExprKind::CastToNumber(Box::new(value)))
    } else {
        context.error(
            "Wrong number of parameters; number requires exactly 1.",
            span,
        );
        None
    }
}

fn fn_ord(context: &mut ModuleContext, span: Span, args: &[ast::Expr]) -> Option<hir::ExprKind> {
    if args.len() == 1 {
        let value = resolve_expr(context, &args[0]);
        Some(hir::ExprKind::CharToAscii(Box::new(value)))
    } else {
        context.error("Wrong number of parameters; ord requires exactly 1.", span);
        None
    }
}

fn fn_play(context: &mut ModuleContext, span: Span, args: &[ast::Expr]) -> Option<hir::ExprKind> {
    if args.is_empty() {
        Some(hir::ExprKind::Play)
    } else {
        context.error("Wrong number of parameters; play requires exactly 0.", span);
        None
    }
}

fn fn_fscommand(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    let (command, args) = if args.len() == 1 {
        let command = resolve_expr(context, &args[0]);
        (
            command,
            hir::Expr::new(
                span,
                hir::ExprKind::Constant(hir::ConstantKind::String("".to_string())),
            ),
        )
    } else if args.len() == 2 {
        let command = resolve_expr(context, &args[0]);
        let args = resolve_expr(context, &args[1]);
        (command, args)
    } else {
        context.error(
            "Wrong number of parameters; FSCommand requires between 1 and 2.",
            span,
        );
        return None;
    };
    Some(hir::ExprKind::GetUrl {
        target: Box::new(args),
        url: Box::new(hir::Expr::new(
            span,
            hir::ExprKind::BinaryOperator(
                hir::BinaryOperator::StringAdd,
                Box::new(hir::Expr::new(
                    span,
                    hir::ExprKind::Constant(hir::ConstantKind::String("FSCommand:".to_string())),
                )),
                Box::new(command),
            ),
        )),
        load_variables: false,
        load_target: false,
        method: hir::GetUrlMethod::None,
    })
}

fn fn_print(context: &mut ModuleContext, span: Span, args: &[ast::Expr]) -> Option<hir::ExprKind> {
    if args.len() == 2 {
        let target = resolve_expr(context, &args[0]);
        let bounds = get_bounds_type(context, &args[1])?;
        let url = format!("print:{}", bounds);

        Some(hir::ExprKind::GetUrl {
            target: Box::new(target),
            url: Box::new(hir::Expr::new(
                args[1].span,
                hir::ExprKind::Constant(hir::ConstantKind::String(url)),
            )),
            load_variables: false,
            load_target: false,
            method: hir::GetUrlMethod::None,
        })
    } else {
        context.error(
            "Wrong number of parameters; print requires exactly 2.",
            span,
        );
        None
    }
}

fn fn_print_as_bitmap(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.len() == 2 {
        let target = resolve_expr(context, &args[0]);
        let bounds = get_bounds_type(context, &args[1])?;
        let url = format!("printasbitmap:{}", bounds);

        Some(hir::ExprKind::GetUrl {
            target: Box::new(target),
            url: Box::new(hir::Expr::new(
                args[1].span,
                hir::ExprKind::Constant(hir::ConstantKind::String(url)),
            )),
            load_variables: false,
            load_target: false,
            method: hir::GetUrlMethod::None,
        })
    } else {
        context.error(
            "Wrong number of parameters; printAsBitmap requires exactly 2.",
            span,
        );
        None
    }
}

fn fn_print_as_bitmap_num(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.len() == 2 {
        let target = resolve_expr(context, &args[0]);
        let bounds = get_bounds_type(context, &args[1])?;
        let url = format!("printasbitmap:{}", bounds);

        Some(hir::ExprKind::GetUrl {
            target: Box::new(hir::Expr::new(
                span,
                hir::ExprKind::BinaryOperator(
                    hir::BinaryOperator::StringAdd,
                    Box::new(hir::Expr::new(
                        span,
                        hir::ExprKind::Constant(hir::ConstantKind::String("_level:".to_string())),
                    )),
                    Box::new(target),
                ),
            )),
            url: Box::new(hir::Expr::new(
                args[1].span,
                hir::ExprKind::Constant(hir::ConstantKind::String(url)),
            )),
            load_variables: false,
            load_target: false,
            method: hir::GetUrlMethod::None,
        })
    } else {
        context.error(
            "Wrong number of parameters; printAsBitmapNum requires exactly 2.",
            span,
        );
        None
    }
}

fn fn_print_num(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.len() == 2 {
        let target = resolve_expr(context, &args[0]);
        let bounds = get_bounds_type(context, &args[1])?;
        let url = format!("print:{}", bounds);

        Some(hir::ExprKind::GetUrl {
            target: Box::new(hir::Expr::new(
                span,
                hir::ExprKind::BinaryOperator(
                    hir::BinaryOperator::StringAdd,
                    Box::new(hir::Expr::new(
                        span,
                        hir::ExprKind::Constant(hir::ConstantKind::String("_level:".to_string())),
                    )),
                    Box::new(target),
                ),
            )),
            url: Box::new(hir::Expr::new(
                args[1].span,
                hir::ExprKind::Constant(hir::ConstantKind::String(url)),
            )),
            load_variables: false,
            load_target: false,
            method: hir::GetUrlMethod::None,
        })
    } else {
        context.error(
            "Wrong number of parameters; printNum requires exactly 2.",
            span,
        );
        None
    }
}

fn fn_start_drag(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.is_empty() || args.len() > 6 {
        context.error(
            "Wrong number of parameters; startDrag requires between 1 and 6.",
            span,
        );
        return None;
    }
    let target = resolve_expr(context, &args[0]); // Only guaranteed argument
    let lock = match args.get(1) {
        Some(ast::Expr {
            value: ast::ExprKind::Constant(ast::ConstantKind::Identifier(value)),
            ..
        }) if *value == "true" => true,
        Some(ast::Expr {
            value: ast::ExprKind::Constant(ast::ConstantKind::Identifier(value)),
            ..
        }) if *value == "false" => false,
        None => false,
        Some(other) => {
            context.error("Lock center parameter must be true or false", other.span);
            return None;
        }
    };

    if args.len() == 6 {
        Some(hir::ExprKind::StartDrag {
            target: Box::new(target),
            lock,
            constraints: Some((
                Box::new(resolve_expr(context, &args[2])),
                Box::new(resolve_expr(context, &args[3])),
                Box::new(resolve_expr(context, &args[4])),
                Box::new(resolve_expr(context, &args[5])),
            )),
        })
    } else if args.len() < 3 {
        Some(hir::ExprKind::StartDrag {
            target: Box::new(target),
            lock,
            constraints: None,
        })
    } else {
        context.error(
            "startDrag requires 1 (target), 2 (target+lock) or 6 (target+lock+constraint) parameters.",
            span,
        );
        None
    }
}

fn fn_stop(context: &mut ModuleContext, span: Span, args: &[ast::Expr]) -> Option<hir::ExprKind> {
    if args.is_empty() {
        Some(hir::ExprKind::Stop)
    } else {
        context.error("Wrong number of parameters; stop requires exactly 0.", span);
        None
    }
}

fn fn_stop_all_sounds(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.is_empty() {
        Some(hir::ExprKind::StopSounds)
    } else {
        context.error(
            "Wrong number of parameters; stopAllSounds requires exactly 0.",
            span,
        );
        None
    }
}

fn fn_stop_drag(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.is_empty() {
        Some(hir::ExprKind::EndDrag)
    } else {
        context.error(
            "Wrong number of parameters; stopDrag requires exactly 0.",
            span,
        );
        None
    }
}

fn fn_string(context: &mut ModuleContext, span: Span, args: &[ast::Expr]) -> Option<hir::ExprKind> {
    if args.len() == 1 {
        let value = resolve_expr(context, &args[0]);
        Some(hir::ExprKind::CastToString(Box::new(value)))
    } else {
        context.error(
            "Wrong number of parameters; String requires exactly 1.",
            span,
        );
        None
    }
}

fn fn_target_path(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.len() == 1 {
        let value = resolve_expr(context, &args[0]);
        Some(hir::ExprKind::GetTargetPath(Box::new(value)))
    } else {
        context.error(
            "Wrong number of parameters; targetPath requires exactly 1.",
            span,
        );
        None
    }
}

fn fn_toggle_high_quality(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.is_empty() {
        Some(hir::ExprKind::ToggleQuality)
    } else {
        context.error(
            "Wrong number of parameters; toggleHighQuality requires exactly 0.",
            span,
        );
        None
    }
}

fn fn_trace(context: &mut ModuleContext, span: Span, args: &[ast::Expr]) -> Option<hir::ExprKind> {
    if args.len() == 1 {
        let value = resolve_expr(context, &args[0]);
        Some(hir::ExprKind::Trace(Box::new(value)))
    } else {
        context.error(
            "Wrong number of parameters; trace requires exactly 1.",
            span,
        );
        None
    }
}

fn fn_remove_movie_clio(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.len() == 1 {
        let value = resolve_expr(context, &args[0]);
        Some(hir::ExprKind::RemoveSprite(Box::new(value)))
    } else {
        context.error(
            "Wrong number of parameters; removeMovieClip requires exactly 1.",
            span,
        );
        None
    }
}

fn fn_random(context: &mut ModuleContext, span: Span, args: &[ast::Expr]) -> Option<hir::ExprKind> {
    if args.len() == 1 {
        let value = resolve_expr(context, &args[0]);
        Some(hir::ExprKind::GetRandomNumber(Box::new(value)))
    } else {
        context.error(
            "Wrong number of parameters; random requires exactly 1.",
            span,
        );
        None
    }
}

fn fn_unload_movie(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.len() == 1 {
        let target = resolve_expr(context, &args[0]);
        Some(hir::ExprKind::GetUrl {
            target: Box::new(target),
            url: Box::new(hir::Expr::new(
                span,
                hir::ExprKind::Constant(hir::ConstantKind::String("".to_string())),
            )),
            load_variables: false,
            load_target: true,
            method: hir::GetUrlMethod::None,
        })
    } else {
        context.error(
            "Wrong number of parameters; unloadMovie requires exactly 1.",
            span,
        );
        None
    }
}

fn fn_get_property(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.len() != 2 {
        context.error(
            "Wrong number of parameters; getProperty requires exactly 2.",
            span,
        );
        return None;
    }
    let target = resolve_expr(context, &args[0]);
    let ast::ExprKind::Constant(ast::ConstantKind::Identifier(name)) = &args[1].value else {
        context.error("Property name expected in GetProperty.", args[1].span);
        return None;
    };
    let Some(property) = get_special_property(name) else {
        context.error("Property name expected in GetProperty.", args[1].span);
        return None;
    };
    Some(hir::ExprKind::GetProperty(Box::new(target), property))
}

fn fn_set_property(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.len() != 3 {
        context.error(
            "Wrong number of parameters; setProperty requires exactly 3.",
            span,
        );
        return None;
    }
    let target = resolve_expr(context, &args[0]);
    let ast::ExprKind::Constant(ast::ConstantKind::Identifier(name)) = &args[1].value else {
        // [NA] I've been trying to match error messages to Flash, but... they say GetProperty here and that's just dumb. <_<
        context.error("Property name expected in SetProperty.", args[1].span);
        return None;
    };
    let Some(property) = get_special_property(name) else {
        // [NA] I've been trying to match error messages to Flash, but... they say GetProperty here and that's just dumb. <_<
        context.error("Property name expected in SetProperty.", args[1].span);
        return None;
    };
    let value = resolve_expr(context, &args[2]);
    Some(hir::ExprKind::SetProperty(
        Box::new(target),
        property,
        Box::new(value),
    ))
}

fn fn_unload_movie_num(
    context: &mut ModuleContext,
    span: Span,
    args: &[ast::Expr],
) -> Option<hir::ExprKind> {
    if args.len() != 1 {
        context.error(
            "Wrong number of parameters; unloadMovieNum requires exactly 1.",
            span,
        );
        return None;
    }

    let target = resolve_expr(context, &args[0]);
    Some(hir::ExprKind::GetUrl {
        target: Box::new(hir::Expr::new(
            span,
            hir::ExprKind::BinaryOperator(
                hir::BinaryOperator::StringAdd,
                Box::new(hir::Expr::new(
                    span,
                    hir::ExprKind::Constant(hir::ConstantKind::String("_level".to_string())),
                )),
                Box::new(target),
            ),
        )),
        url: Box::new(hir::Expr::new(
            span,
            hir::ExprKind::Constant(hir::ConstantKind::String("".to_string())),
        )),
        load_variables: false,
        load_target: false,
        method: hir::GetUrlMethod::None,
    })
}

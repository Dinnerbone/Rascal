use crate::lexer::tokens::{ActionName, Token, TokenKind};
use crate::parser::Tokens;
use crate::pcode::{Action, Actions, PushValue};
use winnow::combinator::{alt, fail, peek, separated};
use winnow::error::{ContextError, ErrMode, ParserError, StrContext, StrContextValue};
use winnow::stream::Stream;
use winnow::token::{any, take_while};
use winnow::{ModalResult, Parser};

pub(crate) fn actions<'i>(
    is_block: bool,
) -> impl Parser<Tokens<'i>, Actions, ErrMode<ContextError>> {
    move |i: &mut Tokens<'i>| {
        let mut actions = Actions::empty();

        take_while(0.., TokenKind::Newline).parse_next(i)?;

        while !i.is_empty() {
            let next = peek(any).parse_next(i)?;

            if next.kind == TokenKind::CloseBrace && is_block {
                break;
            }
            if next.kind == TokenKind::Identifier {
                let identifier = label.parse_next(i)?;
                TokenKind::Colon.parse_next(i)?;
                actions.push_label(identifier);
                continue;
            }
            actions.push(action.parse_next(i)?);
            if !i.is_empty() {
                take_while(1.., TokenKind::Newline).parse_next(i)?;
            }
        }

        Ok(actions)
    }
}

pub(crate) fn action(i: &mut Tokens<'_>) -> ModalResult<Action> {
    let Token {
        kind: TokenKind::ActionName(name),
        ..
    } = any.parse_next(i)?
    else {
        return fail.context(StrContext::Label("action name")).parse_next(i);
    };

    Ok(match name {
        ActionName::Add => Action::Add,
        ActionName::Add2 => Action::Add2,
        ActionName::BitAnd => Action::BitAnd,
        ActionName::BitLShift => Action::BitLShift,
        ActionName::BitOr => Action::BitOr,
        ActionName::BitRShift => Action::BitRShift,
        ActionName::BitURShift => Action::BitURShift,
        ActionName::BitXor => Action::BitXor,
        ActionName::CallFunction => Action::CallFunction,
        ActionName::CallMethod => Action::CallMethod,
        ActionName::ConstantPool => constant_pool.parse_next(i)?,
        ActionName::Decrement => Action::Decrement,
        ActionName::DefineFunction => define_function.parse_next(i)?,
        ActionName::DefineLocal => Action::DefineLocal,
        ActionName::DefineLocal2 => Action::DefineLocal2,
        ActionName::Divide => Action::Divide,
        ActionName::Equals2 => Action::Equals2,
        ActionName::GetMember => Action::GetMember,
        ActionName::GetVariable => Action::GetVariable,
        ActionName::Greater => Action::Greater,
        ActionName::If => if_.parse_next(i)?,
        ActionName::Increment => Action::Increment,
        ActionName::InitObject => Action::InitObject,
        ActionName::InstanceOf => Action::InstanceOf,
        ActionName::Jump => jump.parse_next(i)?,
        ActionName::Less2 => Action::Less2,
        ActionName::Modulo => Action::Modulo,
        ActionName::Multiply => Action::Multiply,
        ActionName::NewMethod => Action::NewMethod,
        ActionName::NewObject => Action::NewObject,
        ActionName::Not => Action::Not,
        ActionName::Pop => Action::Pop,
        ActionName::Push => push.parse_next(i)?,
        ActionName::PushDuplicate => Action::PushDuplicate,
        ActionName::RandomNumber => Action::RandomNumber,
        ActionName::Return => Action::Return,
        ActionName::SetVariable => Action::SetVariable,
        ActionName::StoreRegister => store_register.parse_next(i)?,
        ActionName::StrictEquals => Action::StrictEquals,
        ActionName::Subtract => Action::Subtract,
        ActionName::Trace => Action::Trace,
        ActionName::TypeOf => Action::TypeOf,
    })
}

pub fn if_(i: &mut Tokens<'_>) -> ModalResult<Action> {
    let label = label.parse_next(i)?;
    Ok(Action::If(label))
}

pub fn jump(i: &mut Tokens<'_>) -> ModalResult<Action> {
    let label = label.parse_next(i)?;
    Ok(Action::Jump(label))
}

pub fn store_register(i: &mut Tokens<'_>) -> ModalResult<Action> {
    let n = register_num.parse_next(i)?;
    Ok(Action::StoreRegister(n))
}

pub fn push(i: &mut Tokens<'_>) -> ModalResult<Action> {
    let values = separated(0.., push_value, TokenKind::Comma).parse_next(i)?;
    Ok(Action::Push(values))
}

pub fn constant_pool(i: &mut Tokens<'_>) -> ModalResult<Action> {
    let values = separated(0.., string, TokenKind::Comma).parse_next(i)?;
    Ok(Action::ConstantPool(values))
}

pub fn define_function(i: &mut Tokens<'_>) -> ModalResult<Action> {
    let name = string.parse_next(i)?;
    TokenKind::Comma.parse_next(i)?;
    let num_args = integer.parse_next(i)?;
    let args = if num_args > 0 {
        TokenKind::Comma.parse_next(i)?;
        separated(num_args as usize, string, TokenKind::Comma).parse_next(i)?
    } else {
        vec![]
    };
    TokenKind::OpenBrace.parse_next(i)?;
    let body = actions(true).parse_next(i)?;
    TokenKind::CloseBrace.parse_next(i)?;
    Ok(Action::DefineFunction {
        name,
        params: args,
        actions: body,
    })
}

pub fn string(i: &mut Tokens<'_>) -> ModalResult<String> {
    // TODO parse
    TokenKind::String.parse_next(i).map(|t| t.raw.to_owned())
}

pub fn label(i: &mut Tokens<'_>) -> ModalResult<String> {
    TokenKind::Identifier
        .context(StrContext::Label("label"))
        .parse_next(i)
        .map(|t| t.raw.to_owned())
}

fn integer_or_float(i: &mut Tokens<'_>) -> ModalResult<PushValue> {
    let raw = alt((TokenKind::Integer, TokenKind::Float))
        .parse_next(i)?
        .raw;
    if let Ok(n) = raw.parse::<i32>() {
        return Ok(PushValue::Integer(n));
    }
    raw.parse::<f64>()
        .map_err(|_| ParserError::from_input(&raw))
        .map(PushValue::Float)
}

fn register_num(i: &mut Tokens<'_>) -> ModalResult<u8> {
    let raw = TokenKind::Integer.parse_next(i)?.raw;
    raw.parse::<u8>().map_err(|_| ParserError::from_input(&raw))
}

fn integer(i: &mut Tokens<'_>) -> ModalResult<i32> {
    let raw = TokenKind::Integer.parse_next(i)?.raw;
    raw.parse::<i32>()
        .map_err(|_| ParserError::from_input(&raw))
}

fn float(i: &mut Tokens<'_>) -> ModalResult<f64> {
    let raw = TokenKind::Float.parse_next(i)?.raw;
    raw.parse::<f64>()
        .map_err(|_| ParserError::from_input(&raw))
}

pub(crate) fn push_value(i: &mut Tokens<'_>) -> ModalResult<PushValue> {
    let checkpoint = i.checkpoint();
    let token = any.parse_next(i)?;
    match token.kind {
        TokenKind::String => {
            i.reset(&checkpoint);
            string.parse_next(i).map(PushValue::String)
        }
        TokenKind::Register(n) => Ok(PushValue::Register(n)),
        TokenKind::Constant(n) => Ok(PushValue::Constant(n)),
        TokenKind::False => Ok(PushValue::False),
        TokenKind::True => Ok(PushValue::True),
        TokenKind::Undefined => Ok(PushValue::Undefined),
        TokenKind::Null => Ok(PushValue::Null),
        TokenKind::Integer => {
            i.reset(&checkpoint);
            integer_or_float.parse_next(i)
        }
        TokenKind::Float => {
            i.reset(&checkpoint);
            float.parse_next(i).map(PushValue::Float)
        }
        _ => fail
            .context(StrContext::Label("value"))
            .context(StrContext::Expected(StrContextValue::Description("string")))
            .context(StrContext::Expected(StrContextValue::Description(
                "constant",
            )))
            .context(StrContext::Expected(StrContextValue::Description(
                "register",
            )))
            .context(StrContext::Expected(StrContextValue::Description(
                "integer",
            )))
            .context(StrContext::Expected(StrContextValue::Description("float")))
            .context(StrContext::Expected(StrContextValue::StringLiteral("true")))
            .context(StrContext::Expected(StrContextValue::StringLiteral(
                "false",
            )))
            .context(StrContext::Expected(StrContextValue::StringLiteral("null")))
            .context(StrContext::Expected(StrContextValue::StringLiteral(
                "undefined",
            )))
            .parse_next(i),
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::tokens::{ActionName, TokenKind};
    use crate::parser::parse_actions;
    use crate::parser::tests::build_tokens;
    use crate::pcode::{Action, Actions, PushValue};
    use indexmap::IndexMap;

    macro_rules! trivial_action_tests {
        ($($test_name:ident => $action_name:ident),* $(,)?) => {
            $(
                #[test]
                fn $test_name() {
                    let tokens = [(TokenKind::ActionName(ActionName::$action_name), "")];
                    let tokens = build_tokens(&tokens);
                    assert_eq!(
                        parse_actions(&tokens),
                        Ok(Actions {
                            actions: vec![Action::$action_name],
                            label_positions: Default::default()
                        })
                    )
                }
            )*
        }
    }

    trivial_action_tests! {
        test_add => Add,
        test_add2 => Add2,
        test_bitand => BitAnd,
        test_bitlshift => BitLShift,
        test_bitor => BitOr,
        test_bitrshift => BitRShift,
        test_biturshift => BitURShift,
        test_bitxor => BitXor,
        test_call_function => CallFunction,
        test_call_method => CallMethod,
        test_decrement => Decrement,
        test_define_local => DefineLocal,
        test_definelocal2 => DefineLocal2,
        test_divide => Divide,
        test_equals2 => Equals2,
        test_get_member => GetMember,
        test_get_variable => GetVariable,
        test_greater => Greater,
        test_increment => Increment,
        test_init_object => InitObject,
        test_instanceof => InstanceOf,
        test_less2 => Less2,
        test_modulo => Modulo,
        test_multiply => Multiply,
        test_new_method => NewMethod,
        test_new_object => NewObject,
        test_not => Not,
        test_pop => Pop,
        test_pushduplicate => PushDuplicate,
        test_random_number => RandomNumber,
        test_return => Return,
        test_set_variable => SetVariable,
        test_subtract => Subtract,
        test_strictequals => StrictEquals,
        test_trace => Trace,
        test_typeof => TypeOf,
    }

    #[test]
    fn test_multiple_labels() {
        let tokens = build_tokens(&[
            (TokenKind::Identifier, "start"),
            (TokenKind::Colon, ":"),
            (TokenKind::Identifier, "end"),
            (TokenKind::Colon, ":"),
            (TokenKind::ActionName(ActionName::Trace), "trace"),
        ]);
        assert_eq!(
            parse_actions(&tokens),
            Ok(Actions {
                actions: vec![Action::Trace],
                label_positions: IndexMap::from([("start".to_owned(), 0), ("end".to_owned(), 0)])
            })
        )
    }

    #[test]
    fn test_constant_pool() {
        let tokens = build_tokens(&[
            (
                TokenKind::ActionName(ActionName::ConstantPool),
                "constantPool",
            ),
            (TokenKind::String, "foo"),
            (TokenKind::Comma, ","),
            (TokenKind::String, "bar"),
        ]);
        assert_eq!(
            parse_actions(&tokens),
            Ok(Actions {
                actions: vec![Action::ConstantPool(vec![
                    "foo".to_owned(),
                    "bar".to_owned()
                ])],
                label_positions: Default::default()
            })
        )
    }

    #[test]
    fn test_push_all_values() {
        let tokens = build_tokens(&[
            (TokenKind::ActionName(ActionName::Push), "push"),
            (TokenKind::Register(0), "0"),
            (TokenKind::Comma, ","),
            (TokenKind::String, "foo"),
            (TokenKind::Comma, ","),
            (TokenKind::Integer, "1"),
            (TokenKind::Comma, ","),
            (TokenKind::Float, "1.0"),
            (TokenKind::Comma, ","),
            (TokenKind::True, "true"),
            (TokenKind::Comma, ","),
            (TokenKind::False, "false"),
            (TokenKind::Comma, ","),
            (TokenKind::Null, "null"),
            (TokenKind::Comma, ","),
            (TokenKind::Undefined, "undefined"),
            (TokenKind::Comma, ","),
            (TokenKind::Constant(1), "1"),
        ]);
        assert_eq!(
            parse_actions(&tokens),
            Ok(Actions {
                actions: vec![Action::Push(vec![
                    PushValue::Register(0),
                    PushValue::String("foo".to_owned()),
                    PushValue::Integer(1),
                    PushValue::Float(1.0),
                    PushValue::True,
                    PushValue::False,
                    PushValue::Null,
                    PushValue::Undefined,
                    PushValue::Constant(1),
                ])],
                label_positions: Default::default(),
            })
        )
    }

    #[test]
    fn test_if() {
        let tokens = build_tokens(&[
            (TokenKind::ActionName(ActionName::If), "if"),
            (TokenKind::Identifier, "end"),
        ]);
        assert_eq!(
            parse_actions(&tokens),
            Ok(Actions {
                actions: vec![Action::If("end".to_owned())],
                label_positions: Default::default()
            })
        )
    }

    #[test]
    fn test_jump() {
        let tokens = build_tokens(&[
            (TokenKind::ActionName(ActionName::Jump), "jump"),
            (TokenKind::Identifier, "end"),
        ]);
        assert_eq!(
            parse_actions(&tokens),
            Ok(Actions {
                actions: vec![Action::Jump("end".to_owned())],
                label_positions: Default::default()
            })
        )
    }

    #[test]
    fn test_store_register() {
        let tokens = build_tokens(&[
            (
                TokenKind::ActionName(ActionName::StoreRegister),
                "StoreRegister",
            ),
            (TokenKind::Integer, "0"),
        ]);
        assert_eq!(
            parse_actions(&tokens),
            Ok(Actions {
                actions: vec![Action::StoreRegister(0)],
                label_positions: Default::default()
            })
        )
    }

    #[test]
    fn test_define_function() {
        let tokens = build_tokens(&[
            (
                TokenKind::ActionName(ActionName::DefineFunction),
                "DefineFunction",
            ),
            (TokenKind::String, "foo"),
            (TokenKind::Comma, ","),
            (TokenKind::Integer, "1"),
            (TokenKind::Comma, ","),
            (TokenKind::String, "a"),
            (TokenKind::OpenBrace, "{"),
            (TokenKind::ActionName(ActionName::Push), "push"),
            (TokenKind::String, "a"),
            (TokenKind::Newline, "\n"),
            (TokenKind::ActionName(ActionName::Trace), "trace"),
            (TokenKind::Newline, "\n"),
            (TokenKind::CloseBrace, "}"),
        ]);
        assert_eq!(
            parse_actions(&tokens),
            Ok(Actions {
                actions: vec![Action::DefineFunction {
                    name: "foo".to_string(),
                    params: vec!["a".to_string()],
                    actions: Actions {
                        actions: vec![
                            Action::Push(vec![PushValue::String("a".to_owned())]),
                            Action::Trace
                        ],
                        label_positions: Default::default()
                    },
                }],
                label_positions: Default::default()
            })
        )
    }
}

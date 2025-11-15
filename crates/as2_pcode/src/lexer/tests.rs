use crate::lexer::Lexer;
use crate::lexer::tokens::{ActionName, TokenKind};

fn kinds(input: &str) -> Vec<TokenKind> {
    Lexer::new(input)
        .into_vec()
        .into_iter()
        .map(|t| t.kind)
        .collect()
}

fn raws(input: &str) -> Vec<String> {
    Lexer::new(input)
        .into_vec()
        .into_iter()
        .map(|t| t.raw.to_string())
        .collect()
}

#[test]
fn test_all_samples() {
    insta::glob!("../../../../samples/as2_pcode", "**/*.pcode", |path| {
        let src = std::fs::read_to_string(path).expect("failed to read sample");
        let tokens = Lexer::new(&src).into_vec();
        insta::assert_yaml_snapshot!(tokens);
    });
}

#[test]
fn test_identifiers() {
    let str = "loc01fc puuush notanaction constant";
    assert_eq!(
        kinds(str),
        vec![
            TokenKind::Identifier,
            TokenKind::Identifier,
            TokenKind::Identifier,
            TokenKind::Identifier
        ]
    );
    assert_eq!(
        raws(str),
        vec![
            "loc01fc".to_string(),
            "puuush".to_string(),
            "notanaction".to_string(),
            "constant".to_string()
        ]
    );
}

#[test]
fn test_actions() {
    assert_eq!(
        kinds(
            "push PUsh constantpool trace add2 getvariable definelocal randomnumber equals2 if setvariable not jump add"
        ),
        [
            ActionName::Push,
            ActionName::Push,
            ActionName::ConstantPool,
            ActionName::Trace,
            ActionName::Add2,
            ActionName::GetVariable,
            ActionName::DefineLocal,
            ActionName::RandomNumber,
            ActionName::Equals2,
            ActionName::If,
            ActionName::SetVariable,
            ActionName::Not,
            ActionName::Jump,
            ActionName::Add,
        ]
        .map(TokenKind::ActionName)
    );
}

#[test]
fn test_constants() {
    let input = "constant0 constant1 constant2 constant2000";
    assert_eq!(
        kinds(input),
        vec![
            TokenKind::Constant(0),
            TokenKind::Constant(1),
            TokenKind::Constant(2),
            TokenKind::Constant(2000)
        ]
    );
    assert_eq!(
        raws(input),
        vec![
            "constant0".to_string(),
            "constant1".to_string(),
            "constant2".to_string(),
            "constant2000".to_string()
        ]
    );
}

#[test]
fn test_registers() {
    let input = "register0 register1 register2 register4";
    assert_eq!(
        kinds(input),
        vec![
            TokenKind::Register(0),
            TokenKind::Register(1),
            TokenKind::Register(2),
            TokenKind::Register(4)
        ]
    );
    assert_eq!(
        raws(input),
        vec![
            "register0".to_string(),
            "register1".to_string(),
            "register2".to_string(),
            "register4".to_string()
        ]
    );
}

#[test]
fn test_punctuation_tokens() {
    assert_eq!(
        kinds(",:{}"),
        vec![
            TokenKind::Comma,
            TokenKind::Colon,
            TokenKind::OpenBrace,
            TokenKind::CloseBrace,
        ]
    );
}

#[test]
fn test_newlines_variants() {
    assert_eq!(kinds("\n"), vec![TokenKind::Newline]);
    assert_eq!(kinds("\r\n"), vec![TokenKind::Newline]);
    assert_eq!(kinds("\r"), vec![TokenKind::Newline]);
}

#[test]
fn test_string_literal_and_escapes() {
    assert_eq!(kinds("\"hello\""), vec![TokenKind::String]);
    assert_eq!(raws("\"hello\""), vec!["hello".to_string()]);

    let s = "\"a\\\"b'\\\\c\""; // "a\"b\\c"
    assert_eq!(kinds(s), vec![TokenKind::String]);
    assert_eq!(raws(s), vec!["a\\\"b'\\\\c".to_string()]);
}

#[test]
fn test_mixed_sequence() {
    let input = "push \"hello\", 2\r\n";
    assert_eq!(
        kinds(input),
        vec![
            TokenKind::ActionName(ActionName::Push),
            TokenKind::String,
            TokenKind::Comma,
            TokenKind::Integer,
            TokenKind::Newline,
        ]
    );
}

#[test]
fn test_single_digit() {
    let input = "1";
    assert_eq!(kinds(input), vec![TokenKind::Integer]);
    assert_eq!(raws(input), vec!["1".to_string()]);
}

#[test]
fn test_float_trailing_dot() {
    let input = "1.";
    assert_eq!(kinds(input), vec![TokenKind::Float]);
    assert_eq!(raws(input), vec!["1.".to_string()]);
}

#[test]
fn test_float() {
    let input = "1.2";
    assert_eq!(kinds(input), vec![TokenKind::Float]);
    assert_eq!(raws(input), vec!["1.2".to_string()]);
}

#[test]
fn test_long_integer_comma() {
    let input = "1234567890,";
    assert_eq!(kinds(input), vec![TokenKind::Integer, TokenKind::Comma]);
    assert_eq!(raws(input), vec!["1234567890".to_string(), ",".to_string()]);
}

#[test]
fn test_long_float_comma() {
    let input = "12345.67890,";
    assert_eq!(kinds(input), vec![TokenKind::Float, TokenKind::Comma]);
    assert_eq!(
        raws(input),
        vec!["12345.67890".to_string(), ",".to_string()]
    );
}

#[test]
fn test_negative_integer() {
    let input = "-123";
    assert_eq!(kinds(input), vec![TokenKind::Integer]);
    assert_eq!(raws(input), vec!["-123".to_string()]);
}

#[test]
fn test_negative_float() {
    let input = "-123.0";
    assert_eq!(kinds(input), vec![TokenKind::Float]);
    assert_eq!(raws(input), vec!["-123.0".to_string()]);
}

#[test]
fn test_float_exponent_comma() {
    let input = "1.2e+3,";
    assert_eq!(kinds(input), vec![TokenKind::Float, TokenKind::Comma]);
    assert_eq!(raws(input), vec!["1.2e+3".to_string(), ",".to_string()]);
}

#[test]
fn test_integer_exponent_comma() {
    let input = "1e-3,";
    assert_eq!(kinds(input), vec![TokenKind::Integer, TokenKind::Comma]);
    assert_eq!(raws(input), vec!["1e-3".to_string(), ",".to_string()]);
}

#[test]
fn test_integer_exponent_no_value() {
    let input = "1e,";
    assert_eq!(kinds(input), vec![TokenKind::Integer, TokenKind::Comma]);
    assert_eq!(raws(input), vec!["1e".to_string(), ",".to_string()]);
}

#[test]
fn test_true_false() {
    assert_eq!(kinds("true false"), vec![TokenKind::True, TokenKind::False]);
}

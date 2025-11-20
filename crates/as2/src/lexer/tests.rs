use crate::lexer::Lexer;
use crate::lexer::operator::Operator;
use crate::lexer::tokens::{Keyword, QuoteKind, Token, TokenKind};

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
    insta::glob!("../../../../samples/as2", "**/*.as", |path| {
        let src = std::fs::read_to_string(path).expect("failed to read sample");
        let tokens = Lexer::new(&src).into_vec();
        insta::assert_yaml_snapshot!(tokens);
    });
}

#[test]
fn test_line_comment() {
    let input = "// this is a line comment\n";
    assert_eq!(Lexer::new(input).into_vec(), Vec::<Token>::new());
}

#[test]
fn test_empty_line_comment() {
    let input = "//";
    assert_eq!(Lexer::new(input).into_vec(), Vec::<Token>::new());
}

#[test]
fn test_block_comment() {
    let input = "/* this is a block comment */";
    assert_eq!(Lexer::new(input).into_vec(), Vec::<Token>::new());
}

#[test]
fn test_non_ending_block_comment() {
    let input = "/*";
    assert_eq!(Lexer::new(input).into_vec(), Vec::<Token>::new());
}

#[test]
fn test_identifiers_and_keyword() {
    assert_eq!(
        kinds(
            "var x _abc $d Var instanceof new typeof delete in void function return for if else break continue throw try catch finally ifFrameLoaded tellTarget"
        ),
        vec![
            TokenKind::Keyword(Keyword::Var),
            TokenKind::Identifier,
            TokenKind::Identifier,
            TokenKind::Identifier,
            TokenKind::Identifier, // 'Var' is not the keyword (case-sensitive)
            TokenKind::Keyword(Keyword::InstanceOf),
            TokenKind::Keyword(Keyword::New),
            TokenKind::Keyword(Keyword::TypeOf),
            TokenKind::Keyword(Keyword::Delete),
            TokenKind::Keyword(Keyword::In),
            TokenKind::Keyword(Keyword::Void),
            TokenKind::Keyword(Keyword::Function),
            TokenKind::Keyword(Keyword::Return),
            TokenKind::Keyword(Keyword::For),
            TokenKind::Keyword(Keyword::If),
            TokenKind::Keyword(Keyword::Else),
            TokenKind::Keyword(Keyword::Break),
            TokenKind::Keyword(Keyword::Continue),
            TokenKind::Keyword(Keyword::Throw),
            TokenKind::Keyword(Keyword::Try),
            TokenKind::Keyword(Keyword::Catch),
            TokenKind::Keyword(Keyword::Finally),
            TokenKind::Keyword(Keyword::IfFrameLoaded),
            TokenKind::Keyword(Keyword::TellTarget),
        ]
    );
}

#[test]
fn test_operators_single_and_compound() {
    assert_eq!(
        kinds("+ = += - / * % ++ -- -= *= /= %= & ~ | >> << >>> ^ &= |= ^= >>= <<= >>>="),
        vec![
            TokenKind::Operator(Operator::Add),
            TokenKind::Operator(Operator::Assign),
            TokenKind::Operator(Operator::AddAssign),
            TokenKind::Operator(Operator::Sub),
            TokenKind::Operator(Operator::Divide),
            TokenKind::Operator(Operator::Multiply),
            TokenKind::Operator(Operator::Modulo),
            TokenKind::Operator(Operator::Increment),
            TokenKind::Operator(Operator::Decrement),
            TokenKind::Operator(Operator::SubAssign),
            TokenKind::Operator(Operator::MultiplyAssign),
            TokenKind::Operator(Operator::DivideAssign),
            TokenKind::Operator(Operator::ModuloAssign),
            TokenKind::Operator(Operator::BitAnd),
            TokenKind::Operator(Operator::BitNot),
            TokenKind::Operator(Operator::BitOr),
            TokenKind::Operator(Operator::BitShiftRight),
            TokenKind::Operator(Operator::BitShiftLeft),
            TokenKind::Operator(Operator::BitShiftRightUnsigned),
            TokenKind::Operator(Operator::BitXor),
            TokenKind::Operator(Operator::BitAndAssign),
            TokenKind::Operator(Operator::BitOrAssign),
            TokenKind::Operator(Operator::BitXorAssign),
            TokenKind::Operator(Operator::BitShiftRightAssign),
            TokenKind::Operator(Operator::BitShiftLeftAssign),
            TokenKind::Operator(Operator::BitShiftRightUnsignedAssign),
        ]
    );
}

#[test]
fn test_equality_operators() {
    assert_eq!(
        kinds("== != <= >= < > === !=="),
        vec![
            TokenKind::Operator(Operator::Equal),
            TokenKind::Operator(Operator::NotEqual),
            TokenKind::Operator(Operator::LessThanEqual),
            TokenKind::Operator(Operator::GreaterThanEqual),
            TokenKind::Operator(Operator::LessThan),
            TokenKind::Operator(Operator::GreaterThan),
            TokenKind::Operator(Operator::StrictEqual),
            TokenKind::Operator(Operator::StrictNotEqual),
        ]
    );
}

#[test]
fn test_ternary_operator() {
    assert_eq!(kinds("?:"), vec![TokenKind::Question, TokenKind::Colon]);
}

#[test]
fn test_logic_operators() {
    assert_eq!(
        kinds("|| && !"),
        vec![
            TokenKind::Operator(Operator::LogicalOr),
            TokenKind::Operator(Operator::LogicalAnd),
            TokenKind::Operator(Operator::LogicalNot),
        ]
    );
}

#[test]
fn test_punctuation_tokens() {
    assert_eq!(
        kinds("(,) ); {} . []"),
        vec![
            TokenKind::OpenParen,
            TokenKind::Comma,
            TokenKind::CloseParen,
            TokenKind::CloseParen,
            TokenKind::Semicolon,
            TokenKind::OpenBrace,
            TokenKind::CloseBrace,
            TokenKind::Period,
            TokenKind::OpenBracket,
            TokenKind::CloseBracket,
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
    assert_eq!(
        kinds("\"hello\""),
        vec![TokenKind::String(QuoteKind::Double)]
    );
    assert_eq!(raws("\"hello\""), vec!["hello".to_string()]);

    assert_eq!(kinds("'hello'"), vec![TokenKind::String(QuoteKind::Single)]);
    assert_eq!(raws("'hello'"), vec!["hello".to_string()]);

    let s = "\"a\\\"b'\\\\c\""; // "a\"b\\c"
    assert_eq!(kinds(s), vec![TokenKind::String(QuoteKind::Double)]);
    assert_eq!(raws(s), vec!["a\\\"b'\\\\c".to_string()]);

    let s = "\'a\\'b\"\\\\c\'"; // 'a\'b"\c'
    assert_eq!(kinds(s), vec![TokenKind::String(QuoteKind::Single)]);
    assert_eq!(raws(s), vec!["a\\'b\"\\\\c".to_string()]);
}

#[test]
fn test_mixed_sequence() {
    let input = "var x+=y,($z);\r\n";
    assert_eq!(
        kinds(input),
        vec![
            TokenKind::Keyword(Keyword::Var),
            TokenKind::Identifier,
            TokenKind::Operator(Operator::AddAssign),
            TokenKind::Identifier,
            TokenKind::Comma,
            TokenKind::OpenParen,
            TokenKind::Identifier,
            TokenKind::CloseParen,
            TokenKind::Semicolon,
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
fn test_float_leading_dot() {
    let input = ".1";
    assert_eq!(kinds(input), vec![TokenKind::Float]);
    assert_eq!(raws(input), vec![".1".to_string()]);
}

#[test]
fn test_float() {
    let input = "1.2";
    assert_eq!(kinds(input), vec![TokenKind::Float]);
    assert_eq!(raws(input), vec!["1.2".to_string()]);
}

#[test]
fn test_long_integer_semicolon() {
    let input = "1234567890;";
    assert_eq!(kinds(input), vec![TokenKind::Integer, TokenKind::Semicolon]);
    assert_eq!(raws(input), vec!["1234567890".to_string(), ";".to_string()]);
}

#[test]
fn test_long_float_semicolon() {
    let input = "12345.67890;";
    assert_eq!(kinds(input), vec![TokenKind::Float, TokenKind::Semicolon]);
    assert_eq!(
        raws(input),
        vec!["12345.67890".to_string(), ";".to_string()]
    );
}

#[test]
fn test_hex_integer_semicolon() {
    let input = "0x1A2C;";
    assert_eq!(kinds(input), vec![TokenKind::Integer, TokenKind::Semicolon]);
    assert_eq!(raws(input), vec!["0x1A2C".to_string(), ";".to_string()]);
}

#[test]
fn test_float_exponent_semicolon() {
    let input = "1.2e+3;";
    assert_eq!(kinds(input), vec![TokenKind::Float, TokenKind::Semicolon]);
    assert_eq!(raws(input), vec!["1.2e+3".to_string(), ";".to_string()]);
}

#[test]
fn test_integer_exponent_semicolon() {
    let input = "1e-3;";
    assert_eq!(kinds(input), vec![TokenKind::Integer, TokenKind::Semicolon]);
    assert_eq!(raws(input), vec!["1e-3".to_string(), ";".to_string()]);
}

#[test]
fn test_integer_exponent_no_value() {
    let input = "1e;";
    assert_eq!(kinds(input), vec![TokenKind::Integer, TokenKind::Semicolon]);
    assert_eq!(raws(input), vec!["1e".to_string(), ";".to_string()]);
}

#[test]
fn test_float_exponent_no_digits() {
    let input = ".e";
    assert_eq!(kinds(input), vec![TokenKind::Period, TokenKind::Identifier]);
    assert_eq!(raws(input), vec![".".to_string(), "e".to_string()]);
}

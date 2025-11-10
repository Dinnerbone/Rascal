use crate::lexer::Lexer;
use crate::parser::parse_document;

#[test]
fn test_trace() {
    let tokens = Lexer::new(include_str!("../../samples/trace_hello_world.as")).into_vec();
    insta::assert_yaml_snapshot!(parse_document(&tokens).map_err(|e| format!("{e:#?}")));
}

#[test]
fn test_var() {
    let tokens = Lexer::new(include_str!("../../samples/assign_variable_and_trace.as")).into_vec();
    insta::assert_yaml_snapshot!(parse_document(&tokens).map_err(|e| format!("{e:#?}")));
}

#[test]
fn test_var_assignments() {
    let tokens = Lexer::new(include_str!("../../samples/assign_variable_many_ways.as")).into_vec();
    insta::assert_yaml_snapshot!(parse_document(&tokens).map_err(|e| format!("{e:#?}")));
}

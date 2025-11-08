use crate::lexer::Lexer;

#[test]
fn test_trace() {
    insta::assert_yaml_snapshot!(
        Lexer::new(include_str!("../../samples/trace_hello_world.as")).into_vec()
    );
}

#[test]
fn test_var() {
    insta::assert_yaml_snapshot!(
        Lexer::new(include_str!("../../samples/assign_variable_and_trace.as")).into_vec()
    );
}

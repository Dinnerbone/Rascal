use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
#[allow(dead_code)]
pub(crate) enum Expr {
    Call {
        name: String,
        args: Vec<Expr>,
    },
    String(String),
    Identifier(String),
    Declare {
        name: String,
        value: Option<Box<Expr>>,
    },
}

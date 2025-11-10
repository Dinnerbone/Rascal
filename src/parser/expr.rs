use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
#[allow(dead_code)]
pub(crate) enum Expr {
    Declare { name: String, value: Option<Eval> },
    EVal(Eval),
}

#[derive(Debug, Clone, Serialize)]
#[allow(dead_code)]
pub(crate) enum Eval {
    Constant(Constant),
    Call { name: Box<Eval>, args: Vec<Eval> },
}

#[derive(Debug, Clone, Serialize)]
#[allow(dead_code)]
pub(crate) enum Constant {
    String(String),
    Identifier(String),
}

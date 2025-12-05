use rascal_common::span::Spanned;
use serde::Serialize;

pub type Expr = Spanned<ExprKind>;
pub type Constant = Spanned<ConstantKind>;
pub type Statement = Spanned<StatementKind>;
pub use crate::ast::{Affix, BinaryOperator, UnaryOperator};

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum ExprKind {
    Constant(ConstantKind),
    Call {
        name: Box<Expr>,
        args: Vec<Expr>,
    },
    New {
        name: Box<Expr>,
        args: Vec<Expr>,
    },
    BinaryOperator(BinaryOperator, Box<Expr>, Box<Expr>),
    UnaryOperator(UnaryOperator, Box<Expr>),
    Ternary {
        condition: Box<Expr>,
        yes: Box<Expr>,
        no: Box<Expr>,
    },
    InitObject(Vec<(String, Expr)>),
    InitArray(Vec<Expr>),
    Field(Box<Expr>, Box<Expr>),
    TypeOf(Vec<Expr>),
    Delete(Vec<Expr>),
    Void(Vec<Expr>),
    Function(Function),
    GetVariable(Box<Expr>),
    SetVariable(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum ConstantKind {
    String(String),
    Identifier(String),
    Float(f64),
    Integer(i32),
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum StatementKind {
    Declare(Vec<Declaration>),
    Return(Vec<Expr>),
    Throw(Vec<Expr>),
    Expr(Expr),
    Block(Vec<StatementKind>),
    ForIn {
        condition: ForCondition,
        body: Box<StatementKind>,
    },
    While {
        condition: Expr,
        body: Box<StatementKind>,
    },
    If {
        condition: Expr,
        yes: Box<StatementKind>,
        no: Option<Box<StatementKind>>,
    },
    Break,
    Continue,
    Try(TryCatch),
    WaitForFrame {
        frame: Expr,
        scene: Option<Expr>,
        if_loaded: Box<StatementKind>,
    },
    TellTarget {
        target: Expr,
        body: Box<StatementKind>,
    },
    InlinePCode(String),
    With {
        target: Expr,
        body: Box<StatementKind>,
    },
    Switch {
        target: Expr,
        elements: Vec<SwitchElement>,
    },
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum SwitchElement {
    Case(Expr),
    Default,
    Statement(StatementKind),
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum ForCondition {
    Enumerate {
        variable: String,
        declare: bool,
        object: Expr,
    },
    Classic {
        initialize: Option<Box<StatementKind>>,
        // This is technically incorrect, we treat `a++, b++` as two different expressions, but they should be one (and we'd have a `Next(a, b)` expression)
        // Unfortunately that seems difficult to implement _correctly_, so this is a good stopgap (did anyone even use `,` in regular code, outside for loops?)
        condition: Vec<Expr>,
        update: Vec<Expr>,
    },
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Function {
    pub name: Option<String>,
    pub args: Vec<FunctionArgument>,
    pub body: Vec<StatementKind>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct FunctionArgument {
    pub name: String,
    pub type_name: Option<Spanned<String>>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Declaration {
    pub name: String,
    pub value: Option<Expr>,
    pub type_name: Option<Spanned<String>>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct TryCatch {
    pub try_body: Vec<StatementKind>,
    pub typed_catches: Vec<(Spanned<String>, Catch)>,
    pub catch_all: Option<Catch>,
    pub finally: Vec<StatementKind>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Catch {
    pub name: Spanned<String>,
    pub body: Vec<StatementKind>,
}

#[derive(Debug, Serialize)]
pub struct Document {
    pub statements: Vec<StatementKind>,
}

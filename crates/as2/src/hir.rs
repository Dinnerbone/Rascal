use rascal_common::span::Spanned;
use serde::Serialize;

pub type Expr<'a> = Spanned<ExprKind<'a>>;
pub type Constant<'a> = Spanned<ConstantKind<'a>>;
pub type Statement<'a> = Spanned<StatementKind<'a>>;
pub use crate::ast::{Affix, BinaryOperator, ConstantKind, UnaryOperator};

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum ExprKind<'a> {
    Constant(ConstantKind<'a>),
    Call {
        name: Box<Expr<'a>>,
        args: Vec<Expr<'a>>,
    },
    New {
        name: Box<Expr<'a>>,
        args: Vec<Expr<'a>>,
    },
    BinaryOperator(BinaryOperator, Box<Expr<'a>>, Box<Expr<'a>>),
    UnaryOperator(UnaryOperator, Box<Expr<'a>>),
    Ternary {
        condition: Box<Expr<'a>>,
        yes: Box<Expr<'a>>,
        no: Box<Expr<'a>>,
    },
    InitObject(Vec<(&'a str, Expr<'a>)>),
    InitArray(Vec<Expr<'a>>),
    Field(Box<Expr<'a>>, Box<Expr<'a>>),
    TypeOf(Vec<Expr<'a>>),
    Delete(Vec<Expr<'a>>),
    Void(Vec<Expr<'a>>),
    Function(Function<'a>),
    GetVariable(Box<Expr<'a>>),
    SetVariable(Box<Expr<'a>>, Box<Expr<'a>>),
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum StatementKind<'a> {
    Declare(Vec<Declaration<'a>>),
    Return(Vec<Expr<'a>>),
    Throw(Vec<Expr<'a>>),
    Expr(Expr<'a>),
    Block(Vec<StatementKind<'a>>),
    ForIn {
        condition: ForCondition<'a>,
        body: Box<StatementKind<'a>>,
    },
    While {
        condition: Expr<'a>,
        body: Box<StatementKind<'a>>,
    },
    If {
        condition: Expr<'a>,
        yes: Box<StatementKind<'a>>,
        no: Option<Box<StatementKind<'a>>>,
    },
    Break,
    Continue,
    Try(TryCatch<'a>),
    WaitForFrame {
        frame: Expr<'a>,
        scene: Option<Expr<'a>>,
        if_loaded: Box<StatementKind<'a>>,
    },
    TellTarget {
        target: Expr<'a>,
        body: Box<StatementKind<'a>>,
    },
    InlinePCode(&'a str),
    With {
        target: Expr<'a>,
        body: Box<StatementKind<'a>>,
    },
    Switch {
        target: Expr<'a>,
        elements: Vec<SwitchElement<'a>>,
    },
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum SwitchElement<'a> {
    Case(Expr<'a>),
    Default,
    Statement(StatementKind<'a>),
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum ForCondition<'a> {
    Enumerate {
        variable: &'a str,
        declare: bool,
        object: Expr<'a>,
    },
    Classic {
        initialize: Option<Box<StatementKind<'a>>>,
        // This is technically incorrect, we treat `a++, b++` as two different expressions, but they should be one (and we'd have a `Next(a, b)` expression)
        // Unfortunately that seems difficult to implement _correctly_, so this is a good stopgap (did anyone even use `,` in regular code, outside for loops?)
        condition: Vec<Expr<'a>>,
        update: Vec<Expr<'a>>,
    },
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Function<'a> {
    pub name: Option<&'a str>,
    pub args: Vec<FunctionArgument<'a>>,
    pub body: Vec<StatementKind<'a>>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct FunctionArgument<'a> {
    pub name: &'a str,
    pub type_name: Option<Spanned<&'a str>>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Declaration<'a> {
    pub name: &'a str,
    pub value: Option<Expr<'a>>,
    pub type_name: Option<Spanned<&'a str>>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct TryCatch<'a> {
    pub try_body: Vec<StatementKind<'a>>,
    pub typed_catches: Vec<(Spanned<&'a str>, Catch<'a>)>,
    pub catch_all: Option<Catch<'a>>,
    pub finally: Vec<StatementKind<'a>>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Catch<'a> {
    pub name: Spanned<&'a str>,
    pub body: Vec<StatementKind<'a>>,
}

#[derive(Debug, Serialize)]
pub struct Document<'src> {
    pub statements: Vec<StatementKind<'src>>,
}

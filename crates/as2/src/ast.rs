use serde::Serialize;
use std::borrow::Cow;

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum ExprKind<'a> {
    Constant(ConstantKind<'a>),
    Call {
        name: Box<ExprKind<'a>>,
        args: Vec<ExprKind<'a>>,
    },
    New {
        name: Box<ExprKind<'a>>,
        args: Vec<ExprKind<'a>>,
    },
    BinaryOperator(BinaryOperator, Box<ExprKind<'a>>, Box<ExprKind<'a>>),
    UnaryOperator(UnaryOperator, Box<ExprKind<'a>>),
    Parenthesis(Box<ExprKind<'a>>),
    Ternary {
        condition: Box<ExprKind<'a>>,
        yes: Box<ExprKind<'a>>,
        no: Box<ExprKind<'a>>,
    },
    InitObject(Vec<(&'a str, ExprKind<'a>)>),
    InitArray(Vec<ExprKind<'a>>),
    Field(Box<ExprKind<'a>>, Box<ExprKind<'a>>),
    TypeOf(Vec<ExprKind<'a>>),
    Delete(Vec<ExprKind<'a>>),
    Void(Vec<ExprKind<'a>>),
    Function(Function<'a>),
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum ConstantKind<'a> {
    String(Cow<'a, str>),
    Identifier(&'a str),
    Float(f64),
    Integer(i32),
}

#[derive(Debug, Clone, Copy, Serialize, PartialEq)]
pub enum BinaryOperator {
    Add,
    Assign,
    AddAssign,
    Sub,
    SubAssign,
    Divide,
    DivideAssign,
    Multiply,
    MultiplyAssign,
    Modulo,
    ModuloAssign,
    BitAnd,
    BitAndAssign,
    BitOr,
    BitOrAssign,
    BitXor,
    BitXorAssign,
    BitShiftLeft,
    BitShiftLeftAssign,
    BitShiftRight,
    BitShiftRightAssign,
    BitShiftRightUnsigned,
    BitShiftRightUnsignedAssign,
    Equal,
    StrictEqual,
    NotEqual,
    StrictNotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    LogicalAnd,
    LogicalOr,
    InstanceOf,
}

#[derive(Debug, Clone, Copy, Serialize, PartialEq)]
pub enum UnaryOperator {
    Sub,
    BitNot,
    Increment(Affix),
    Decrement(Affix),
    LogicalNot,
}

#[derive(Debug, Clone, Copy, Serialize, PartialEq)]
pub enum Affix {
    Postfix,
    Prefix,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum StatementKind<'a> {
    Declare(Vec<Declaration<'a>>),
    Return(Vec<ExprKind<'a>>),
    Expr(ExprKind<'a>),
    Block(Vec<StatementKind<'a>>),
    ForIn {
        condition: ForCondition<'a>,
        body: Box<StatementKind<'a>>,
    },
    If {
        condition: ExprKind<'a>,
        yes: Box<StatementKind<'a>>,
        no: Option<Box<StatementKind<'a>>>,
    },
    Break,
    Continue,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum ForCondition<'a> {
    Enumerate {
        variable: &'a str,
        declare: bool,
        object: ExprKind<'a>,
    },
    Classic {
        initialize: Option<Box<StatementKind<'a>>>,
        // This is technically incorrect, we treat `a++, b++` as two different expressions, but they should be one (and we'd have a `Next(a, b)` expression)
        // Unfortunately that seems difficult to implement _correctly_, so this is a good stopgap (did anyone even use `,` in regular code, outside for loops?)
        condition: Vec<ExprKind<'a>>,
        update: Vec<ExprKind<'a>>,
    },
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Function<'a> {
    pub name: Option<&'a str>,
    pub args: Vec<&'a str>,
    pub body: Vec<StatementKind<'a>>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Declaration<'a> {
    pub name: &'a str,
    pub value: Option<ExprKind<'a>>,
}

#[derive(Debug, Serialize)]
pub struct Document<'src> {
    pub statements: Vec<StatementKind<'src>>,
}

use serde::Serialize;
use std::borrow::Cow;

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum Expr<'a> {
    Constant(Constant<'a>),
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
    Parenthesis(Box<Expr<'a>>),
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
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum Constant<'a> {
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
pub enum Statement<'a> {
    Declare(Vec<Declaration<'a>>),
    Return(Vec<Expr<'a>>),
    Expr(Expr<'a>),
    Block(Vec<Statement<'a>>),
    ForIn {
        condition: ForCondition<'a>,
        body: Box<Statement<'a>>,
    },
    If {
        condition: Expr<'a>,
        yes: Box<Statement<'a>>,
        no: Option<Box<Statement<'a>>>,
    },
    Break,
    Continue,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum ForCondition<'a> {
    Enumerate {
        variable: String,
        declare: bool,
        object: Expr<'a>,
    },
    Classic {
        initialize: Option<Box<Statement<'a>>>,
        // This is technically incorrect, we treat `a++, b++` as two different expressions, but they should be one (and we'd have a `Next(a, b)` expression)
        // Unfortunately that seems difficult to implement _correctly_, so this is a good stopgap (did anyone even use `,` in regular code, outside for loops?)
        condition: Vec<Expr<'a>>,
        update: Vec<Expr<'a>>,
    },
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Function<'a> {
    pub name: Option<String>,
    pub args: Vec<String>,
    pub body: Vec<Statement<'a>>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Declaration<'a> {
    pub name: String,
    pub value: Option<Expr<'a>>,
}

#[derive(Debug, Serialize)]
pub struct Document<'src> {
    pub statements: Vec<Statement<'src>>,
}

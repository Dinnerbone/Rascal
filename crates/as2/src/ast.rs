use crate::lexer::tokens::Keyword;
use rascal_common::span::Spanned;
use serde::Serialize;
use std::borrow::Cow;

pub type Expr<'a> = Spanned<ExprKind<'a>>;
pub type Constant<'a> = Spanned<ConstantKind<'a>>;
pub type Statement<'a> = Spanned<StatementKind<'a>>;

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
    GetVariable(Box<Expr<'a>>),
    SetVariable(Box<Expr<'a>>, Box<Expr<'a>>),
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
    StringEqual,
    StringGreaterThan,
    StringGreaterThanEqual,
    StringLessThan,
    StringLessThanEqual,
    StringNotEqual,
    BooleanAnd,
    BooleanOr,
    StringAdd,
}

impl BinaryOperator {
    pub(crate) fn for_keyword(keyword: Keyword) -> Option<BinaryOperator> {
        match keyword {
            Keyword::InstanceOf => Some(BinaryOperator::InstanceOf),
            Keyword::Eq => Some(BinaryOperator::StringEqual),
            Keyword::Gt => Some(BinaryOperator::StringGreaterThan),
            Keyword::Ge => Some(BinaryOperator::StringGreaterThanEqual),
            Keyword::Lt => Some(BinaryOperator::StringLessThan),
            Keyword::Le => Some(BinaryOperator::StringLessThanEqual),
            Keyword::Ne => Some(BinaryOperator::StringNotEqual),
            Keyword::And => Some(BinaryOperator::BooleanAnd),
            Keyword::Or => Some(BinaryOperator::BooleanOr),
            Keyword::Add => Some(BinaryOperator::StringAdd),
            _ => None,
        }
    }
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
    pub args: Vec<&'a str>,
    pub body: Vec<StatementKind<'a>>,
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

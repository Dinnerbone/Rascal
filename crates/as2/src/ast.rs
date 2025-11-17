use serde::Serialize;

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum Expr {
    Constant(Constant),
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
    Parenthesis(Box<Expr>),
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
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum Constant {
    String(String),
    Identifier(String),
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
pub enum Statement {
    Declare(Vec<Declaration>),
    Return(Vec<Expr>),
    Expr(Expr),
    Block(Vec<Statement>),
    ForIn {
        condition: ForCondition,
        body: Box<Statement>,
    },
    If {
        condition: Expr,
        yes: Box<Statement>,
        no: Option<Box<Statement>>,
    },
    Break,
    Continue,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum ForCondition {
    Enumerate {
        variable: String,
        declare: bool,
        object: Expr,
    },
    Classic {
        initialize: Option<Box<Statement>>,
        // This is technically incorrect, we treat `a++, b++` as two different expressions, but they should be one (and we'd have a `Next(a, b)` expression)
        // Unfortunately that seems difficult to implement _correctly_, so this is a good stopgap (did anyone even use `,` in regular code, outside for loops?)
        condition: Vec<Expr>,
        update: Vec<Expr>,
    },
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Function {
    pub name: Option<String>,
    pub args: Vec<String>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Declaration {
    pub name: String,
    pub value: Option<Expr>,
}

#[derive(Debug, Serialize)]
pub struct Document {
    pub statements: Vec<Statement>,
}

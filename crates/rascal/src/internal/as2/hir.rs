pub(crate) mod constant_folder;
pub(crate) mod register_promoter;
pub(crate) mod scope;
pub(crate) mod visitor;

use crate::internal::span::Spanned;
use indexmap::IndexMap;
use serde::Serialize;

pub type Expr = Spanned<ExprKind>;
pub use crate::internal::as2::ast::{Affix, BinaryOperator, UnaryOperator};
use crate::internal::as2::hir::scope::Scope;

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
    DuplicateMovieClip {
        source: Box<Expr>,
        target: Box<Expr>,
        depth: Box<Expr>,
    },
    AsciiToChar(Box<Expr>),
    MBAsciiToChar(Box<Expr>),
    CallFrame(Box<Expr>),
    GetTime,
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
    TypeOf(Box<Expr>),
    Delete(Box<Expr>),
    Void(Box<Expr>),
    Function(Box<Function>),
    GetVariable(Box<Expr>),
    SetVariable(Box<Expr>, Box<Expr>),
    GotoFrame(Box<Expr>, bool),
    GetUrl {
        target: Box<Expr>,
        url: Box<Expr>,
        load_variables: bool,
        load_target: bool,
        method: GetUrlMethod,
    },
    CastToInteger(Box<Expr>),
    CastToNumber(Box<Expr>),
    CastToString(Box<Expr>),
    CastToObject {
        class: Box<Expr>,
        object: Box<Expr>,
    },
    StringLength(Box<Expr>),
    MBStringLength(Box<Expr>),
    CharToAscii(Box<Expr>),
    MBCharToAscii(Box<Expr>),
    Substring {
        string: Box<Expr>,
        start: Box<Expr>,
        length: Box<Expr>,
    },
    MBSubstring {
        string: Box<Expr>,
        start: Box<Expr>,
        length: Box<Expr>,
    },
    NextFrame,
    PreviousFrame,
    Play,
    Stop,
    StopSounds,
    StartDrag {
        target: Box<Expr>,
        lock: bool,
        #[expect(clippy::type_complexity)]
        constraints: Option<(Box<Expr>, Box<Expr>, Box<Expr>, Box<Expr>)>,
    },
    EndDrag,
    GetTargetPath(Box<Expr>),
    Trace(Box<Expr>),
    RemoveSprite(Box<Expr>),
    GetRandomNumber(Box<Expr>),
    GetProperty(Box<Expr>, i32),
    SetProperty(Box<Expr>, i32, Box<Expr>),
    ToggleQuality,
}

#[derive(Debug, Copy, Clone, Serialize, PartialEq)]
pub enum GetUrlMethod {
    None,
    Get,
    Post,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum ConstantKind {
    String(String),
    Identifier(String),
    Float(f64),
    Integer(i32),
    Boolean(bool),
    Register(u8),
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum StatementKind {
    Declare(Box<Declaration>),
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
pub enum EnumeratorTarget {
    Variable { name: String, declare: bool },
    Register(u8),
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub enum ForCondition {
    Enumerate {
        target: EnumeratorTarget,
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
pub struct Method {
    pub function: Function,
    pub is_static: bool,
}

#[derive(Debug, Clone, Serialize, PartialEq, Default)]
pub struct Function {
    pub signature: FunctionSignature,
    pub body: Vec<StatementKind>,
    pub scope: Scope,
    pub register_count: u8,
    pub preload_this: bool,
    pub suppress_this: bool,
    pub preload_arguments: bool,
    pub suppress_arguments: bool,
    pub preload_super: bool,
    pub suppress_super: bool,
    pub preload_root: bool,
    pub preload_parent: bool,
    pub preload_global: bool,
}

#[derive(Debug, Clone, Serialize, PartialEq, Default)]
pub struct FunctionSignature {
    pub name: Option<Spanned<String>>,
    pub args: Vec<FunctionArgument>,
    pub return_type: Option<Spanned<String>>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct FunctionArgument {
    pub name: String,
    pub type_name: Option<Spanned<String>>,
    pub register: Option<u8>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Declaration {
    pub name: Spanned<String>,
    pub value: Option<Expr>,
    pub type_name: Option<Spanned<String>>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Field {
    pub value: Option<Expr>,
    pub type_name: Option<Spanned<String>>,
    pub is_static: bool,
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

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Interface {
    pub name: String,
    pub extends: Option<String>,
    pub functions: IndexMap<String, FunctionSignature>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Class {
    pub name: String,
    pub extends: Option<String>,
    pub implements: Vec<String>,
    pub functions: IndexMap<String, Method>,
    pub virtual_properties: IndexMap<String, VirtualProperty>,
    pub fields: IndexMap<String, Field>,
    pub constructor: Function,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct VirtualProperty {
    pub name: String,
    pub getter: Option<String>,
    pub setter: Option<String>,
    pub is_static: bool,
}

#[derive(Debug, Serialize)]
pub enum Document {
    Script {
        statements: Vec<StatementKind>,
        scope: Scope,
    },
    Interface(Interface),
    Class(Box<Class>),
    Invalid,
}

use indexmap::IndexMap;
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
    TypeOf(Vec<Expr>),
    Delete(Vec<Expr>),
    Void(Vec<Expr>),
    Function(Function),
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

impl ExprKind {
    pub(crate) fn simplify(&mut self, anything_changed: &mut bool) {
        match self {
            ExprKind::Constant(_) => {}
            ExprKind::Call { name, args } => {
                name.value.simplify(anything_changed);
                for arg in args {
                    arg.value.simplify(anything_changed);
                }
            }
            ExprKind::New { name, args } => {
                name.value.simplify(anything_changed);
                for arg in args {
                    arg.value.simplify(anything_changed);
                }
            }
            ExprKind::DuplicateMovieClip {
                target,
                depth,
                source,
            } => {
                target.value.simplify(anything_changed);
                depth.value.simplify(anything_changed);
                source.value.simplify(anything_changed);
            }
            ExprKind::AsciiToChar(expr) => {
                expr.value.simplify(anything_changed);
            }
            ExprKind::MBAsciiToChar(expr) => {
                expr.value.simplify(anything_changed);
            }
            ExprKind::CallFrame(expr) => {
                expr.value.simplify(anything_changed);
            }
            ExprKind::GetTime => {}
            ExprKind::BinaryOperator(op, left, right) => {
                left.value.simplify(anything_changed);
                right.value.simplify(anything_changed);
                if let ExprKind::Constant(left) = &left.value
                    && let ExprKind::Constant(right) = &right.value
                {
                    #[expect(clippy::single_match)]
                    match (op, left, right) {
                        (
                            BinaryOperator::StringAdd,
                            ConstantKind::String(left),
                            ConstantKind::String(right),
                        ) => {
                            *self = ExprKind::Constant(ConstantKind::String(format!(
                                "{}{}",
                                left, right
                            )));
                            *anything_changed = true;
                        }
                        _ => {}
                    }
                }
            }
            ExprKind::UnaryOperator(op, expr) => {
                expr.value.simplify(anything_changed);
                if let ExprKind::Constant(value) = &expr.value {
                    #[expect(clippy::single_match)]
                    match (op, value) {
                        (UnaryOperator::LogicalNot, ConstantKind::Boolean(value)) => {
                            *self = ExprKind::Constant(ConstantKind::Boolean(!value));
                            *anything_changed = true;
                        }
                        _ => {}
                    }
                }
            }
            ExprKind::Ternary { condition, yes, no } => {
                condition.value.simplify(anything_changed);
                yes.value.simplify(anything_changed);
                no.value.simplify(anything_changed);
            }
            ExprKind::InitObject(values) => {
                for (_key, value) in values {
                    value.value.simplify(anything_changed);
                }
            }
            ExprKind::InitArray(values) => {
                for value in values {
                    value.value.simplify(anything_changed);
                }
            }
            ExprKind::Field(obj, key) => {
                obj.value.simplify(anything_changed);
                key.value.simplify(anything_changed);
            }
            ExprKind::TypeOf(values) => {
                for value in values {
                    value.value.simplify(anything_changed);
                }
            }
            ExprKind::Delete(values) => {
                for value in values {
                    value.value.simplify(anything_changed);
                }
            }
            ExprKind::Void(values) => {
                for value in values {
                    value.value.simplify(anything_changed);
                }
            }
            ExprKind::Function(function) => {
                for statement in &mut function.body {
                    statement.simplify(anything_changed);
                }
            }
            ExprKind::GetVariable(key) => {
                key.value.simplify(anything_changed);
            }
            ExprKind::SetVariable(key, value) => {
                key.value.simplify(anything_changed);
                value.value.simplify(anything_changed);
            }
            ExprKind::GotoFrame(frame, _scene_change) => {
                frame.value.simplify(anything_changed);
            }
            ExprKind::GetUrl { url, target, .. } => {
                url.value.simplify(anything_changed);
                target.value.simplify(anything_changed);
            }
            ExprKind::CastToInteger(expr) => {
                expr.value.simplify(anything_changed);
            }
            ExprKind::CastToNumber(expr) => {
                expr.value.simplify(anything_changed);
            }
            ExprKind::CastToString(expr) => {
                expr.value.simplify(anything_changed);
            }
            ExprKind::StringLength(expr) => {
                expr.value.simplify(anything_changed);
            }
            ExprKind::MBStringLength(expr) => {
                expr.value.simplify(anything_changed);
            }
            ExprKind::CharToAscii(expr) => {
                expr.value.simplify(anything_changed);
            }
            ExprKind::MBCharToAscii(expr) => {
                expr.value.simplify(anything_changed);
            }
            ExprKind::Substring {
                start,
                length,
                string,
            } => {
                string.value.simplify(anything_changed);
                start.value.simplify(anything_changed);
                length.value.simplify(anything_changed);
            }
            ExprKind::MBSubstring {
                start,
                length,
                string,
            } => {
                string.value.simplify(anything_changed);
                start.value.simplify(anything_changed);
                length.value.simplify(anything_changed);
            }
            ExprKind::NextFrame => {}
            ExprKind::PreviousFrame => {}
            ExprKind::Play => {}
            ExprKind::Stop => {}
            ExprKind::StopSounds => {}
            ExprKind::StartDrag {
                target,
                constraints,
                ..
            } => {
                target.value.simplify(anything_changed);
                if let Some((a, b, c, d)) = constraints {
                    a.value.simplify(anything_changed);
                    b.value.simplify(anything_changed);
                    c.value.simplify(anything_changed);
                    d.value.simplify(anything_changed);
                }
            }
            ExprKind::EndDrag => {}
            ExprKind::GetTargetPath(expr) => {
                expr.value.simplify(anything_changed);
            }
            ExprKind::Trace(expr) => {
                expr.value.simplify(anything_changed);
            }
            ExprKind::RemoveSprite(expr) => {
                expr.value.simplify(anything_changed);
            }
            ExprKind::GetRandomNumber(expr) => {
                expr.value.simplify(anything_changed);
            }
            ExprKind::GetProperty(obj, _property) => {
                obj.value.simplify(anything_changed);
            }
            ExprKind::SetProperty(obj, _property, value) => {
                obj.value.simplify(anything_changed);
                value.value.simplify(anything_changed);
            }
            ExprKind::ToggleQuality => {}
        }
    }
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
    This,
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

impl StatementKind {
    pub(crate) fn simplify(&mut self, anything_changed: &mut bool) {
        match self {
            StatementKind::Declare(declarations) => {
                for declaration in declarations {
                    if let Some(value) = &mut declaration.value {
                        value.value.simplify(anything_changed);
                    }
                }
            }
            StatementKind::Return(values) => {
                for value in values {
                    value.value.simplify(anything_changed);
                }
            }
            StatementKind::Throw(values) => {
                for value in values {
                    value.value.simplify(anything_changed);
                }
            }
            StatementKind::Expr(expr) => expr.value.simplify(anything_changed),
            StatementKind::Block(statements) => {
                for statement in statements {
                    statement.simplify(anything_changed);
                }
            }
            StatementKind::ForIn { condition, body } => {
                match condition {
                    ForCondition::Enumerate { object, .. } => {
                        object.value.simplify(anything_changed);
                    }
                    ForCondition::Classic {
                        initialize,
                        condition,
                        update,
                    } => {
                        if let Some(initialize) = initialize.as_mut() {
                            initialize.simplify(anything_changed);
                        }
                        for value in condition {
                            value.value.simplify(anything_changed);
                        }
                        for value in update {
                            value.value.simplify(anything_changed);
                        }
                    }
                }
                body.simplify(anything_changed);
            }
            StatementKind::While { condition, body } => {
                condition.value.simplify(anything_changed);
                body.simplify(anything_changed);
            }
            StatementKind::If { condition, yes, no } => {
                condition.value.simplify(anything_changed);
                yes.simplify(anything_changed);
                if let Some(no) = no.as_mut() {
                    no.simplify(anything_changed);
                }
            }
            StatementKind::Break => {}
            StatementKind::Continue => {}
            StatementKind::Try(try_catch) => {
                try_catch.simplify(anything_changed);
            }
            StatementKind::WaitForFrame {
                frame,
                scene,
                if_loaded,
            } => {
                frame.value.simplify(anything_changed);
                if let Some(scene) = scene {
                    scene.value.simplify(anything_changed);
                }
                if_loaded.simplify(anything_changed);
            }
            StatementKind::TellTarget { target, body } => {
                target.value.simplify(anything_changed);
                body.simplify(anything_changed);
            }
            StatementKind::InlinePCode(_) => {}
            StatementKind::With { target, body } => {
                target.value.simplify(anything_changed);
                body.simplify(anything_changed);
            }
            StatementKind::Switch { target, elements } => {
                target.value.simplify(anything_changed);
                for element in elements {
                    match element {
                        SwitchElement::Case(expr) => {
                            expr.value.simplify(anything_changed);
                        }
                        SwitchElement::Default => {}
                        SwitchElement::Statement(statement) => {
                            statement.simplify(anything_changed);
                        }
                    }
                }
            }
        }
    }
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
    pub signature: FunctionSignature,
    pub body: Vec<StatementKind>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct FunctionSignature {
    pub name: Option<Spanned<String>>,
    pub args: Vec<FunctionArgument>,
    pub return_type: Option<Spanned<String>>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct FunctionArgument {
    pub name: String,
    pub type_name: Option<Spanned<String>>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Declaration {
    pub name: Spanned<String>,
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

impl TryCatch {
    pub(crate) fn simplify(&mut self, anything_changed: &mut bool) {
        for statement in &mut self.try_body {
            statement.simplify(anything_changed);
        }
        for (_, catch) in &mut self.typed_catches {
            catch.simplify(anything_changed);
        }
        if let Some(catch_all) = &mut self.catch_all {
            catch_all.simplify(anything_changed);
        }
        for statement in &mut self.finally {
            statement.simplify(anything_changed);
        }
    }
}

#[derive(Debug, Clone, Serialize, PartialEq)]
pub struct Catch {
    pub name: Spanned<String>,
    pub body: Vec<StatementKind>,
}

impl Catch {
    pub(crate) fn simplify(&mut self, anything_changed: &mut bool) {
        for statement in &mut self.body {
            statement.simplify(anything_changed);
        }
    }
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
    pub functions: IndexMap<String, Function>,
    pub variables: IndexMap<String, Declaration>,
    pub constructor: Function,
}

#[derive(Debug, Serialize)]
pub enum Document {
    Script(Vec<StatementKind>),
    Interface(Interface),
    Class(Box<Class>),
    Invalid,
}

impl Document {
    pub(crate) fn simplify(&mut self) -> bool {
        let mut anything_changed = false;

        match self {
            Document::Script(statements) => {
                for statement in statements {
                    statement.simplify(&mut anything_changed);
                }
            }
            Document::Interface(_interface) => {}
            Document::Class(_class) => {}
            Document::Invalid => {}
        }

        anything_changed
    }
}

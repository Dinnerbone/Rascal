use indexmap::IndexMap;
use serde::Serialize;

#[derive(Debug, PartialEq, Serialize, Clone)]
pub struct Actions {
    pub(crate) actions: Vec<Action>,
    pub(crate) label_positions: IndexMap<String, usize>,
}

impl Actions {
    pub fn empty() -> Self {
        Self {
            actions: vec![],
            label_positions: IndexMap::new(),
        }
    }

    pub fn push(&mut self, action: Action) {
        self.actions.push(action);
    }

    pub fn push_label(&mut self, label: String) {
        self.label_positions.insert(label, self.actions.len());
    }

    pub fn actions(&self) -> &[Action] {
        &self.actions
    }

    pub fn label_positions(&self) -> &IndexMap<String, usize> {
        &self.label_positions
    }

    pub fn replace_action(&mut self, index: usize, action: Action) {
        self.actions[index] = action;
    }

    pub fn last_mut(&mut self) -> Option<&mut Action> {
        self.actions.last_mut()
    }

    pub fn has_dangling_label(&self) -> bool {
        let current_pos = self.actions.len();
        self.label_positions.values().any(|p| *p == current_pos)
    }

    pub fn append(&mut self, mut other: Actions) {
        let offset = self.actions.len();
        self.actions.append(&mut other.actions);
        for (label, pos) in other.label_positions {
            self.label_positions.insert(label, pos + offset);
        }
    }
}

impl std::fmt::Display for Actions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut labels_per_pos: IndexMap<usize, Vec<String>> = IndexMap::new();
        for (label, &pos) in self.label_positions.iter() {
            labels_per_pos.entry(pos).or_default().push(label.clone());
        }
        for (i, action) in self.actions.iter().enumerate() {
            if let Some(labels) = labels_per_pos.get(&i) {
                for label in labels {
                    write!(f, "{}:", label)?;
                }
            }
            writeln!(f, "{}", action)?;
        }
        if let Some(labels) = labels_per_pos.get(&self.actions.len()) {
            for label in labels {
                write!(f, "{}:", label)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Serialize, Clone)]
pub enum Action {
    Add,
    Add2,
    And,
    AsciiToChar,
    BitAnd,
    BitLShift,
    BitOr,
    BitRShift,
    BitURShift,
    BitXor,
    Call,
    CallFunction,
    CallMethod,
    CastOp,
    CharToAscii,
    CloneSprite,
    ConstantPool(Vec<String>),
    Decrement,
    DefineFunction {
        name: String,
        params: Vec<String>,
        actions: Actions,
    },
    DefineLocal,
    DefineLocal2,
    Delete,
    Delete2,
    Divide,
    End,
    EndDrag,
    Enumerate,
    Enumerate2,
    Equals,
    Equals2,
    Extends,
    GetMember,
    GetProperty,
    GetTime,
    GetUrl {
        url: String,
        target: String,
    },
    GetUrl2 {
        load_variables: bool,
        load_target: bool,
        method: u8,
    },
    GetVariable,
    GotoFrame(u16),
    GotoFrame2 {
        scene_bias: u16,
        play: bool,
    },
    GotoLabel(String),
    Greater,
    If(String),
    ImplementsOp,
    Increment,
    InitArray,
    InitObject,
    InstanceOf,
    Jump(String),
    Less,
    Less2,
    MBAsciiToChar,
    MBCharToAscii,
    MBStringExtract,
    MBStringLength,
    Modulo,
    Multiply,
    NewMethod,
    NewObject,
    NextFrame,
    Not,
    Or,
    Play,
    Pop,
    PrevFrame,
    Push(Vec<PushValue>),
    PushDuplicate,
    RandomNumber,
    RemoveSprite,
    Return,
    SetMember,
    SetProperty,
    SetTarget(String),
    SetTarget2,
    SetVariable,
    StackSwap,
    StartDrag,
    Stop,
    StopSounds,
    StoreRegister(u8),
    StrictEquals,
    StringAdd,
    StringEquals,
    StringExtract,
    StringGreater,
    StringLength,
    StringLess,
    Subtract,
    TargetPath,
    Throw,
    ToInteger,
    ToNumber,
    ToString,
    ToggleQuality,
    Trace,
    Try {
        try_body: Actions,
        catch_body: Option<(CatchTarget, Actions)>,
        finally_body: Option<Actions>,
    },
    TypeOf,
    WaitForFrame {
        frame: u16,
        skip_count: u8,
    },
    WaitForFrame2 {
        skip_count: u8,
    },
}

#[derive(Debug, PartialEq, Serialize, Clone)]
pub enum CatchTarget {
    Variable(String),
    Register(u8),
}

impl Action {
    pub fn stack_delta(&self) -> i32 {
        match self {
            Action::Add => -1,
            Action::Add2 => -1,
            Action::And => -1,
            Action::AsciiToChar => 0,
            Action::BitAnd => -1,
            Action::BitLShift => -1,
            Action::BitOr => -1,
            Action::BitRShift => -1,
            Action::BitURShift => -1,
            Action::BitXor => -1,
            Action::Call => -1,
            Action::CastOp => -1,
            Action::CharToAscii => 0,
            Action::CloneSprite => -3,
            Action::ConstantPool(_) => 0,
            Action::Decrement => -1,
            Action::DefineFunction { .. } => 1,
            Action::DefineLocal => -2,
            Action::DefineLocal2 => -1,
            Action::Delete => -2,
            Action::Delete2 => -1,
            Action::Divide => -1,
            Action::End => 0,
            Action::EndDrag => 0,
            Action::Enumerate => 0,
            Action::Enumerate2 => 0,
            Action::Equals => -1,
            Action::Equals2 => -1,
            Action::Extends => -2,
            Action::GetMember => -1,
            Action::GetProperty => -1,
            Action::GetTime => 1,
            Action::GetUrl { .. } => 0,
            Action::GetUrl2 { .. } => -2,
            Action::GetVariable => 0,
            Action::GotoFrame(_) => 0,
            Action::GotoFrame2 { .. } => -1,
            Action::GotoLabel(_) => 0,
            Action::Greater => -1,
            Action::If(_) => -1,
            Action::Increment => -1,
            Action::InstanceOf => -1,
            Action::Jump(_) => 0,
            Action::Less => -1,
            Action::Less2 => -1,
            Action::MBAsciiToChar => 0,
            Action::MBCharToAscii => 0,
            Action::MBStringExtract => -2,
            Action::MBStringLength => 0,
            Action::Modulo => -1,
            Action::Multiply => -1,
            Action::NewObject => 1,
            Action::NextFrame => 0,
            Action::Not => 0,
            Action::Or => -1,
            Action::Play => 0,
            Action::PrevFrame => 0,
            Action::Pop => -1,
            Action::Push(values) => values.len() as i32,
            Action::PushDuplicate => 1,
            Action::RandomNumber => 0,
            Action::RemoveSprite => -1,
            Action::Return => -1,
            Action::SetMember => -3,
            Action::SetProperty => -3,
            Action::SetTarget(_) => 0,
            Action::SetTarget2 => -1,
            Action::SetVariable => -2,
            Action::StackSwap => 0,
            Action::Stop => 0,
            Action::StopSounds => 0,
            Action::StrictEquals => -1,
            Action::StringAdd => -1,
            Action::StringEquals => -1,
            Action::StringExtract => -2,
            Action::StringGreater => -1,
            Action::StringLength => 0,
            Action::StringLess => -1,
            Action::StoreRegister(_) => 0,
            Action::Subtract => -1,
            Action::TargetPath => 0,
            Action::Throw => -1,
            Action::ToInteger => 0,
            Action::ToNumber => 0,
            Action::ToString => 0,
            Action::ToggleQuality => 0,
            Action::Trace => -1,
            Action::Try { .. } => 0,
            Action::TypeOf => 0,
            Action::WaitForFrame { .. } => 0,
            Action::WaitForFrame2 { .. } => -1,

            Action::CallFunction => panic!("CallFunction must have explicit stack delta provided!"),
            Action::CallMethod => panic!("CallMethod must have explicit stack delta provided!"),
            Action::ImplementsOp => panic!("ImplementsOp must have explicit stack delta provided!"),
            Action::InitArray => panic!("InitArray must have explicit stack delta provided!"),
            Action::InitObject => panic!("InitObject must have explicit stack delta provided!"),
            Action::NewMethod => panic!("NewMethod must have explicit stack delta provided!"),
            Action::StartDrag => panic!("StartDrag must have explicit stack delta provided!"),
        }
    }
}

impl std::fmt::Display for Action {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Action::Add => write!(f, "Add"),
            Action::Add2 => write!(f, "Add2"),
            Action::And => write!(f, "And"),
            Action::AsciiToChar => write!(f, "AsciiToChar"),
            Action::BitAnd => write!(f, "BitAnd"),
            Action::BitLShift => write!(f, "BitLShift"),
            Action::BitOr => write!(f, "BitOr"),
            Action::BitRShift => write!(f, "BitRShift"),
            Action::BitURShift => write!(f, "BitURShift"),
            Action::BitXor => write!(f, "BitXor"),
            Action::Call => write!(f, "Call"),
            Action::CallFunction => write!(f, "CallFunction"),
            Action::CallMethod => write!(f, "CallMethod"),
            Action::CastOp => write!(f, "CastOp"),
            Action::CharToAscii => write!(f, "CharToAscii"),
            Action::CloneSprite => write!(f, "CloneSprite"),
            Action::ConstantPool(values) => {
                write!(f, "ConstantPool ")?;
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "\"{}\"", value)?;
                }
                Ok(())
            }
            Action::Decrement => write!(f, "Decrement"),
            Action::DefineFunction {
                name,
                params,
                actions,
            } => {
                write!(f, "DefineFunction \"{}\", {}", name, params.len())?;
                for param in params {
                    write!(f, ", \"{}\"", param)?;
                }
                writeln!(f, " {{")?;
                write!(f, "{}", actions)?;
                write!(f, "}}")
            }
            Action::DefineLocal => write!(f, "DefineLocal"),
            Action::DefineLocal2 => write!(f, "DefineLocal2"),
            Action::Delete => write!(f, "Delete"),
            Action::Delete2 => write!(f, "Delete2"),
            Action::Divide => write!(f, "Divide"),
            Action::End => write!(f, "End"),
            Action::EndDrag => write!(f, "EndDrag"),
            Action::Enumerate => write!(f, "Enumerate"),
            Action::Enumerate2 => write!(f, "Enumerate2"),
            Action::Equals => write!(f, "Equals"),
            Action::Equals2 => write!(f, "Equals2"),
            Action::Extends => write!(f, "Extends"),
            Action::GetMember => write!(f, "GetMember"),
            Action::GetProperty => write!(f, "GetProperty"),
            Action::GetTime => write!(f, "GetTime"),
            Action::GetUrl { url, target } => write!(f, "GetUrl \"{}\", \"{}\"", url, target),
            Action::GetUrl2 {
                load_variables,
                load_target,
                method,
            } => write!(f, "GetUrl2 {}, {}, {}", load_variables, load_target, method),
            Action::GetVariable => write!(f, "GetVariable"),
            Action::GotoFrame(frame) => write!(f, "GotoFrame {}", frame),
            Action::GotoFrame2 { scene_bias, play } => {
                if *scene_bias > 0 {
                    write!(f, "GotoFrame2 true, {}, {}", play, scene_bias)
                } else {
                    write!(f, "GotoFrame2 false, {}", play)
                }
            }
            Action::GotoLabel(label) => write!(f, "GotoLabel \"{}\"", label),
            Action::Greater => write!(f, "Greater"),
            Action::If(label) => write!(f, "If {}", label),
            Action::ImplementsOp => write!(f, "ImplementsOp"),
            Action::Increment => write!(f, "Increment"),
            Action::InitArray => write!(f, "InitArray"),
            Action::InitObject => write!(f, "InitObject"),
            Action::InstanceOf => write!(f, "InstanceOf"),
            Action::Jump(label) => write!(f, "Jump {}", label),
            Action::Less => write!(f, "Less"),
            Action::Less2 => write!(f, "Less2"),
            Action::MBAsciiToChar => write!(f, "MBAsciiToChar"),
            Action::MBCharToAscii => write!(f, "MBCharToAscii"),
            Action::MBStringExtract => write!(f, "MBStringExtract"),
            Action::MBStringLength => write!(f, "MBStringLength"),
            Action::Modulo => write!(f, "Modulo"),
            Action::Multiply => write!(f, "Multiply"),
            Action::NewMethod => write!(f, "NewMethod"),
            Action::NewObject => write!(f, "NewObject"),
            Action::NextFrame => write!(f, "NextFrame"),
            Action::Not => write!(f, "Not"),
            Action::Or => write!(f, "Or"),
            Action::Play => write!(f, "Play"),
            Action::Pop => write!(f, "Pop"),
            Action::PrevFrame => write!(f, "PrevFrame"),
            Action::Push(values) => {
                write!(f, "Push ")?;
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value)?;
                }
                Ok(())
            }
            Action::PushDuplicate => write!(f, "PushDuplicate"),
            Action::RandomNumber => write!(f, "RandomNumber"),
            Action::RemoveSprite => write!(f, "RemoveSprite"),
            Action::Return => write!(f, "Return"),
            Action::SetMember => write!(f, "SetMember"),
            Action::SetProperty => write!(f, "SetProperty"),
            Action::SetTarget(target) => write!(f, "SetTarget \"{}\"", target),
            Action::SetTarget2 => write!(f, "SetTarget2"),
            Action::SetVariable => write!(f, "SetVariable"),
            Action::StackSwap => write!(f, "StackSwap"),
            Action::StartDrag => write!(f, "StartDrag"),
            Action::Stop => write!(f, "Stop"),
            Action::StopSounds => write!(f, "StopSounds"),
            Action::StoreRegister(r) => write!(f, "StoreRegister {}", r),
            Action::StrictEquals => write!(f, "StrictEquals"),
            Action::StringAdd => write!(f, "StringAdd"),
            Action::StringEquals => write!(f, "StringEquals"),
            Action::StringExtract => write!(f, "StringExtract"),
            Action::StringGreater => write!(f, "StringGreater"),
            Action::StringLength => write!(f, "StringLength"),
            Action::StringLess => write!(f, "StringLess"),
            Action::Subtract => write!(f, "Subtract"),
            Action::TargetPath => write!(f, "TargetPath"),
            Action::Throw => write!(f, "Throw"),
            Action::ToInteger => write!(f, "ToInteger"),
            Action::ToNumber => write!(f, "ToNumber"),
            Action::ToString => write!(f, "ToString"),
            Action::ToggleQuality => write!(f, "ToggleQuality"),
            Action::Try {
                try_body,
                catch_body,
                finally_body,
            } => {
                if let Some((catch_target, catch_actions)) = catch_body {
                    match catch_target {
                        CatchTarget::Variable(v) => writeln!(f, "Try \"{v}\" {{")?,
                        CatchTarget::Register(r) => writeln!(f, "Try register\"{r}\" {{")?,
                    }
                    write!(f, "{}", try_body)?;
                    writeln!(f, "}}")?;
                    writeln!(f, "Catch {{")?;
                    write!(f, "{}", catch_actions)?;
                } else {
                    writeln!(f, "Try {{")?;
                    write!(f, "{}", try_body)?;
                }
                if let Some(finally_actions) = finally_body {
                    writeln!(f, "}}")?;
                    writeln!(f, "Finally {{")?;
                    write!(f, "{}", finally_actions)?;
                }
                write!(f, "}}")?;
                Ok(())
            }
            Action::Trace => write!(f, "Trace"),
            Action::TypeOf => write!(f, "TypeOf"),
            Action::WaitForFrame { frame, skip_count } => {
                write!(f, "WaitForFrame {}, {}", frame, skip_count)
            }
            Action::WaitForFrame2 { skip_count } => {
                write!(f, "WaitForFrame2 {}", skip_count)
            }
        }
    }
}

#[derive(Debug, PartialEq, Serialize, Clone)]
pub enum PushValue {
    Null,
    Undefined,
    String(String),
    Integer(i32),
    Float(f64),
    True,
    False,
    Register(u8),
    Constant(u16),
}

impl std::fmt::Display for PushValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PushValue::Null => write!(f, "null"),
            PushValue::Undefined => write!(f, "undefined"),
            PushValue::String(s) => write!(f, "\"{}\"", s),
            PushValue::Integer(i) => write!(f, "{}", i),
            PushValue::Float(v) => {
                let mut s = v.to_string();
                // Ensure at least one decimal place, for FFDEC compatibility
                if !s.contains('.') {
                    s.push_str(".0");
                }
                write!(f, "{}", s)
            }
            PushValue::True => write!(f, "true"),
            PushValue::False => write!(f, "false"),
            PushValue::Register(r) => write!(f, "register{}", r),
            PushValue::Constant(c) => write!(f, "constant{}", c),
        }
    }
}

impl From<bool> for PushValue {
    fn from(b: bool) -> Self {
        if b { PushValue::True } else { PushValue::False }
    }
}

impl From<i32> for PushValue {
    fn from(i: i32) -> Self {
        PushValue::Integer(i)
    }
}

impl From<f64> for PushValue {
    fn from(f: f64) -> Self {
        PushValue::Float(f)
    }
}

impl From<&str> for PushValue {
    fn from(s: &str) -> Self {
        PushValue::String(s.to_string())
    }
}

impl From<String> for PushValue {
    fn from(s: String) -> Self {
        PushValue::String(s)
    }
}

impl From<f32> for PushValue {
    fn from(f: f32) -> Self {
        PushValue::Float(f as f64)
    }
}

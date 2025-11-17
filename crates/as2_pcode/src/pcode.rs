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
                    write!(f, "{}: ", label)?;
                }
            }
            writeln!(f, "{}", action)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Serialize, Clone)]
pub enum Action {
    Add,
    Add2,
    BitAnd,
    BitLShift,
    BitOr,
    BitRShift,
    BitURShift,
    BitXor,
    CallMethod,
    ConstantPool(Vec<String>),
    Decrement,
    DefineLocal,
    DefineLocal2,
    Divide,
    Equals2,
    GetMember,
    GetVariable,
    Greater,
    If(String),
    Increment,
    InitObject,
    InstanceOf,
    Jump(String),
    Less2,
    Modulo,
    Multiply,
    NewMethod,
    NewObject,
    Not,
    Pop,
    Push(Vec<PushValue>),
    PushDuplicate,
    RandomNumber,
    SetVariable,
    StoreRegister(u8),
    StrictEquals,
    Subtract,
    Trace,
    TypeOf,
}

impl std::fmt::Display for Action {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Action::Add => write!(f, "Add"),
            Action::Add2 => write!(f, "Add2"),
            Action::BitAnd => write!(f, "BitAnd"),
            Action::BitLShift => write!(f, "BitLShift"),
            Action::BitOr => write!(f, "BitOr"),
            Action::BitRShift => write!(f, "BitRShift"),
            Action::BitURShift => write!(f, "BitURShift"),
            Action::BitXor => write!(f, "BitXor"),
            Action::CallMethod => write!(f, "CallMethod"),
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
            Action::DefineLocal => write!(f, "DefineLocal"),
            Action::DefineLocal2 => write!(f, "DefineLocal2"),
            Action::Divide => write!(f, "Divide"),
            Action::Equals2 => write!(f, "Equals2"),
            Action::GetMember => write!(f, "GetMember"),
            Action::GetVariable => write!(f, "GetVariable"),
            Action::Greater => write!(f, "Greater"),
            Action::If(label) => write!(f, "If {}", label),
            Action::Increment => write!(f, "Increment"),
            Action::InitObject => write!(f, "InitObject"),
            Action::InstanceOf => write!(f, "InstanceOf"),
            Action::Jump(label) => write!(f, "Jump {}", label),
            Action::Less2 => write!(f, "Less2"),
            Action::Modulo => write!(f, "Modulo"),
            Action::Multiply => write!(f, "Multiply"),
            Action::NewMethod => write!(f, "NewMethod"),
            Action::NewObject => write!(f, "NewObject"),
            Action::Not => write!(f, "Not"),
            Action::Pop => write!(f, "Pop"),
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
            Action::SetVariable => write!(f, "SetVariable"),
            Action::StoreRegister(r) => write!(f, "StoreRegister {}", r),
            Action::StrictEquals => write!(f, "StrictEquals"),
            Action::Subtract => write!(f, "Subtract"),
            Action::Trace => write!(f, "Trace"),
            Action::TypeOf => write!(f, "TypeOf"),
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

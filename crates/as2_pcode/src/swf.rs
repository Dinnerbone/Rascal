use crate::pcode::{Action, Actions, PushValue};
use byteorder::{LittleEndian, WriteBytesExt};
use indexmap::IndexMap;
use std::collections::HashMap;
use std::io::Result;
use std::io::Write;
use swf::write::SwfWriteExt;
use swf::{Fixed8, SwfStr, Tag, Twips};

impl<'a> ActionEncoder<'a> {}

pub fn pcode_to_swf(actions: &Actions) -> swf::error::Result<Vec<u8>> {
    let mut result = ActionEncoder::new();
    result.write_actions(actions)?;
    result.patch_labels();

    let mut swf = Vec::new();
    swf::write::write_swf(
        &swf::Header {
            compression: swf::Compression::None,
            version: 15,
            stage_size: swf::Rectangle {
                x_min: Twips::ZERO,
                x_max: Twips::from_pixels_i32(100),
                y_min: Twips::ZERO,
                y_max: Twips::from_pixels_i32(100),
            },
            frame_rate: Fixed8::ONE,
            num_frames: 1,
        },
        &[
            Tag::SetBackgroundColor(swf::Color::WHITE),
            Tag::DoAction(&result.output),
            Tag::ShowFrame,
        ],
        &mut swf,
    )?;
    Ok(swf)
}

struct PendingLabelWrite<'a> {
    label: &'a str,
    offset_pos: usize,
}

struct ActionEncoder<'a> {
    output: Vec<u8>,
    pending_label_writes: Vec<PendingLabelWrite<'a>>,
    label_positions: HashMap<&'a str, usize>,
}

impl SwfWriteExt for ActionEncoder<'_> {
    #[inline]
    fn write_u8(&mut self, n: u8) -> Result<()> {
        self.output.write_u8(n)
    }

    #[inline]
    fn write_u16(&mut self, n: u16) -> Result<()> {
        self.output.write_u16::<LittleEndian>(n)
    }

    #[inline]
    fn write_u32(&mut self, n: u32) -> Result<()> {
        self.output.write_u32::<LittleEndian>(n)
    }

    #[inline]
    fn write_u64(&mut self, n: u64) -> Result<()> {
        self.output.write_u64::<LittleEndian>(n)
    }

    #[inline]
    fn write_i8(&mut self, n: i8) -> Result<()> {
        self.output.write_i8(n)
    }

    #[inline]
    fn write_i16(&mut self, n: i16) -> Result<()> {
        self.output.write_i16::<LittleEndian>(n)
    }

    #[inline]
    fn write_i32(&mut self, n: i32) -> Result<()> {
        self.output.write_i32::<LittleEndian>(n)
    }

    #[inline]
    fn write_f32(&mut self, n: f32) -> Result<()> {
        self.output.write_f32::<LittleEndian>(n)
    }

    #[inline]
    fn write_f64(&mut self, n: f64) -> Result<()> {
        self.output.write_f64::<LittleEndian>(n)
    }

    #[inline]
    fn write_string(&mut self, s: &'_ SwfStr) -> Result<()> {
        self.output.write_all(s.as_bytes())?;
        self.write_u8(0)
    }
}

impl<'a> ActionEncoder<'a> {
    fn new() -> Self {
        Self {
            output: Vec::new(),
            pending_label_writes: Vec::new(),
            label_positions: HashMap::new(),
        }
    }

    fn patch_labels(&mut self) {
        for write in &self.pending_label_writes {
            if let Some(position) = self.label_positions.get(write.label) {
                let offset = (*position as isize - (write.offset_pos + 2) as isize) as i16;
                self.output[write.offset_pos..write.offset_pos + 2]
                    .copy_from_slice(&offset.to_le_bytes());
            }
        }
    }

    #[inline]
    fn write_f64_me(&mut self, n: f64) -> Result<()> {
        // Flash weirdly stores f64 as two LE 32-bit chunks.
        // First word is the hi-word, second word is the lo-word.
        self.write_u64(n.to_bits().rotate_left(32))
    }

    fn write_actions(&mut self, actions: &'a Actions) -> Result<()> {
        let mut labels_per_pos: IndexMap<usize, Vec<&str>> = IndexMap::new();
        for (label, &pos) in actions.label_positions.iter() {
            labels_per_pos.entry(pos).or_default().push(label);
        }
        for (i, action) in actions.actions.iter().enumerate() {
            if let Some(labels) = labels_per_pos.get(&i) {
                for label in labels {
                    self.label_positions.insert(label, self.output.len());
                }
            }
            self.write_action(action)?;
        }
        if let Some(labels) = labels_per_pos.get(&actions.actions.len()) {
            for label in labels {
                self.label_positions.insert(label, self.output.len());
            }
        }
        Ok(())
    }

    fn write_action(&mut self, action: &'a Action) -> Result<()> {
        match action {
            Action::Add => self.write_small_action(OpCode::Add),
            Action::Add2 => self.write_small_action(OpCode::Add2),
            // Action::And => self.write_small_action(OpCode::And),
            Action::AsciiToChar => self.write_small_action(OpCode::AsciiToChar),
            Action::BitAnd => self.write_small_action(OpCode::BitAnd),
            Action::BitLShift => self.write_small_action(OpCode::BitLShift),
            Action::BitOr => self.write_small_action(OpCode::BitOr),
            Action::BitRShift => self.write_small_action(OpCode::BitRShift),
            Action::BitURShift => self.write_small_action(OpCode::BitURShift),
            Action::BitXor => self.write_small_action(OpCode::BitXor),
            Action::Call => self.write_small_action(OpCode::Call),
            Action::CallFunction => self.write_small_action(OpCode::CallFunction),
            Action::CallMethod => self.write_small_action(OpCode::CallMethod),
            // Action::CastOp => self.write_small_action(OpCode::CastOp),
            // Action::CharToAscii => self.write_small_action(OpCode::CharToAscii),
            // Action::CloneSprite => self.write_small_action(OpCode::CloneSprite),
            Action::ConstantPool(action) => self.write_constant_pool(action),
            Action::Decrement => self.write_small_action(OpCode::Decrement),
            Action::DefineFunction {
                name,
                params,
                actions,
            } => self.write_define_function(name, params, actions),
            // Action::DefineFunction2(action) => self.write_define_function_2(action),
            Action::DefineLocal => self.write_small_action(OpCode::DefineLocal),
            Action::DefineLocal2 => self.write_small_action(OpCode::DefineLocal2),
            Action::Divide => self.write_small_action(OpCode::Divide),
            Action::Delete => self.write_small_action(OpCode::Delete),
            Action::Delete2 => self.write_small_action(OpCode::Delete2),
            // Action::End => self.write_small_action(OpCode::End),
            // Action::EndDrag => self.write_small_action(OpCode::EndDrag),
            // Action::Enumerate => self.write_small_action(OpCode::Enumerate),
            Action::Enumerate2 => self.write_small_action(OpCode::Enumerate2),
            // Action::Equals => self.write_small_action(OpCode::Equals),
            Action::Equals2 => self.write_small_action(OpCode::Equals2),
            // Action::Extends => self.write_small_action(OpCode::Extends),
            Action::GetMember => self.write_small_action(OpCode::GetMember),
            Action::GetProperty => self.write_small_action(OpCode::GetProperty),
            Action::GetTime => self.write_small_action(OpCode::GetTime),
            // Action::GetUrl(action) => self.write_get_url(action),
            // Action::GetUrl2(action) => self.write_get_url_2(*action),
            Action::GetVariable => self.write_small_action(OpCode::GetVariable),
            // Action::GotoFrame(action) => self.write_goto_frame(*action),
            // Action::GotoFrame2(action) => self.write_goto_frame_2(*action),
            // Action::GotoLabel(action) => self.write_goto_label(action),
            Action::Greater => self.write_small_action(OpCode::Greater),
            Action::If(action) => self.write_if(action),
            // Action::ImplementsOp => self.write_small_action(OpCode::ImplementsOp),
            Action::Increment => self.write_small_action(OpCode::Increment),
            Action::InitArray => self.write_small_action(OpCode::InitArray),
            Action::InitObject => self.write_small_action(OpCode::InitObject),
            Action::InstanceOf => self.write_small_action(OpCode::InstanceOf),
            Action::Jump(action) => self.write_jump(action),
            // Action::Less => self.write_small_action(OpCode::Less),
            Action::Less2 => self.write_small_action(OpCode::Less2),
            // Action::MBAsciiToChar => self.write_small_action(OpCode::MBAsciiToChar),
            // Action::MBCharToAscii => self.write_small_action(OpCode::MBCharToAscii),
            // Action::MBStringExtract => self.write_small_action(OpCode::MBStringExtract),
            // Action::MBStringLength => self.write_small_action(OpCode::MBStringLength),
            Action::Modulo => self.write_small_action(OpCode::Modulo),
            Action::Multiply => self.write_small_action(OpCode::Multiply),
            Action::NewMethod => self.write_small_action(OpCode::NewMethod),
            Action::NewObject => self.write_small_action(OpCode::NewObject),
            // Action::NextFrame => self.write_small_action(OpCode::NextFrame),
            Action::Not => self.write_small_action(OpCode::Not),
            // Action::Or => self.write_small_action(OpCode::Or),
            // Action::Play => self.write_small_action(OpCode::Play),
            Action::Pop => self.write_small_action(OpCode::Pop),
            // Action::PreviousFrame => self.write_small_action(OpCode::PreviousFrame),
            Action::Push(action) => self.write_push(action),
            Action::PushDuplicate => self.write_small_action(OpCode::PushDuplicate),
            Action::RandomNumber => self.write_small_action(OpCode::RandomNumber),
            // Action::RemoveSprite => self.write_small_action(OpCode::RemoveSprite),
            Action::Return => self.write_small_action(OpCode::Return),
            Action::SetMember => self.write_small_action(OpCode::SetMember),
            // Action::SetProperty => self.write_small_action(OpCode::SetProperty),
            // Action::SetTarget(action) => self.write_set_target(action),
            // Action::SetTarget2 => self.write_small_action(OpCode::SetTarget2),
            Action::SetVariable => self.write_small_action(OpCode::SetVariable),
            // Action::StackSwap => self.write_small_action(OpCode::StackSwap),
            // Action::StartDrag => self.write_small_action(OpCode::StartDrag),
            // Action::Stop => self.write_small_action(OpCode::Stop),
            // Action::StopSounds => self.write_small_action(OpCode::StopSounds),
            Action::StoreRegister(action) => self.write_store_register(*action),
            Action::StrictEquals => self.write_small_action(OpCode::StrictEquals),
            // Action::StringAdd => self.write_small_action(OpCode::StringAdd),
            // Action::StringEquals => self.write_small_action(OpCode::StringEquals),
            // Action::StringExtract => self.write_small_action(OpCode::StringExtract),
            // Action::StringGreater => self.write_small_action(OpCode::StringGreater),
            // Action::StringLength => self.write_small_action(OpCode::StringLength),
            // Action::StringLess => self.write_small_action(OpCode::StringLess),
            Action::Subtract => self.write_small_action(OpCode::Subtract),
            // Action::TargetPath => self.write_small_action(OpCode::TargetPath),
            // Action::Throw => self.write_small_action(OpCode::Throw),
            // Action::ToggleQuality => self.write_small_action(OpCode::ToggleQuality),
            // Action::ToInteger => self.write_small_action(OpCode::ToInteger),
            // Action::ToNumber => self.write_small_action(OpCode::ToNumber),
            // Action::ToString => self.write_small_action(OpCode::ToString),
            Action::Trace => self.write_small_action(OpCode::Trace),
            // Action::Try(action) => self.write_try(action),
            Action::TypeOf => self.write_small_action(OpCode::TypeOf),
            // Action::WaitForFrame(action) => self.write_wait_for_frame(*action),
            // Action::WaitForFrame2(action) => self.write_wait_for_frame_2(*action),
            // Action::With(action) => self.write_with(action),
            // Action::Unknown(action) => self.write_unknown(action),
        }
    }

    fn write_action_header(&mut self, opcode: OpCode, length: usize) -> Result<()> {
        self.write_opcode_and_length(opcode as u8, length)
    }

    fn write_opcode_and_length(&mut self, opcode: u8, length: usize) -> Result<()> {
        self.write_u8(opcode)?;
        assert!(
            opcode >= 0x80 || length == 0,
            "Opcodes less than 0x80 must have length 0"
        );
        if opcode >= 0x80 {
            self.write_u16(length as u16)?;
        }
        Ok(())
    }

    /// Writes an action that has no payload.
    fn write_small_action(&mut self, opcode: OpCode) -> Result<()> {
        self.write_action_header(opcode, 0)
    }

    fn write_constant_pool(&mut self, strings: &'a [String]) -> Result<()> {
        let len = 2 + strings.iter().map(|c| c.len() + 1).sum::<usize>();
        self.write_action_header(OpCode::ConstantPool, len)?;
        self.write_u16(strings.len() as u16)?;
        for string in strings {
            self.write_string(SwfStr::from_utf8_str(string))?;
        }
        Ok(())
    }

    fn write_define_function(
        &mut self,
        name: &str,
        params: &'a [String],
        actions: &'a Actions,
    ) -> Result<()> {
        // 1 zero byte for string name, 1 zero byte per param, 2 bytes for # of params,
        // 2 bytes for code length
        let len = name.len() + 1 + 2 + params.iter().map(|p| p.len() + 1).sum::<usize>() + 2;
        self.write_action_header(OpCode::DefineFunction, len)?;
        self.write_string(SwfStr::from_utf8_str(name))?;
        self.write_u16(params.len() as u16)?;
        for param in params {
            self.write_string(SwfStr::from_utf8_str(param))?;
        }
        let length_offset = self.output.len();
        self.write_u16(0)?;
        let length_before_function = self.output.len();
        self.write_actions(actions)?;
        let length_after_function = self.output.len();
        self.output[length_offset..length_offset + 2].copy_from_slice(
            &((length_after_function - length_before_function) as u16).to_le_bytes(),
        );
        Ok(())
    }

    fn write_if(&mut self, label: &'a str) -> Result<()> {
        self.write_action_header(OpCode::If, 2)?;
        self.pending_label_writes.push(PendingLabelWrite {
            label,
            offset_pos: self.output.len(),
        });
        self.write_i16(0)?;
        Ok(())
    }

    fn write_jump(&mut self, label: &'a str) -> Result<()> {
        self.write_action_header(OpCode::Jump, 2)?;
        self.pending_label_writes.push(PendingLabelWrite {
            label,
            offset_pos: self.output.len(),
        });
        self.write_i16(0)?;
        Ok(())
    }

    fn write_push(&mut self, values: &'a [PushValue]) -> Result<()> {
        let len = values
            .iter()
            .map(|v| match v {
                PushValue::String(string) => string.len() + 2,
                PushValue::Null | PushValue::Undefined => 1,
                PushValue::Register(_) | PushValue::False | PushValue::True => 2,
                PushValue::Float(_) => 9,
                PushValue::Integer(_) => 5,
                PushValue::Constant(v) => {
                    if *v < 256 {
                        2
                    } else {
                        3
                    }
                }
            })
            .sum();
        self.write_action_header(OpCode::Push, len)?;
        for value in values {
            self.write_push_value(value)?;
        }
        Ok(())
    }

    fn write_push_value(&mut self, value: &PushValue) -> Result<()> {
        match value {
            PushValue::String(string) => {
                self.write_u8(0)?;
                self.write_string(SwfStr::from_utf8_str(string))?;
            }
            PushValue::Float(v) => {
                self.write_u8(6)?;
                self.write_f64_me(*v)?;
            }
            PushValue::Null => {
                self.write_u8(2)?;
            }
            PushValue::Undefined => {
                self.write_u8(3)?;
            }
            PushValue::Register(v) => {
                self.write_u8(4)?;
                self.write_u8(*v)?;
            }
            PushValue::True => {
                self.write_u8(5)?;
                self.write_u8(1)?;
            }
            PushValue::False => {
                self.write_u8(5)?;
                self.write_u8(0)?;
            }
            PushValue::Integer(v) => {
                self.write_u8(7)?;
                self.write_i32(*v)?;
            }
            PushValue::Constant(v) => {
                if *v < 256 {
                    self.write_u8(8)?;
                    self.write_u8(*v as u8)?;
                } else {
                    self.write_u8(9)?;
                    self.write_u16(*v)?;
                }
            }
        };
        Ok(())
    }

    fn write_store_register(&mut self, register: u8) -> Result<()> {
        self.write_action_header(OpCode::StoreRegister, 1)?;
        self.write_u8(register)?;
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[expect(dead_code)]
pub enum OpCode {
    End = 0x00,

    NextFrame = 0x04,
    PreviousFrame = 0x05,
    Play = 0x06,
    Stop = 0x07,
    ToggleQuality = 0x08,
    StopSounds = 0x09,
    Add = 0x0A,
    Subtract = 0x0B,
    Multiply = 0x0C,
    Divide = 0x0D,
    Equals = 0x0E,
    Less = 0x0F,
    And = 0x10,
    Or = 0x11,
    Not = 0x12,
    StringEquals = 0x13,
    StringLength = 0x14,
    StringExtract = 0x15,

    Pop = 0x17,
    ToInteger = 0x18,

    GetVariable = 0x1C,
    SetVariable = 0x1D,

    SetTarget2 = 0x20,
    StringAdd = 0x21,
    GetProperty = 0x22,
    SetProperty = 0x23,
    CloneSprite = 0x24,
    RemoveSprite = 0x25,
    Trace = 0x26,
    StartDrag = 0x27,
    EndDrag = 0x28,
    StringLess = 0x29,
    Throw = 0x2A,
    CastOp = 0x2B,
    ImplementsOp = 0x2C,

    RandomNumber = 0x30,
    MBStringLength = 0x31,
    CharToAscii = 0x32,
    AsciiToChar = 0x33,
    GetTime = 0x34,
    MBStringExtract = 0x35,
    MBCharToAscii = 0x36,
    MBAsciiToChar = 0x37,

    Delete = 0x3A,
    Delete2 = 0x3B,
    DefineLocal = 0x3C,
    CallFunction = 0x3D,
    Return = 0x3E,
    Modulo = 0x3F,
    NewObject = 0x40,
    DefineLocal2 = 0x41,
    InitArray = 0x42,
    InitObject = 0x43,
    TypeOf = 0x44,
    TargetPath = 0x45,
    Enumerate = 0x46,
    Add2 = 0x47,
    Less2 = 0x48,
    Equals2 = 0x49,
    ToNumber = 0x4A,
    ToString = 0x4B,
    PushDuplicate = 0x4C,
    StackSwap = 0x4D,
    GetMember = 0x4E,
    SetMember = 0x4F,
    Increment = 0x50,
    Decrement = 0x51,
    CallMethod = 0x52,
    NewMethod = 0x53,
    InstanceOf = 0x54,
    Enumerate2 = 0x55,

    BitAnd = 0x60,
    BitOr = 0x61,
    BitXor = 0x62,
    BitLShift = 0x63,
    BitRShift = 0x64,
    BitURShift = 0x65,
    StrictEquals = 0x66,
    Greater = 0x67,
    StringGreater = 0x68,
    Extends = 0x69,

    GotoFrame = 0x81,

    GetUrl = 0x83,

    StoreRegister = 0x87,
    ConstantPool = 0x88,

    WaitForFrame = 0x8A,
    SetTarget = 0x8B,
    GotoLabel = 0x8C,
    WaitForFrame2 = 0x8D,
    DefineFunction2 = 0x8E,
    Try = 0x8F,

    With = 0x94,

    Push = 0x96,

    Jump = 0x99,
    GetUrl2 = 0x9A,
    DefineFunction = 0x9B,
    If = 0x9D,
    Call = 0x9E,
    GotoFrame2 = 0x9F,
}

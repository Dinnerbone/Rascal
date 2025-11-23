#[cfg(test)]
mod tests;
pub(crate) mod tokens;

use crate::lexer::tokens::{ActionName, Token, TokenKind};
use ruasc_common::span::Span;
use winnow::stream::{AsBStr, FindSlice, Location, Stream as _};

pub(crate) type Stream<'i> = winnow::stream::LocatingSlice<&'i str>;

pub struct Lexer<'i> {
    stream: Stream<'i>,
}

impl<'i> Lexer<'i> {
    pub fn new(input: &'i str) -> Self {
        Self {
            stream: Stream::new(input),
        }
    }

    pub fn into_vec(self) -> Vec<Token<'i>> {
        let capacity = core::cmp::min(self.stream.len(), usize::MAX / size_of::<Token>());
        let mut vec = Vec::with_capacity(capacity);
        vec.extend(self);
        vec
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let peek_byte = self.stream.as_bstr().first()?;
            if let Some(token) = process_token(*peek_byte, &mut self.stream) {
                return Some(token);
            }
        }
    }
}

fn process_token<'a>(peek_byte: u8, stream: &mut Stream<'a>) -> Option<Token<'a>> {
    match peek_byte {
        b' ' | b'\t' => {
            stream.next_slice(1);
            None
        }
        b',' => Some(lex_ascii_char(stream, TokenKind::Comma)),
        b'{' => Some(lex_ascii_char(stream, TokenKind::OpenBrace)),
        b'}' => Some(lex_ascii_char(stream, TokenKind::CloseBrace)),
        b'\r' => Some(lex_crlf(stream)),
        b'\n' => Some(lex_ascii_char(stream, TokenKind::Newline)),
        b':' => Some(lex_ascii_char(stream, TokenKind::Colon)),
        b'"' => Some(lex_string(stream)),
        b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'$' => Some(lex_identifier_or_action(stream)),
        b'0'..=b'9' | b'-' => Some(lex_integer_or_float(stream)),
        _ => {
            let start = stream.current_token_start();
            let raw = stream.next_slice(stream.eof_offset());
            let end = stream.previous_token_end();
            Some(Token::new(
                TokenKind::Unknown,
                Span::new_unchecked(start, end),
                raw,
            ))
        }
    }
}

fn lex_ascii_char<'a>(stream: &mut Stream<'a>, kind: TokenKind) -> Token<'a> {
    let start = stream.current_token_start();

    let offset = 1; // an ascii character
    let raw = stream.next_slice(offset);

    let end = stream.previous_token_end();
    let span = Span::new_unchecked(start, end);
    Token::new(kind, span, raw)
}

fn lex_integer_or_float<'a>(stream: &mut Stream<'a>) -> Token<'a> {
    let start = stream.current_token_start();
    let start_checkpoint = stream.checkpoint();

    if stream.as_bstr().first() == Some(&b'-') {
        stream.next_slice(1);
    }

    if let Some(offset) = stream.as_bstr().offset_for(|b| !b.is_ascii_digit()) {
        stream.next_slice(offset)
    } else {
        stream.finish()
    };

    let kind = if stream.as_bstr().first() == Some(&b'.') {
        stream.next_slice(1); // skip the '.'
        if let Some(offset) = stream.as_bstr().offset_for(|b| !b.is_ascii_digit()) {
            stream.next_slice(offset)
        } else {
            stream.finish()
        };
        TokenKind::Float
    } else {
        TokenKind::Integer
    };

    if matches!(stream.as_bstr().first(), Some(b'e' | b'E')) {
        // Optional exponent looks like e+2, E-1, e5, etc
        stream.next_slice(1); // skip the 'e' or 'E'
        if stream.as_bstr().first() == Some(&b'+') || stream.as_bstr().first() == Some(&b'-') {
            stream.next_slice(1); // skip the '+' or '-'
        }
        if let Some(offset) = stream.as_bstr().offset_for(|b| !b.is_ascii_digit()) {
            stream.next_slice(offset);
        }
    }

    let end = stream.previous_token_end();
    stream.reset(&start_checkpoint);
    let raw = stream.next_slice(end - start);

    let end = stream.previous_token_end();
    let span = Span::new_unchecked(start, end);
    Token::new(kind, span, raw)
}

fn lex_crlf<'a>(stream: &mut Stream<'a>) -> Token<'a> {
    let start = stream.current_token_start();

    let mut offset = '\r'.len_utf8();
    let has_lf = stream.as_bstr().get(1) == Some(&b'\n');
    if has_lf {
        offset += '\n'.len_utf8();
    }

    let raw = stream.next_slice(offset);
    let end = stream.previous_token_end();
    let span = Span::new_unchecked(start, end);

    Token::new(TokenKind::Newline, span, raw)
}

pub(crate) const ESCAPE: u8 = b'\\';
fn lex_string<'a>(stream: &mut Stream<'a>) -> Token<'a> {
    let start = stream.current_token_start();

    let offset = 1; // quotation mark
    stream.next_slice(offset);
    let start_checkpoint = stream.checkpoint();
    let quotation_mark = b'"';

    loop {
        // newline is present for error recovery
        if let Some(span) = stream.as_bstr().find_slice((quotation_mark, ESCAPE, b'\n')) {
            let found = stream.as_bstr()[span.start];
            if found == quotation_mark {
                let offset = span.end;
                stream.next_slice(offset);
                break;
            } else if found == ESCAPE {
                let offset = span.end;
                stream.next_slice(offset);

                let peek = stream.as_bstr().peek_token();
                if peek == Some(ESCAPE) || peek == Some(quotation_mark) {
                    let offset = 1; // ESCAPE / QUOTATION_MARK
                    stream.next_slice(offset);
                }
                continue;
            } else if found == b'\n' {
                let offset = span.start;
                stream.next_slice(offset);
                break;
            }
            unreachable!("found `{found}`");
        } else {
            stream.finish();
            break;
        }
    }
    let end = stream.previous_token_end();
    stream.reset(&start_checkpoint);
    let raw = stream.next_slice(end - start - 2);
    stream.next_slice(1);

    let span = Span::new_unchecked(start, end);
    Token::new(TokenKind::String, span, raw)
}

fn lex_identifier_or_action<'a>(stream: &mut Stream<'a>) -> Token<'a> {
    let start = stream.current_token_start();
    let offset = stream
        .as_bstr()
        .offset_for(|b| !b.is_ascii_alphanumeric() && b != b'_' && b != b'$')
        .unwrap_or(stream.eof_offset());
    let raw = stream.next_slice(offset);

    let end = stream.previous_token_end();
    let span = Span::new_unchecked(start, end);
    let lower = raw.to_ascii_lowercase();

    if let Some(num) = lower
        .strip_prefix("constant")
        .and_then(|s| s.parse::<u16>().ok())
    {
        return Token::new(TokenKind::Constant(num), span, raw);
    }

    if let Some(num) = lower
        .strip_prefix("register")
        .and_then(|s| s.parse::<u8>().ok())
    {
        return Token::new(TokenKind::Register(num), span, raw);
    }

    let kind = match lower.as_str() {
        // Actions
        "add" => TokenKind::ActionName(ActionName::Add),
        "add2" => TokenKind::ActionName(ActionName::Add2),
        "asciitochar" => TokenKind::ActionName(ActionName::AsciiToChar),
        "bitand" => TokenKind::ActionName(ActionName::BitAnd),
        "bitlshift" => TokenKind::ActionName(ActionName::BitLShift),
        "bitor" => TokenKind::ActionName(ActionName::BitOr),
        "bitrshift" => TokenKind::ActionName(ActionName::BitRShift),
        "biturshift" => TokenKind::ActionName(ActionName::BitURShift),
        "bitxor" => TokenKind::ActionName(ActionName::BitXor),
        "call" => TokenKind::ActionName(ActionName::Call),
        "callfunction" => TokenKind::ActionName(ActionName::CallFunction),
        "callmethod" => TokenKind::ActionName(ActionName::CallMethod),
        "chartoascii" => TokenKind::ActionName(ActionName::CharToAscii),
        "constantpool" => TokenKind::ActionName(ActionName::ConstantPool),
        "decrement" => TokenKind::ActionName(ActionName::Decrement),
        "definefunction" => TokenKind::ActionName(ActionName::DefineFunction),
        "definelocal" => TokenKind::ActionName(ActionName::DefineLocal),
        "definelocal2" => TokenKind::ActionName(ActionName::DefineLocal2),
        "delete" => TokenKind::ActionName(ActionName::Delete),
        "delete2" => TokenKind::ActionName(ActionName::Delete2),
        "divide" => TokenKind::ActionName(ActionName::Divide),
        "enddrag" => TokenKind::ActionName(ActionName::EndDrag),
        "enumerate2" => TokenKind::ActionName(ActionName::Enumerate2),
        "equals2" => TokenKind::ActionName(ActionName::Equals2),
        "getmember" => TokenKind::ActionName(ActionName::GetMember),
        "getproperty" => TokenKind::ActionName(ActionName::GetProperty),
        "gettime" => TokenKind::ActionName(ActionName::GetTime),
        "geturl" => TokenKind::ActionName(ActionName::GetUrl),
        "geturl2" => TokenKind::ActionName(ActionName::GetUrl2),
        "getvariable" => TokenKind::ActionName(ActionName::GetVariable),
        "gotoframe" => TokenKind::ActionName(ActionName::GotoFrame),
        "gotoframe2" => TokenKind::ActionName(ActionName::GotoFrame2),
        "gotolabel" => TokenKind::ActionName(ActionName::GotoLabel),
        "greater" => TokenKind::ActionName(ActionName::Greater),
        "if" => TokenKind::ActionName(ActionName::If),
        "increment" => TokenKind::ActionName(ActionName::Increment),
        "initarray" => TokenKind::ActionName(ActionName::InitArray),
        "initobject" => TokenKind::ActionName(ActionName::InitObject),
        "instanceof" => TokenKind::ActionName(ActionName::InstanceOf),
        "jump" => TokenKind::ActionName(ActionName::Jump),
        "less2" => TokenKind::ActionName(ActionName::Less2),
        "mbasciitochar" => TokenKind::ActionName(ActionName::MBAsciiToChar),
        "mbchartoascii" => TokenKind::ActionName(ActionName::MBCharToAscii),
        "mbstringextract" => TokenKind::ActionName(ActionName::MBStringExtract),
        "mbstringlength" => TokenKind::ActionName(ActionName::MBStringLength),
        "modulo" => TokenKind::ActionName(ActionName::Modulo),
        "multiply" => TokenKind::ActionName(ActionName::Multiply),
        "newmethod" => TokenKind::ActionName(ActionName::NewMethod),
        "newobject" => TokenKind::ActionName(ActionName::NewObject),
        "nextframe" => TokenKind::ActionName(ActionName::NextFrame),
        "not" => TokenKind::ActionName(ActionName::Not),
        "play" => TokenKind::ActionName(ActionName::Play),
        "pop" => TokenKind::ActionName(ActionName::Pop),
        "prevframe" => TokenKind::ActionName(ActionName::PrevFrame),
        "pushduplicate" => TokenKind::ActionName(ActionName::PushDuplicate),
        "push" => TokenKind::ActionName(ActionName::Push),
        "randomnumber" => TokenKind::ActionName(ActionName::RandomNumber),
        "removesprite" => TokenKind::ActionName(ActionName::RemoveSprite),
        "return" => TokenKind::ActionName(ActionName::Return),
        "setmember" => TokenKind::ActionName(ActionName::SetMember),
        "setvariable" => TokenKind::ActionName(ActionName::SetVariable),
        "startdrag" => TokenKind::ActionName(ActionName::StartDrag),
        "stop" => TokenKind::ActionName(ActionName::Stop),
        "stopsounds" => TokenKind::ActionName(ActionName::StopSounds),
        "storeregister" => TokenKind::ActionName(ActionName::StoreRegister),
        "strictequals" => TokenKind::ActionName(ActionName::StrictEquals),
        "stringadd" => TokenKind::ActionName(ActionName::StringAdd),
        "stringextract" => TokenKind::ActionName(ActionName::StringExtract),
        "stringlength" => TokenKind::ActionName(ActionName::StringLength),
        "subtract" => TokenKind::ActionName(ActionName::Subtract),
        "tointeger" => TokenKind::ActionName(ActionName::ToInteger),
        "tonumber" => TokenKind::ActionName(ActionName::ToNumber),
        "tostring" => TokenKind::ActionName(ActionName::ToString),
        "trace" => TokenKind::ActionName(ActionName::Trace),
        "typeof" => TokenKind::ActionName(ActionName::TypeOf),
        // Other
        "false" => TokenKind::False,
        "null" => TokenKind::Null,
        "true" => TokenKind::True,
        "undefined" => TokenKind::Undefined,
        _ => TokenKind::Identifier,
    };

    Token::new(kind, span, raw)
}

use crate::pcode::PushValue;
use serde::Serialize;
use std::collections::HashMap;

#[derive(Debug, Default, Serialize)]
pub struct Constants {
    strings: Vec<String>,
    lookup: HashMap<String, u16>,
}

impl Constants {
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn add(&mut self, string: String) -> PushValue {
        if let Some(index) = self.lookup.get(&string) {
            return PushValue::Constant(*index);
        }
        if self.strings.len() < u16::MAX as usize {
            let index = self.strings.len() as u16;
            self.strings.push(string.clone());
            self.lookup.insert(string, index);
            return PushValue::Constant(index);
        }
        PushValue::String(string)
    }
}

impl IntoIterator for Constants {
    type Item = String;
    type IntoIter = std::vec::IntoIter<String>;

    fn into_iter(self) -> Self::IntoIter {
        self.strings.into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_single() {
        let mut constants = Constants::empty();
        assert_eq!(constants.add("foo".to_owned()), PushValue::Constant(0));
    }

    #[test]
    fn test_duplicate_is_not_added() {
        let mut constants = Constants::empty();
        assert_eq!(constants.add("foo".to_owned()), PushValue::Constant(0));
        assert_eq!(constants.add("foo".to_owned()), PushValue::Constant(0));
    }

    #[test]
    fn test_none_after_full() {
        let mut constants = Constants::empty();
        for i in 0..u16::MAX {
            assert_eq!(constants.add(format!("foo{}", i)), PushValue::Constant(i));
        }
        assert_eq!(
            constants.add("foo".to_owned()),
            PushValue::String("foo".to_owned())
        );
    }
}

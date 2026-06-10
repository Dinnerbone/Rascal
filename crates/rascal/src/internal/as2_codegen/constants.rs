use crate::internal::as2_pcode::PushValue;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Constants {
    strings: Vec<String>,
    lookup: HashMap<String, u16>,
    tag_length: u16,
}

impl Default for Constants {
    fn default() -> Self {
        Self {
            strings: Vec::new(),
            lookup: HashMap::new(),
            tag_length: 2, // We start off with 2 for the length of strings (as u16)
        }
    }
}

impl Constants {
    pub fn empty() -> Self {
        Self::default()
    }

    pub fn add(&mut self, string: &str) -> PushValue {
        if let Some(index) = self.lookup.get(string) {
            return PushValue::Constant(*index);
        }
        if let Ok(str_length_with_null) = u16::try_from(string.len() + 1)
            && let Some(potential_length) = self.tag_length.checked_add(str_length_with_null)
            && self.strings.len() < u16::MAX as usize
        {
            self.tag_length = potential_length;
            let index = self.strings.len() as u16;
            self.strings.push(string.to_owned());
            self.lookup.insert(string.to_owned(), index);
            return PushValue::Constant(index);
        }

        PushValue::String(string.to_owned())
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
        assert_eq!(constants.add("foo"), PushValue::Constant(0));
    }

    #[test]
    fn test_duplicate_is_not_added() {
        let mut constants = Constants::empty();
        assert_eq!(constants.add("foo"), PushValue::Constant(0));
        assert_eq!(constants.add("foo"), PushValue::Constant(0));
    }

    #[test]
    fn test_none_after_full() {
        let mut constants = Constants::empty();
        let num_chars_per_string = 5;
        let limit = (u16::MAX - 2) / (num_chars_per_string + 1);
        for i in 0..limit {
            assert_eq!(constants.add(&format!("{i:05}")), PushValue::Constant(i));
        }
        assert_eq!(
            constants.add("foobar"),
            PushValue::String("foobar".to_owned())
        );
    }
}

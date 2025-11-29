use crate::constants::Constants;

#[derive(Debug)]
pub struct ScriptContext {
    pub constants: Constants,
    pub next_label: usize,
    pub is_in_tell_target: bool,
    pub can_use_special_properties: bool,
}

impl ScriptContext {
    pub fn new() -> Self {
        Self {
            constants: Constants::empty(),
            next_label: 0,
            is_in_tell_target: false,
            can_use_special_properties: true,
        }
    }

    pub fn create_label(&mut self) -> String {
        let id = self.next_label;
        self.next_label += 1;
        format!("loc{:04x}", id)
    }

    pub fn set_is_in_tell_target(&mut self, is_in_tell_target: bool) {
        self.is_in_tell_target = is_in_tell_target;
    }

    pub fn is_in_tell_target(&self) -> bool {
        self.is_in_tell_target
    }

    pub fn set_can_use_special_properties(&mut self, can_use_special_properties: bool) {
        self.can_use_special_properties = can_use_special_properties;
    }

    pub fn can_use_special_properties(&self) -> bool {
        self.can_use_special_properties
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_labels_unique() {
        let mut context = ScriptContext::new();
        assert_ne!(context.create_label(), context.create_label());
    }
}

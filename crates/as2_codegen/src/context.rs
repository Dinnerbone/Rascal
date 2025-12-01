use crate::constants::Constants;

#[derive(Debug)]
pub struct ScriptContext {
    pub constants: Constants,
    pub next_label: usize,
    pub is_in_tell_target: bool,
    pub can_use_special_properties: bool,
    break_label: Option<String>,
    continue_label: Option<String>,
}

impl ScriptContext {
    pub fn new() -> Self {
        Self {
            constants: Constants::empty(),
            next_label: 0,
            is_in_tell_target: false,
            can_use_special_properties: true,
            break_label: None,
            continue_label: None,
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

    pub fn break_label(&self) -> Option<String> {
        self.break_label.clone()
    }

    pub fn continue_label(&self) -> Option<String> {
        self.continue_label.clone()
    }

    pub fn set_break_label(&mut self, label: Option<String>) -> Option<String> {
        std::mem::replace(&mut self.break_label, label)
    }

    pub fn set_continue_label(&mut self, label: Option<String>) -> Option<String> {
        std::mem::replace(&mut self.continue_label, label)
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

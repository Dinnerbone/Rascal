use crate::constants::Constants;

#[derive(Debug)]
pub struct ScriptContext {
    pub constants: Constants,
    pub next_label: usize,
    pub is_in_tell_target: bool,
}

impl ScriptContext {
    pub fn new() -> Self {
        Self {
            constants: Constants::empty(),
            next_label: 0,
            is_in_tell_target: false,
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

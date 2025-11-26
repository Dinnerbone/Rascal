use crate::constants::Constants;

#[derive(Debug)]
pub struct ScriptContext {
    pub constants: Constants,
    pub next_label: usize,
}

impl ScriptContext {
    pub fn new() -> Self {
        Self {
            constants: Constants::empty(),
            next_label: 0,
        }
    }

    pub fn create_label(&mut self) -> String {
        let id = self.next_label;
        self.next_label += 1;
        format!("loc{:04x}", id)
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

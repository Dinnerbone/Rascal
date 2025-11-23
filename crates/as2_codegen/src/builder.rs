use crate::constants::Constants;
use crate::error::Error;
use ruasc_as2_pcode::{Action, Actions, PushValue};
use ruasc_common::span::Span;

#[derive(Debug)]
pub(crate) struct CodeBuilder<'a> {
    constants: &'a mut Constants,
    errors: Vec<Error>,
    actions: Actions,
    next_label: usize,
    stack_size: u32,
    break_label: Option<String>,
    continue_label: Option<String>,
}

impl<'a> CodeBuilder<'a> {
    pub fn new(constants: &'a mut Constants) -> Self {
        Self {
            constants,
            errors: Vec::new(),
            actions: Actions::empty(),
            next_label: 0,
            stack_size: 0,
            break_label: None,
            continue_label: None,
        }
    }

    pub fn into_actions(self) -> (Actions, Vec<Error>) {
        (self.actions, self.errors)
    }

    pub fn stack_size(&self) -> u32 {
        self.stack_size
    }

    pub fn truncate_stack(&mut self, stack_size: u32) {
        let delta = self.stack_size.saturating_sub(stack_size);
        for _ in 0..delta {
            self.action(Action::Pop);
        }
    }

    pub fn assume_stack_delta(&mut self, delta: i32) {
        // It's fine for stack to go negative - flash does this too. It's weird, though.
        let stack_size = ((self.stack_size as i32) + delta).max(0);
        self.stack_size = stack_size as u32;
    }

    pub fn create_label(&mut self) -> String {
        let id = self.next_label;
        self.next_label += 1;
        format!("loc{:04x}", id)
    }

    pub fn mark_label(&mut self, label: String) {
        if self.actions.label_positions().contains_key(&label) {
            panic!("Label already exists");
        }
        self.actions.push_label(label);
    }

    pub fn action(&mut self, action: Action) {
        let delta = action.stack_delta();
        self.action_with_stack_delta(action, delta);
    }

    pub fn action_with_stack_delta(&mut self, action: Action, stack_delta: i32) {
        self.assume_stack_delta(stack_delta);
        self.actions.push(action);
    }

    pub fn push<V>(&mut self, value: V)
    where
        V: Into<PushValue>,
    {
        self.action(Action::Push(vec![value.into()]));
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

    pub fn constants_mut(&mut self) -> &mut Constants {
        self.constants
    }

    pub fn error(&mut self, message: &'static str, span: Span) {
        self.errors.push(Error { message, span });
    }

    pub fn add_errors(&mut self, mut errors: Vec<Error>) {
        self.errors.append(&mut errors);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_snapshot;

    #[test]
    fn test_multiple_pushes() {
        let mut constants = Constants::empty();
        let mut builder = CodeBuilder::new(&mut constants);
        builder.push(PushValue::True);
        builder.push(PushValue::False);
        assert_snapshot!(builder.actions, @r"
        Push true
        Push false
        ");
    }
}

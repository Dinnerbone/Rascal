use crate::error::Error;
use rascal_as2_pcode::{Action, Actions, PushValue};
use rascal_common::span::Span;

#[derive(Debug)]
pub(crate) struct CodeBuilder {
    errors: Vec<Error>,
    actions: Actions,
    stack_size: u32,
}

impl CodeBuilder {
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            actions: Actions::empty(),
            stack_size: 0,
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
        if !self.actions.has_dangling_label()
            && let Some(Action::Push(values)) = self.actions.last_mut()
        {
            values.push(value.into());
            self.assume_stack_delta(1);
        } else {
            self.action(Action::Push(vec![value.into()]));
        }
    }

    #[expect(dead_code)]
    pub fn error(&mut self, message: &'static str, span: Span) {
        self.errors.push(Error { message, span });
    }

    pub fn add_errors(&mut self, mut errors: Vec<Error>) {
        self.errors.append(&mut errors);
    }

    pub fn append(&mut self, actions: Actions) {
        self.actions.append(actions);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_snapshot;

    #[test]
    fn test_multiple_pushes() {
        let mut builder = CodeBuilder::new();
        builder.push(PushValue::True);
        builder.push(PushValue::False);
        assert_snapshot!(builder.actions, @"Push true, false");
    }

    #[test]
    fn test_multiple_pushes_split_by_label() {
        let mut builder = CodeBuilder::new();
        builder.push(PushValue::True);
        builder.mark_label("loc0001".to_string());
        builder.push(PushValue::False);
        assert_snapshot!(builder.actions, @r"
        Push true
        loc0001:Push false
        ");
    }
}

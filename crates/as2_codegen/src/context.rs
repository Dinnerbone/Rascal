use crate::constants::Constants;

#[derive(Debug)]
pub struct ScriptContext {
    pub constants: Constants,
}

impl ScriptContext {
    pub fn new() -> Self {
        Self {
            constants: Constants::empty(),
        }
    }
}

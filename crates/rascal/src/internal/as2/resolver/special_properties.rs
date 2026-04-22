const SPECIAL_PROPERTIES: &[&str] = &[
    "_x",
    "_y",
    "_xscale",
    "_yscale",
    "_currentframe",
    "_totalframes",
    "_alpha",
    "_visible",
    "_width",
    "_height",
    "_rotation",
    "_target",
    "_framesloaded",
    "_name",
    "_droptarget",
    "_url",
    "_highquality",
    "_focusrect",
    "_soundbuftime",
    "_quality",
    "_xmouse",
    "_ymouse",
];

pub fn get_special_property(name: &str) -> Option<i32> {
    for (i, prop) in SPECIAL_PROPERTIES.iter().enumerate() {
        if name.eq_ignore_ascii_case(prop) {
            return Some(i as i32);
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_special_property() {
        assert_eq!(get_special_property("_x"), Some(0));
        assert_eq!(get_special_property("_foo"), None);
        assert_eq!(get_special_property("_totalFRAMES"), Some(5));
    }
}

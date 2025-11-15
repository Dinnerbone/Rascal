use crate::ast_to_pcode;
use ruasc_as2::ActionScript;

#[test]
fn test_all_samples() {
    insta::glob!("../../../../samples", "**/*.as", |path| {
        let src = std::fs::read_to_string(path).expect("failed to read sample");
        let filename = path.file_name().unwrap().to_string_lossy();
        let ast = ActionScript::new(&filename, &src)
            .to_ast()
            .expect("failed to parse sample");
        let builder = ast_to_pcode(&ast);
        insta::assert_snapshot!(builder);
    });
}

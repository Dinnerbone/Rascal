use crate::ast_to_pcode;
use rascal_as2::ActionScript;

#[test]
fn test_all_samples() {
    insta::glob!("../../../samples", "**/*.as", |path| {
        let src = std::fs::read_to_string(path).expect("failed to read sample");
        let filename = path.file_name().unwrap().to_string_lossy();
        let actionscript = ActionScript::new(&filename, &src);
        let ast = actionscript.to_ast().expect("failed to parse sample");
        let builder = ast_to_pcode(&filename, &src, &ast);
        let result = match builder {
            Ok(v) => v.to_string(),
            Err(e) => e.to_string(),
        };
        insta::assert_snapshot!(result);
    });
}

#[test]
fn test_fail_samples() {
    insta::glob!("../fail_samples", "**/*.as", |path| {
        let src = std::fs::read_to_string(path).expect("failed to read sample");
        let filename = path.file_name().unwrap().to_string_lossy();
        let actionscript = ActionScript::new(&filename, &src);
        let ast = actionscript.to_ast().expect("failed to parse sample");
        let builder = ast_to_pcode(&filename, &src, &ast);
        let result = match builder {
            Ok(v) => v.to_string(),
            Err(e) => e.to_string(),
        };
        insta::assert_snapshot!(result);
    });
}

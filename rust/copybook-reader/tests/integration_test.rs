use copybook_reader;
use parameterized::parameterized;
use std::env;
use std::fs;

fn get_copybook_directory() -> String {
    format!(
        "{}{}",
        env::var("CARGO_MANIFEST_DIR").unwrap(),
        "/tests/resources/copybooks/"
    )
}

#[parameterized(filename = {
    "one_group_field.cpy",
    "one_field.cpy",
    "types.cpy",
    // FIXME: These copybooks are not yet supported
    // "complex.cpy",
    // "line_numbers.cpy",
    // "program.cbl"
})]
fn test_copybook_files(filename: &str) {
    let copybook_str =
        fs::read_to_string(format!("{}{}", get_copybook_directory(), filename)).unwrap();
    let parse_result = copybook_reader::parse(&copybook_str);

    match parse_result {
        Ok(copybook) => {
            println!("{}", copybook);
            assert!(true)
        }
        Err(parse_error) => {
            println!("{}", parse_error);
            unreachable!();
        }
    }
}

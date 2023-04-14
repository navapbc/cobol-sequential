use copybook_reader;
use copybook_reader::copybook;
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
    // FIXME: These copybooks are not yet supported
    // "types.cpy",
    // "complex.cpy",
    // "line_numbers.cpy",
    // "program.cbl"
})]
fn test_copybook_files(filename: &str) {
    let copybook_str =
        fs::read_to_string(format!("{}{}", get_copybook_directory(), filename)).unwrap();
    let parse_result = copybook_reader::parse(&copybook_str);

    match parse_result {
        Ok(_) => assert!(true),
        Err(parse_error) => {
            println!("{}", parse_error);
            unreachable!();
        }
    }
}

#[test]
fn should_parse_pic_field() {
    let parse_result = copybook_reader::parse("01 FIELDNAME PIC X(5).\n");

    match parse_result {
        Ok(copybook_definition) => {
            assert!(copybook_definition.get_groups().len() == 1);

            let group_definition = copybook_definition.get_groups().get(0).unwrap();
            assert!(*group_definition.get_level() == 1);
            assert!(group_definition.get_label() == "FIELDNAME");
            assert!(group_definition.get_statements().len() == 1);

            let line_definition = group_definition.get_statements().get(0).unwrap();

            match line_definition {
                copybook::StatementDefinition::FieldDefinition(def) => {
                    assert!(*def.get_level() == 1u32);
                    assert!(def.get_label() == "FIELDNAME");
                    assert!(def.get_data_type() == "PIC X(5)");
                }
                copybook::StatementDefinition::GroupDefinition(_) => unreachable!(),
            }
        }
        Err(_) => unreachable!(),
    };
}

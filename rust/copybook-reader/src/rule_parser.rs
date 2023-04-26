use pest::error::Error;
use pest::iterators::Pair;
use pest::iterators::Pairs;
use pest::Parser;

use crate::copybook;
use crate::copybook::DataTypeEnum;

#[derive(Parser)]
#[grammar = "src/copybook_grammar.pest"]
struct CopybookPestParser;

fn field_rule_into_definition(field_rule: Pair<Rule>, level: u32) -> copybook::FieldDefinition {
    let mut inner_field_rules = field_rule.into_inner();

    let label: String = inner_field_rules.next().unwrap().as_str().to_owned();

    let data_type_rule = inner_field_rules.next().unwrap();
    let data_type_str: String = data_type_rule.as_str().to_owned();

    // The data_type rule should always contain 1 inner rule that identifies the actual type
    // and carries extra information about that specific data type.
    let subtype_pair = data_type_rule.into_inner().next().unwrap();
    print!("subtype pair: {:?}", subtype_pair);
    let length_and_data_type = match subtype_pair.as_rule() {
        Rule::alphanumeric_type => {
            let mut subtype_inner = subtype_pair.into_inner();

            // The alpha-numeric type always contains a data type length
            let length: u32 = subtype_inner.next().unwrap().as_str()
                .parse()
                .unwrap_or_else(|error|  panic!(" field length should have been an unsigned integer because the grammar for an integer is always a positive number: {error}"));

            (length, DataTypeEnum::AlphaNumeric)
        },
        Rule::numeric_type => {
            let mut subtype_inner = subtype_pair.into_inner();

            // The numeric type always contains a data type length
            //TODO is this repetitive?
            let length: u32 = subtype_inner.next().unwrap().as_str()
                .parse()
                .unwrap_or_else(|error|  panic!(" field length should have been an unsigned integer because the grammar for an integer is always a positive number: {error}"));

            //TODO test this
            //TODO how to handle the derived length?
            (length, DataTypeEnum::Alphabetic)
        },
        _ => unreachable!("Undefined data type rule in rule parser"),
    };
    println!("length and type {:?}", length_and_data_type);

    copybook::FieldDefinition::new_with_length(level, label, data_type_str, length_and_data_type.0, length_and_data_type.1)
}

fn group_rule_into_definition(group: Pair<Rule>, level: u32) -> copybook::GroupDefinition {
    let mut inner_group_rules = group.into_inner();

    let group_label = inner_group_rules.next().unwrap().as_str().to_owned();
    copybook::GroupDefinition::new(level, group_label)
}

pub fn statement_rule_into_definition(statement: Pair<Rule>) -> copybook::StatementDefinition {
    let mut inner_statement_rules = statement.into_inner();

    let level = inner_statement_rules.next().unwrap().as_str()
        .parse()
        .unwrap_or_else(|error|  panic!("level should have been an unsigned integer because the grammar for a level rule is always a positive number: {error}"));

    let next_rule = inner_statement_rules.next().unwrap();
    match next_rule.as_rule() {
        Rule::field => copybook::StatementDefinition::FieldDefinition(field_rule_into_definition(
            next_rule, level,
        )),
        Rule::group => copybook::StatementDefinition::GroupDefinition(group_rule_into_definition(
            next_rule, level,
        )),
        _ => unreachable!(),
    }
}

pub fn string_into_file_rule(copybook_str: &str) -> Result<Pairs<Rule>, Box<Error<Rule>>> {
    let result = CopybookPestParser::parse(Rule::file, copybook_str);
    match result {
        Ok(pairs) => Ok(pairs),
        // the pest_error object is rather large so by boxing it we place it on the heap and avoid unnecessary copies
        Err(pest_error) => Err(Box::new(pest_error)),
    }
}

pub fn map_rule_to_name(rule: &Rule) -> &'static str {
    match rule {
        Rule::EOI => "EOI",
        Rule::whitespace => "whitespace",
        Rule::picture => "picture",
        Rule::level => "level",
        Rule::label => "label",
        Rule::data_type => "data_type",
        Rule::integer => "integer",
        Rule::alphanumeric_type => "alphanumeric_type",
        Rule::numeric_type => "numeric_type",
        Rule::field => "field",
        Rule::group => "group",
        Rule::statement => "statement",
        Rule::file => "file",
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use parameterized::parameterized;

    #[test]
    fn pest_file_grammar_is_valid() {
        let result = CopybookPestParser::parse(Rule::file, "01 FIELDNAME PIC X(5).\n");
        assert!(result.is_ok());
    }

    #[test]
    fn should_parse_group_into_def() {
        let result = CopybookPestParser::parse(Rule::group, "GROUPNAME");
        assert!(result.is_ok());

        let first_group_rule = result.unwrap().next().unwrap();

        let group_def = group_rule_into_definition(first_group_rule, 1);
        assert_eq!(group_def.get_label(), "GROUPNAME");
        assert_eq!(*group_def.get_level(), 1);
        assert_eq!(group_def.get_statements().len(), 0);
    }

    #[test]
    fn should_parse_field_into_def() {
        let result = CopybookPestParser::parse(Rule::field, "FIELDNAME PIC X(5)");
        assert!(result.is_ok());

        let first_field_rule = result.unwrap().next().unwrap();

        let field_definition = field_rule_into_definition(first_field_rule, 1);

        assert_eq!(*field_definition.get_level(), 1u32);
        assert_eq!(field_definition.get_label(), "FIELDNAME");
        assert_eq!(*field_definition.get_length(), Some(5u32));
        assert_eq!(field_definition.get_data_type(), "PIC X(5)");

        let maybe_data_type = field_definition.get_data_type2();
        match maybe_data_type {
            Some(data_type) => match data_type {
                DataTypeEnum::AlphaNumeric => println!("success!"),
                _ => unreachable!("The data type should be AlphaNumeric"),
            },
            None => unreachable!(),
        }
    }

    #[parameterized(copybook_str = {
        "PIC 9(2)",
        "PIC 999",
        "PIC 9(20)"
    })]
    fn should_parse_numeric_types(copybook_str: &str) {
        let result = CopybookPestParser::parse(Rule::data_type, copybook_str);
        assert!(result.is_ok());
    }

    #[parameterized(copybook_str = {
        "FIELD-NAME PIC X(5)", // fieldnames should support dashes
        "FILLER01 PIC X(5)",   // fieldnames should support numbers
        "fILLer PIC X(5)",     // fieldnames should be case insensitive
    })]
    fn should_parse_fieldnames(copybook_str: &str) {
        let result = CopybookPestParser::parse(Rule::field, copybook_str);
        assert!(result.is_ok());
    }

    #[test]
    fn should_parse_statement_into_field_def() {
        let result = CopybookPestParser::parse(Rule::statement, "01 FIELDNAME PIC X(5).\n");
        assert!(result.is_ok());

        let statement_definition = statement_rule_into_definition(result.unwrap().next().unwrap());
        match statement_definition {
            copybook::StatementDefinition::FieldDefinition(field) => {
                assert_eq!(*field.get_level(), 1u32);
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn should_parse_statement_into_group_def() {
        let result = CopybookPestParser::parse(Rule::statement, "01 GROUPNAME.\n");
        assert!(result.is_ok());

        let statement_definition = statement_rule_into_definition(result.unwrap().next().unwrap());
        match statement_definition {
            copybook::StatementDefinition::GroupDefinition(group) => {
                assert_eq!(*group.get_level(), 1u32);
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn statement_def_should_allow_leading_whitespace() {
        let result = CopybookPestParser::parse(Rule::statement, "     01 GROUPNAME.\n");
        assert!(result.is_ok());
    }

    #[test]
    fn invalid_grammar_should_return_error() {
        let result = string_into_file_rule("lksjdf");
        match result {
            Ok(_) => unreachable!("this string should always be invalid"),
            Err(_) => assert!(true),
        }
    }
}

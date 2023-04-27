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
    let length_and_data_type = data_type_rule_into_length_and_type(data_type_rule);

    copybook::FieldDefinition::new(
        level,
        label,
        length_and_data_type.0,
        length_and_data_type.1,
    )
}

fn length_literal_rule_into_u32(length_literal_pair: Pair<Rule>) -> u32 {
    let length_literal_span = length_literal_pair.as_span();
    // Based on the grammar a length literal pair should always have at least 1 inner pair
    // but it could be a alpha-numeric character or an integer.
    let maybe_length_pair = length_literal_pair.into_inner().next();

    match maybe_length_pair {
        None => {
            // When the copybook does not explicitly define the length in the format
            // 9(<length>) then we should assume that the length is implicitly defined by
            // repeating the value 9 for each digit in the number. For example if the PIC clause
            // is 999 the length of the field should be 3.
            
            // Since the grammar guarantees that there are no other values in the matched
            // string besides 9 we can assume that the length of the data type is the length
            // of the matched string.

            //TODO is a usize better than u32?
            (length_literal_span.end() - length_literal_span.start()).try_into().unwrap()

        }
        Some(explicit_length_pair) => {
            explicit_length_pair.as_str()
                .parse()
                .unwrap()
        }
    }
}

fn data_type_rule_into_length_and_type(data_type_pair: Pair<Rule>) -> (u32, DataTypeEnum) {
    // The data_type rule should always contain 1 inner rule that identifies the actual type
    // and carries extra information about that specific data type.
    let subtype_pair = data_type_pair.into_inner().next().unwrap();
    let subtype_span = subtype_pair.as_span();
    println!("subtype pair: {:?}", subtype_pair);
    println!("subtype span: {:?}", subtype_span);
    match subtype_pair.as_rule() {
        Rule::alphanumeric_type => {
            let mut subtype_inner = subtype_pair.into_inner();

            let length_literal_pair = subtype_inner.next().unwrap();
            let length = length_literal_rule_into_u32(length_literal_pair);

            (length, DataTypeEnum::AlphaNumeric)
        }
        Rule::numeric_type => {
            let mut subtype_inner = subtype_pair.into_inner();

            let length_literal_pair = subtype_inner.next().unwrap();
            let length = length_literal_rule_into_u32(length_literal_pair);

            (length, DataTypeEnum::Number())
        }
        _ => unreachable!("Undefined data type rule in rule parser"),
    }
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
        Rule::length_literal => "length_literal",
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
        assert_eq!(*field_definition.get_length(), 5u32);

        let data_type = field_definition.get_data_type();
        match data_type {
            DataTypeEnum::AlphaNumeric => println!("success!"),
            _ => unreachable!("The data type should be AlphaNumeric"),
        }
    }

    #[parameterized(copybook_str = {
        "PIC 9(2)",
        "PIC 999",
        "PIC 9(20)",
        "PIC X(3)",
        "PIC XX",
    }, expected_length = {
        2u32,
        3u32,
        20u32,
        3u32,
        2u32,
    })]
    fn should_parse_lengths(copybook_str: &str, expected_length: u32) {
        let result = CopybookPestParser::parse(Rule::data_type, copybook_str);
        assert!(result.is_ok());

        let data_type_pair = result.unwrap().next().unwrap();
        let length_and_type = data_type_rule_into_length_and_type(data_type_pair);
        assert_eq!(length_and_type.0, expected_length);
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

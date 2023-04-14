use crate::{rule_parser::map_rule_to_name, Rule};
use pest::error::Error;
use std::fmt;

/// A CopybookParseError can occurr when the provided copybook does not match the expected grammar.
pub struct CopybookParseError {
    line_number: usize,
    column_number: usize,
    line: String,
    expected: String,
}

impl CopybookParseError {
    // Creates a new CopybookParseError from a Pest Parsing Error with key information extracted.
    pub fn new(pest_error: &Error<Rule>) -> CopybookParseError {
        let line_column_location = &pest_error.line_col;
        let line_number: usize;
        let column_number: usize;
        match line_column_location {
            pest::error::LineColLocation::Pos(pair) => {
                line_number = pair.0;
                column_number = pair.1;
            }
            pest::error::LineColLocation::Span(pair, _) => {
                line_number = pair.0;
                column_number = pair.1;
            }
        }

        let line = pest_error.line().to_string();

        let expected_type: String = match &pest_error.variant {
            pest::error::ErrorVariant::ParsingError {
                positives,
                negatives: _,
            } => positives
                .iter()
                .map(map_rule_to_name)
                .collect::<Vec<&str>>()
                .join(","),
            pest::error::ErrorVariant::CustomError { message } => message.to_string(),
        };

        CopybookParseError {
            line_number,
            column_number,
            line,
            expected: expected_type,
        }
    }

    // Gets a CopybookParse Error Message.
    pub fn get_message(&self) -> String {
        format!(
            "Failed to parse copybook on Line {}, Column {}. Expected a {} but found \"{}\"",
            self.line_number,
            self.column_number,
            self.expected,
            &self.line[self.column_number - 1..]
        )
    }
}

impl fmt::Display for CopybookParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.get_message())
    }
}

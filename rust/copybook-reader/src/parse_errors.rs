use crate::{rule_parser::map_rule_to_name, Rule};
use pest::error::Error;
use std::fmt;

/// A CopybookParseError can occur when the provided copybook does not match the expected grammar in
/// which case this error may be returned. This struct contains some useful information to help
/// understand which token was not able to be parsed and what token was expected.
///
/// # Example
/// ```
/// // This copybook is invalid because it is missing a period at the end of the first line.
/// let copybook = "01 RECORD
///                     02 FIRST-FIELD PIC X(5).
///                     02 SECOND-FIELD PIC X(5).";
/// let copybook_parse_error = copybook_reader::parse(copybook).err().unwrap();
///
/// // The error can be printed
/// print!("{}", copybook_parse_error);
///
/// assert_eq!(copybook_parse_error.get_message(), "Failed to parse copybook on Line 1, Column 4. Expected a field token but found \"RECORD\"");
/// ```
pub struct CopybookParseError {
    // Line number in the copybook that caused the error.
    line_number: usize,

    // Column number in the copybook that caused the error.
    column_number: usize,

    // Line in copybook that caused the error.
    line: String,

    // The expected token in the
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

    /// Gets a CopybookParse Error Message. This is useful to help understand what is wrong
    /// with the copybook.
    pub fn get_message(&self) -> String {
        format!(
            "Failed to parse copybook on Line {}, Column {}. Expected a {} token but found \"{}\"",
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

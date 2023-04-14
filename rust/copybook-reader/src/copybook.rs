use crate::{rule_parser::map_rule_to_name, Rule};
use lombok::{AllArgsConstructor, Getter};
use pest::error::Error;
use std::fmt;

/// The CopybookDefinition is an IR of the copybook that has been parsed.
/// The CopybookDefinition contains a Vector of [GroupDefinition]s.
#[derive(Getter, Default, Debug)]
pub struct CopybookDefinition {
    groups: Vec<GroupDefinition>,
}

impl CopybookDefinition {
    pub fn create_with_groups(groups: Vec<GroupDefinition>) -> CopybookDefinition {
        CopybookDefinition { groups }
    }

    pub fn add_group(&mut self, group_definition: GroupDefinition) {
        self.groups.push(group_definition);
    }
}

impl PartialEq for CopybookDefinition {
    fn eq(&self, other: &Self) -> bool {
        if self.groups.len() != other.groups.len() {
            return false;
        }

        for (index, group) in self.groups.iter().enumerate() {
            if group != other.groups.get(index).unwrap() {
                return false;
            }
        }

        true
    }
}

impl fmt::Display for CopybookDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let _ = write!(f, "{}", "CopybookDefinition:\n");
        for group in self.get_groups() {
            let _ = write!(f, "{}\n", group);
        }
        write!(f, "{}", "\n")
    }
}

/// The [GroupDefinition] defines a group of related fields in the copybook.
/// These fields are typically grouped within the copybook by their level. For example,
/// this group of copybook fields would be parsed into a single group definition where `01`
/// is the level,`TRANSACTION-RECORD` is the GroupDefinition label, and the fields UID and
/// DESC would make up the statements in the GroupDefinition.
///
/// ```cpy
/// 01 TRANSACTION-RECORD.
///     02 UID PIC 9(5).
///     02 DESC PIC X(25).
/// ```
///
/// More Complex structures may have nested GroupDefinitions as well like this example
/// ```cpy
///  01 TRANSACTION-RECORD.
///     02 UID PIC 9(5).
///     02 DESC PIC X(25).
///         03 ACCOUNT-ID PIC 9(7).
///         03 ACCOUNT-HOLDER PIC X(50).
/// ```
/// [StatementDefinition]s are stored in the same order they exist in the copybook.
#[derive(Getter, Debug)]
pub struct GroupDefinition {
    level: u32,
    label: String,
    statements: Vec<StatementDefinition>,
}

impl GroupDefinition {
    pub fn new(level: u32, label: String) -> GroupDefinition {
        Self::create_with_statements(level, label, Vec::new())
    }

    pub fn create_with_statements(
        level: u32,
        label: String,
        statements: Vec<StatementDefinition>,
    ) -> GroupDefinition {
        GroupDefinition {
            level,
            label,
            statements
        }
    }

    /// Adds a new [StatementDefinition] to the existing [GroupDefinition].
    /// Keep in mind that a [StatementDefinition] can contain either a [FieldDefinition]
    /// or another [GroupDefinition].
    pub fn add_statement(&mut self, statement_definition: StatementDefinition) {
        self.statements.push(statement_definition);
    }
}

impl PartialEq for GroupDefinition {
    fn eq(&self, other: &Self) -> bool {
        if self.statements.len() != other.statements.len() {
            return false;
        }

        for (index, statement) in self.statements.iter().enumerate() {
            if statement != other.statements.get(index).unwrap() {
                return false;
            }
        }

        self.level == other.level && self.label == other.label
    }
}

impl fmt::Display for GroupDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let _ = write!(f, "GroupDefinition level={}, label={}:\n", self.get_level(), self.get_label());
        for statement in self.get_statements() {
            let _ = write!(f, "{}\n", statement);
        }
        write!(f, "GroupDefinition End level={} label={}", self.get_level(), self.get_label())
    }
}

/// A StatementDefinition can either be a [FieldDefinition] or a [GroupDefinition].
#[derive(Debug, PartialEq)]
pub enum StatementDefinition {
    GroupDefinition(GroupDefinition),
    FieldDefinition(FieldDefinition),
}

impl fmt::Display for StatementDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StatementDefinition::GroupDefinition(group) => write!(f, "{}", group),
            StatementDefinition::FieldDefinition(field) => write!(f, "{}", field),
        }
    }
}

/// A FieldDefinition defines a copybook field.
/// This contains it's level, label, and data type.
#[derive(AllArgsConstructor, Getter, Debug, Clone, PartialEq)]
pub struct FieldDefinition {
    level: u32,
    label: String,
    data_type: String, //TODO should be an enum
}

impl fmt::Display for FieldDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "FieldDefinition level={}, label={}, dataType={}", self.get_level(), self.get_label(), self.get_data_type())
    }
}

/// A CopybookParseError can occurr when the provided copybook does not match the expected grammar.
/// Currently this implementation is somewhat naive and only wraps the error from the pest grammar parser.
pub struct CopybookParseError {
    line_number: usize,
    column_number: usize,
    line: String,
    expected: String,
}

//TODO maybe there should be a parse module and copybook module directories
impl CopybookParseError {
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

use crate::Rule;
use lombok::{AllArgsConstructor, Getter};
use pest::error::Error;

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
            statements,
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

/// A StatementDefinition can either be a [FieldDefinition] or a [GroupDefinition].
#[derive(Debug, PartialEq)]
pub enum StatementDefinition {
    GroupDefinition(GroupDefinition),
    FieldDefinition(FieldDefinition),
}

/// A FieldDefinition defines a copybook field.
/// This contains it's level, label, and data type.
#[derive(AllArgsConstructor, Getter, Debug, Clone, PartialEq)]
pub struct FieldDefinition {
    level: u32,
    label: String,
    data_type: String, //TODO should be an enum
}

/// A CopybookParseError can occurr when the provided copybook does not match the expected grammar.
/// Currently this implementation is somewhat naive and only wraps the error from the pest grammar parser.
#[derive(AllArgsConstructor, Debug)]
pub struct CopybookParseError {
    // this is currently dead code, but we should build out a better error interface that
    // provides more descriptive copybook parse errors later. I think ideally we would not
    // need to reference the pest error here either.
    #[allow(dead_code)]
    pest_error: Box<Error<Rule>>,
}

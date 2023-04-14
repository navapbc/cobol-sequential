use std::fmt;

use super::{group_definition::GroupDefinition, FieldDefinition};

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

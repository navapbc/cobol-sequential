use lombok::Getter;
use std::fmt;

use super::StatementDefinition;

/// The CopybookDefinition is an IR of the copybook that has been parsed.
/// The CopybookDefinition contains a Vector of [StatementDefinition]s where each statement can be a
/// field or another group of statements.
#[derive(Getter, Default, Debug)]
pub struct CopybookDefinition {
    statements: Vec<StatementDefinition>,
}

impl CopybookDefinition {
    // Creates a new copybook definition from a Vector of Statements.
    pub fn create_with_statements(statements: Vec<StatementDefinition>) -> CopybookDefinition {
        CopybookDefinition { statements }
    }

    // Adds the provided statement to the end of the copybook.
    pub fn add_statement(&mut self, statement_definition: StatementDefinition) {
        self.statements.push(statement_definition);
    }
}

impl PartialEq for CopybookDefinition {
    // Checks if the current copybook definition matches the provided copybook definition.
    fn eq(&self, other: &Self) -> bool {
        if self.statements.len() != other.statements.len() {
            return false;
        }

        for (index, statement) in self.statements.iter().enumerate() {
            if statement != other.statements.get(index).unwrap() {
                return false;
            }
        }

        true
    }
}

impl Clone for CopybookDefinition {
    fn clone(&self) -> Self {
        CopybookDefinition::create_with_statements(self.get_statements().to_vec())
    }
}

impl fmt::Display for CopybookDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let _ = writeln!(f, "CopybookDefinition:");
        for group in self.get_statements() {
            let _ = writeln!(f, "{}", group);
        }
        writeln!(f)
    }
}

#[cfg(test)]
mod tests {
    use crate::copybook::{DataTypeEnum, FieldDefinition, GroupDefinition};

    use super::*;

    #[test]
    fn matching_copybooks_should_be_equal() {
        let original =
            CopybookDefinition::create_with_statements(vec![StatementDefinition::GroupDefinition(
                GroupDefinition::create_with_statements(
                    1u32,
                    String::from("TOP-LEVEL"),
                    vec![StatementDefinition::FieldDefinition(FieldDefinition::new(
                        2u32,
                        String::from("ONE"),
                        5u32,
                        DataTypeEnum::AlphaNumeric,
                    ))],
                ),
            )]);

        assert_eq!(original, original.clone());
    }

    #[test]
    fn copybooks_with_different_lengths_are_different() {
        let first =
            CopybookDefinition::create_with_statements(vec![StatementDefinition::GroupDefinition(
                GroupDefinition::create_with_statements(1u32, String::from("TOP-LEVEL"), vec![]),
            )]);

        let second = CopybookDefinition::create_with_statements(vec![
            StatementDefinition::GroupDefinition(GroupDefinition::create_with_statements(
                1u32,
                String::from("TOP-LEVEL"),
                vec![],
            )),
            StatementDefinition::GroupDefinition(GroupDefinition::create_with_statements(
                1u32,
                String::from("TOP-LEVEL-2"),
                vec![],
            )),
        ]);

        assert_ne!(first, second);
    }

    #[test]
    fn copybooks_with_different_group_labels_are_different() {
        let first =
            CopybookDefinition::create_with_statements(vec![StatementDefinition::GroupDefinition(
                GroupDefinition::create_with_statements(1u32, String::from("TOP-LEVEL"), vec![]),
            )]);

        let second =
            CopybookDefinition::create_with_statements(vec![StatementDefinition::GroupDefinition(
                GroupDefinition::create_with_statements(1u32, String::from("TOP-LEVEL-2"), vec![]),
            )]);

        assert_ne!(first, second);
    }
}

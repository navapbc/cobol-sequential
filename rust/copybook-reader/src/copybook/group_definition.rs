use lombok::Getter;
use std::fmt;

use super::statement_definition::StatementDefinition;

/// The [GroupDefinition] defines a group of related fields in the copybook.
/// Fields are grouped within the copybook by their level. For example,
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
/// [StatementDefinition]s are always stored in the same order that they exist in the copybook.
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
    /// Keep in mind that a [StatementDefinition] can contain either a FieldDefinition
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

impl Clone for GroupDefinition {
    fn clone(&self) -> Self {
        GroupDefinition::create_with_statements(self.level, self.label.clone(), self.statements.to_vec().clone())
    }
}

impl fmt::Display for GroupDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let _ = writeln!(
            f,
            "GroupDefinition level={}, label={}:",
            self.get_level(),
            self.get_label()
        );
        for statement in self.get_statements() {
            let _ = writeln!(f, "{}", statement);
        }
        write!(
            f,
            "GroupDefinition End level={} label={}",
            self.get_level(),
            self.get_label()
        )
    }
} 

#[cfg(test)]
 mod tests {
    use crate::copybook::FieldDefinition;

    use super::*;
 
     #[test]
     fn matching_groups_should_be_equal() {
        let original = GroupDefinition::create_with_statements(
            1u32, 
            String::from("GROUP"), 
            vec![
                StatementDefinition::FieldDefinition(FieldDefinition::new(
                    2u32,
                    String::from("ONE"),
                    String::from("PIC X(5)"),
                )),
        ]);
 
         assert_eq!(original, original.clone());
     }

     #[test]
    fn groups_with_different_lengths_are_different() {
        let first = GroupDefinition::create_with_statements(
            1u32, 
            String::from("GROUP"), 
            vec![
                StatementDefinition::FieldDefinition(FieldDefinition::new(
                    2u32,
                    String::from("ONE"),
                    String::from("PIC X(5)"),
                )),
        ]);

        let second = GroupDefinition::create_with_statements(
            1u32, 
            String::from("GROUP"), 
            vec![
                StatementDefinition::FieldDefinition(FieldDefinition::new(
                    2u32,
                    String::from("ONE"),
                    String::from("PIC X(5)"),
                )),
                StatementDefinition::FieldDefinition(FieldDefinition::new(
                    2u32,
                    String::from("TWO"),
                    String::from("PIC X(5)"),
                )),
        ]);
 
         assert_ne!(first, second);
    }

    #[test]
    fn groups_with_different_fields_are_different() {
        let first = GroupDefinition::create_with_statements(
            1u32, 
            String::from("GROUP"), 
            vec![
                StatementDefinition::FieldDefinition(FieldDefinition::new(
                    2u32,
                    String::from("ONE"),
                    String::from("PIC X(5)"),
                )),
        ]);

        let second = GroupDefinition::create_with_statements(
            1u32, 
            String::from("GROUP"), 
            vec![
                StatementDefinition::FieldDefinition(FieldDefinition::new(
                    5u32, // different level
                    String::from("ONE"),
                    String::from("PIC X(5)"),
                )),
        ]);
 
         assert_ne!(first, second);
    }
 }

use lombok::Getter;
use std::fmt;

use super::StatementDefinition;

/// The CopybookDefinition is an IR of the copybook that has been parsed.
/// The CopybookDefinition contains a Vector of [StatementDefinition]s.
#[derive(Getter, Default, Debug)]
pub struct CopybookDefinition {
    statements: Vec<StatementDefinition>,
}

impl CopybookDefinition {
    pub fn create_with_statements(statements: Vec<StatementDefinition>) -> CopybookDefinition {
        CopybookDefinition { statements }
    }

    pub fn add_statement(&mut self, statement_definition: StatementDefinition) {
        self.statements.push(statement_definition);
    }
}

impl PartialEq for CopybookDefinition {
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

impl fmt::Display for CopybookDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let _ = writeln!(f, "CopybookDefinition:");
        for group in self.get_statements() {
            let _ = writeln!(f, "{}", group);
        }
        writeln!(f)
    }
}

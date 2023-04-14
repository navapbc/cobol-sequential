use lombok::Getter;
use std::fmt;

use super::group_definition::GroupDefinition;

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
        let _ = writeln!(f, "CopybookDefinition:");
        for group in self.get_groups() {
            let _ = writeln!(f, "{}", group);
        }
        writeln!(f)
    }
}

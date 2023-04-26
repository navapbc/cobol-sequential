use lombok::Getter;
use std::fmt;

use super::DataTypeEnum;

/// A FieldDefinition defines a copybook field.
/// This contains it's level, label, and data type.
#[derive(Getter, Debug, Clone, PartialEq)]
pub struct FieldDefinition {
    // Field's defined level in the copybook. This is the number the comes before every field
    // name in the copybook.
    level: u32,

    // The Field's label is defined by the copybook and used to reference the field.
    label: String,

    //TODO NOTES - maybe add this to the PR as references
    //  - cobol data types: https://www.tutorialspoint.com/cobol/cobol_data_types.htm
    //  - comp clause: https://www.techagilist.com/mainframe/usage-comp-declaration/
    //  - decimal point: https://www.mainframestechhelp.com/tutorials/cobol/assumed-decimal-point-data-type.htm

    // The total length of the field.
    length: u32,

    // The data type for the field.
    data_type: DataTypeEnum,
}

impl FieldDefinition {
    pub fn new(
        level: u32,
        label: String,
        length: u32,
        data_type: DataTypeEnum,
    ) -> FieldDefinition {
        FieldDefinition {
            level,
            label,
            length,
            data_type,
        }
    }
}

impl fmt::Display for FieldDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "FieldDefinition level={}, label={}, length={}",
            self.get_level(),
            self.get_label(),
            self.get_length(),
            //FIXME: implement display attribute for datatype
        )
    }
}

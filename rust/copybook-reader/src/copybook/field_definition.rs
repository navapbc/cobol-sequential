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
    data_type: String, //TODO should be an enum

    // FIXME: I'm adding new fields as optional for now to avoid upfront refactoring but this should be fixed before main
    // The total length of the copybook field
    length: Option<u32>,
    data_type2: Option<DataTypeEnum>,
}

impl FieldDefinition {
    pub fn new(level: u32, label: String, data_type: String) -> FieldDefinition {
        FieldDefinition {
            level,
            label,
            data_type,
            length: None,
            data_type2: None,
        }
    }

    pub fn new_with_length(level: u32, label: String, data_type_str: String,  length: u32, data_type: DataTypeEnum) -> FieldDefinition {
        FieldDefinition { level, label, data_type: data_type_str, length: Some(length), data_type2: Some(data_type) }
    }
}

impl fmt::Display for FieldDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        //FIXME: add length and new data type to output
        write!(
            f,
            "FieldDefinition level={}, label={}, dataType={}",
            self.get_level(),
            self.get_label(),
            self.get_data_type(),
        )
    }
}

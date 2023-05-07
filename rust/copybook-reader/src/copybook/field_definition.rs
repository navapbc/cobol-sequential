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

    // The total character length of the field as specified by the copybook. This should not be
    // treated as the byte-length of the field, although, in most cases it is the same. There are
    // some binary encoded fields that do not have a character length and others that do not
    // have a one-to-one mapping between character length and byte length where they are not the same.
    maybe_char_count: Option<u32>,

    // The data type for the field.
    data_type: DataTypeEnum,
}

impl FieldDefinition {
    pub fn new(
        level: u32,
        label: String,
        maybe_char_count: Option<u32>,
        data_type: DataTypeEnum,
    ) -> FieldDefinition {
        FieldDefinition {
            level,
            label,
            maybe_char_count,
            data_type,
        }
    }
}

impl fmt::Display for FieldDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "FieldDefinition level={}, label={}, char_count={}, type={}",
            self.get_level(),
            self.get_label(),
            self.get_maybe_char_count()
                .map_or(String::from("null"), |count| count.to_string()),
            self.get_data_type(),
        )
    }
}

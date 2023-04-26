use lombok::{AllArgsConstructor, Getter};
use std::fmt;

/// A FieldDefinition defines a copybook field.
/// This contains it's level, label, and data type.
#[derive(AllArgsConstructor, Getter, Debug, Clone, PartialEq)]
pub struct FieldDefinition {
    // Field's defined level in the copybook. This is the number the comes before every field
    // name in the copybook.
    level: u32,

    // The Field's label is defined by the copybook and used to reference the field.
    label: String,

    //TODO NOTES
    //  - cobol data types: https://www.tutorialspoint.com/cobol/cobol_data_types.htm
    //  - comp clause: https://www.techagilist.com/mainframe/usage-comp-declaration/
    //  - decimal point: https://www.mainframestechhelp.com/tutorials/cobol/assumed-decimal-point-data-type.htm

    // Data Type - struct?:
    //      length - uint32?
    //
    //
    //      
    //      type_enum - Enum:
    //          TEXT
    //          NUMERIC struct

    //              sign - Enum:
    //                  SIGNED
    //                  UNSIGNED

    //              comp - Enum
    //                  NONE
    //                  COMP-1
    //                  COMP-2
    //                  COMP-3
    //          
    //          
    //          DECIMAL_IMPLIED_POINT
    //          DECIMAL_ASSUMED_POINT_LEFT
    //          DECIMAL_ASSUMED_POINT_RIGHT
    //
    // decimals:
    //      - digits
    //      - point position
    //      - implied decimal point
    //      - assumed point position
    //          There are two forms of an assumed decimal point. These are expressed in cobol
    //          by defining the position value either on the left or right
    //          LEFT
    //              P(2)9(3)
    //                  Is used to represent very small decimal values
    //                  The P = 2 value means that we are assuming that there are 2 digits
    //                  to the right of the decimal with the value 0. The 9(3) clause means
    //                  to keep/track the following 3 digits.
    //              For Example, we may store the value 0.00345 in this field as 345
    //          RIGHT
    //              9(3)P(2)
    //                  Is used to represent very large integer values
    //                  The 9(3) clause means to keep/track the first 3 digits of the number while
    //                  the P = 2 value mans that we are assuming that the last 2 digits in the number
    //                  are 0.
    //              For example, we may store the 34500 in this field as 345

    //          there can only be 1 assumed decimal point allowed.

    // The total length of the copybook field
    // length: uint32,
    data_type: String, //TODO should be an enum
}

impl fmt::Display for FieldDefinition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "FieldDefinition level={}, label={}, dataType={}",
            self.get_level(),
            self.get_label(),
            self.get_data_type()
        )
    }
}

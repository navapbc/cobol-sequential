//! A COBOL Copybook Data Type. COBOL primarily has three categories of data types.
//! This enum captures those top level types and the additional meta data for each
//! data type.

use lombok::AllArgsConstructor;
#[derive(Debug, Clone, PartialEq)]
pub enum DataTypeEnum {
    // Text fields that contain alphabetic values. In COBOL these may be referenced as A().
    Alphabetic,

    // Text fields that contain alpha-numeric values. In COBOL htese may be referenced as X().
    AlphaNumeric,

    // Number fields contain whole numbers. In COBOL these may be referenced as 9() or 999.
    Number(Number),

    // Decimal fields in COBOL allow you to store approximate values for small fractional values or
    // very large whole numbers without using as many bytes as you would need to store an exact value.
    // In COBOL these may be referenced as 9()V9(), 9().9(), or P()V()
    Decimal(Decimal),
}

// The data type sign identifies if a numerical data type is allowed to be negative or can only be positive.
// It is worth noting that COBOL uses Signed OverPunch to represent negative numbers. This is an
// artifact of programming with punch cards. In order to save space on the punch card for a negative
// number the last digit will be overwritten with a letter that maps back to the original digit.
// The existence of the letter typically indicates that the number was negative.
//TODO test PartialEq
#[derive(Debug, Clone, PartialEq)]
pub enum SignEnum {
    // A signed numerical data type can have negative or positive values.
    SIGNED,

    // An unsigned numerical data type will not track whether the value is positive or negative.
    UNSIGNED,
}

// Computational types are stored in a binary format. COBOL typically stores all data types in an
// ASCII format meaning that the number 12 requires 2 bytes of space. But because computational
// types are stored in binary they can be more compressed.
#[derive(Debug, Clone, PartialEq)]
pub enum CompEnum {
    Ascii, //TODO this does not make sense

    // Comp stores data in hexadecimal format. This does change the total number of bytes required
    // for the field. For example, if you have a field defined as PIC 9(n) COMP
    //      if n is between 1 to 4 then the field will require 2 bytes
    //      if n is between 5 to 9 then the field will require 4 bytes
    //      if n is between 10 to 18 then the field will require 8 bytes
    // This does enable the value 1234567 to be stored as hexadecimal "0012 d687"
    // A PIC clause is required for this type.
    // can be signed or unsigned
    Comp,
    // Comp-1 stores a single-precision floating-point number in hexadecimal format. this field is always 4 bytes. 
    // See https://www.cs.cornell.edu/~tomf/notes/cps104/floating.html for more details
    // NOTE: the syntax is werider here than I thought. Cobol expects there to be no PIC clause or a length literal.
    // So the correct way to define a Comp-1 field is literally just "02 UID COMP-1"
    Comp1,
    // Comp-2 stores a double precision floating point number in hexadecimal format. this field is always 8 bytes.
    // no pic clause allowed.
    Comp2,
    // Comp-3 always signed, pic clause is required. must be signed. Cannot have a V.
    // In Comp-3 each digit represents half a byte (nibble). The sign is always stored in the right most nibble.
    // A negative sign is represented as 1101 and positive is 1100.
    // byte size = ceil(n/2)
    Comp3,
}

//TODO modeling notes:
// so the comp shouldn't be modeled this way because the required syntax is different the byte sizes vary
// which makes this current approach awkward.
// 
// I think the way to go is a comp struct in the data type enum.
// struct comp
//  - comp_type
//  - sign
//
// this does mean that the field length does not always match the byte size which could make things
// so it may make sense to rename the field length to character length and either make the character
// length specific to the type or optional.
// I don't think its a good idea to derive the byte size because it may be different on different machines
// and so it's getting a little bit too far away from modeling the copybook.

// COBOL has three forms of decimals types that can be used to represent numbers with
// fractional parts, extremely small fractional parts, or extremely large numbers.
#[derive(Debug, Clone, PartialEq)]
pub enum DecimalTypeEnum {
    // The implied decimal point is the type of decimal that you typically think about when you
    // see a decimal data type. In COBOL the implied decimal point is typically represented as
    // something like 9(6)V9(2). The V represents where the decimal point should go so the 9(6)
    // clause means that there can be 6 digits to the left of the decimal and the 9(2) clause
    // means that there can be 2 digits to the right of the decimal. The decimal point is
    // considered implied because the decimal is typically not stored in the number. Cobol will
    // just assume that the decimal point belongs after the sixth digit.
    ImpliedPoint,

    // The Assumed Point Left decimal should only be used to represent extremely small numbers
    // where you may not care what the values for the first few digits of the number are. In
    // Cobol you will see this type of decimal represented as something like P(2)9(3) where the
    // P() clause is on the left side of the 9() clause. In this example, the P(2) clause means
    // that we should assume that there are 2 digits just right of the decimal with the value 0.
    // The 9(3) clause means that we should keep the next 3 digits of the number. So for example,
    // we may store the numerical value 0.00345 in this field as just "345"
    AssumedPointLeft,

    // The Assumed Point Right decimal should only be used to represent extremely large numbers
    // where you may not care what the values for the last few digits of the number are. In
    // Cobol you will see this type of decimal represented as something like 9(3)P(2) where the
    // P() clause is on the right side of the 9() clause. In this example, the 9(3) clause means
    // to keep the first 3 digits of the number while the P(2) clause means that we are assuming
    // that the last 2 digits are 0. So for example, we may store the numerical value 34500 in
    // this field as "345"
    AssumedPointRight,
}

// The Decimal helps define the attributes required to understand and use a Cobol Decimal Field.
#[derive(AllArgsConstructor, Debug, Clone, PartialEq)]
pub struct Decimal {
    // The sign of the Decimal Field.
    sign: SignEnum,

    // The Computational Format that this field is stored in.
    comp: CompEnum,

    // Which variation of the cobol decimal type this field is.
    decimal_type: DecimalTypeEnum,

    // The point position of the decimal field. The exact meaning of this value may vary based on
    // the decimal type:
    //
    //  For [DecimalType::ImpliedPoint] this is the number of digits that come before the decimal point.
    //  For [DecimalType::AssumedPointLeft] this is the number of assumed digits to the right of the decimal point.
    //  For [DecimalType::AssumedPointRight] this is the number of assumed digits to the left of the decimal point.
    point_position: u32,
}

// The Number helps define the attributes required to understand and use a Cobol Number Field.
#[derive(AllArgsConstructor, Debug, Clone, PartialEq)]
pub struct Number {
    // The sign of the Number Field.
    sign: SignEnum,

    // The Computational Format that this field is stored in.
    comp: CompEnum,
}

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


    //TODO parse comp1,comp2, and comp3 types
    // The COMP-1 type stores a single-precision floating-point number in binary format. The
    // representation should be similiar to https://www.cs.cornell.edu/~tomf/notes/cps104/floating.html
    // and should always be 4 bytes in length and is always signed. In Cobol this data type is not
    // allowed to have a PIC clause and may be defined as 01 FIELDNAME COMP-1.
    Comp1,

    // The COMP-2 type is a double-precision floating-point number in binary format. This should
    // work similarly to a COMP-1 but instead requires 8 bytes.
    Comp2,

    // The COMP-3 type is a signed integer stored in a compressed binary format. Each digit is stored
    //  as a half a byte or a nibble. The sign is always stored in the right most nibble where
    // "1100" is positive and "1101" is negative. In a copybook this field type is typically
    // defined as 01 FIELDNAME PIC 9(n) COMP-3. The total byte size = ceil(n/2).
    Comp3
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

    // Which variation of the cobol decimal type this field is.
    decimal_type: DecimalTypeEnum,

    // The point position of the decimal field. The exact meaning of this value may vary based on
    // the decimal type:
    //
    //  For [DecimalType::ImpliedPoint] this is the number of digits that come before the decimal point.
    //  For [DecimalType::AssumedPointLeft] this is the number of assumed digits to the right of the decimal point.
    //  For [DecimalType::AssumedPointRight] this is the number of assumed digits to the left of the decimal point.
    point_position: u32,

    // When a Number is stored in COMP format it will be represented as a binary number instead
    // of characters. In the copybook these fields are defined as PIC 9(n)V(m) COMP. The byte-length
    // of these fields depend on the character length provided (n and m) in the PIC clause and the
    // mapping utilized by the cobol compiler.
    //TODO parse this
    isSimpleBinary: bool,
}

// The Number helps define the attributes required to understand and use a Cobol Number Field.
#[derive(AllArgsConstructor, Debug, Clone, PartialEq)]
pub struct Number {
    // The sign of the Number Field.
    sign: SignEnum,

    // When a Number is stored in COMP format it will be represented as a binary number instead
    // of characters. In the copybook these fields are defined as PIC 9(n) COMP. The byte-length
    // of these fields depend on the character length provided in the PIC clause and the mapping
    // utilized by the cobol compiler. Typically this mapping could be something like
    // n=1-4, size=2 bytes; n=5-9, size=4 bytes; n=10-18, size=8 bytes
    isSimpleBinary: bool,
}

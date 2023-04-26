//! A COBOL Copybook Data Type. COBOL primarily has three categories of data types.
//! This enum captures those top level types and the additional meta data for each
//! data type.
pub enum DataTypeEnum {

    // Text fields contain alpha-numeric values. In COBOL these may be referenced as X() or A()
    // TODO: would it make more sense to model this with X or A separate. Or should that just be meta data.
    TEXT,

    // Number fields contain whole numbers. In COBOL these may be referenced as 9() or 999.
    Number(),

    // Decimal fields in COBOL allow you to store approximate values for small fractional values or
    // very large whole numbers without using as many bytes as you would need to store an exact value.
    // In COBOL these may be referenced as 9()V9(), 9().9(), or P()V()
    Decimal()
}

// The data type sign identifies if a numerical data type is allowed to be negative or can only be positive.
pub enum SignEnum {

    // A signed numerical data type can have negative or positive values.
    SIGNED,

    // An unsigned numerical data type will not track whether the value is positive or negative.
    UNSIGNED
}

// Computational types are stored in a binary format. COBOL typically stores all data types in an
// ASCII format meaning that the number 12 requires 2 bytes of space. But because computational
// types are stored in binary they can be more compressed.
pub enum CompEnum {
    Ascii,
    Comp1,
    Comp2,
    Comp3
}

// COBOL has three forms of decimals types that can be used to represent numbers with
// fractional parts, extremely small fractional parts, or extremely large numbers.
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
    AssumedPointRight
} 

// The Decimal helps define the attributes required to understand and use a Cobol Decimal Field.
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
pub struct Number {

    // The sign of the Number Field.
    sign: SignEnum,

    // The Computational Format that this field is stored in.
    comp: CompEnum,
}
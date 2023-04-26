
//! The data type module defines various structures and enums to model COBOL data types that
//! you would find in a copybook

//! A COBOL Copybook Data Type. COBOL primarily has three categories of data types.
//! This enum captures those top level types and the additional meta data for each
//! data type.
enum DataTypeEnum {

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

enum SignEnum {
    SIGNED,
    UNSIGNED
}

enum CompEnum {
    COMP_1,
    COMP_2,
    COMP_3
}

enum DecimalTypeEnum {
    IMPLIED_POINT,
    ASSUMED_POINT_LEFT,
    ASSUMED_POINT_RIGHT
}

struct Decimal {
    sign: SignEnum,
    comp: CompEnum,
    decimal_type: DecimalTypeEnum,
    point_position: uint32,
}

struct Number {
    sign: SignEnum,
    comp: CompEnum,
}
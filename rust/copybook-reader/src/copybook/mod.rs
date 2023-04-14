// Defines related modules for the copybook module
pub mod copybook_definition;
pub mod field_definition;
pub mod group_definition;
pub mod statement_definition;

// Re-Exports Structs. This provides a flatter Interface for the Copybook module
// while still allowing us to store this code in separate files.
pub use self::copybook_definition::CopybookDefinition;
pub use self::field_definition::FieldDefinition;
pub use self::group_definition::GroupDefinition;
pub use self::statement_definition::StatementDefinition;

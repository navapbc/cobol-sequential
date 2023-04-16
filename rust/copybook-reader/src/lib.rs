#![feature(no_coverage)]
//! copybook-reader
//!
//! The CopybookReader library is designed to parse COBOL Copybooks into an Intermediary Representation
//! called a [copybook::CopybookDefinition]. The CopybookDefinition provides a collection of structs and getter methods that
//! allow you to access parts of the copybook programmatically. This is useful for looking up fields, data types,
//! or even parsing file formats defined by the copybook.
//!
//! # What is a Copybook?
//! A copybook is a part of a COBOL program that defines the length and data type for fields and how the fields are structured in a record.
//! COBOL programs can use the copybook to read/write that data to a file for storage or transmission. If you have ever come across a copybook
//! before you may have seen something like this:
//!
//! ```cpy
//!        *> for this copybook the TRANSACTION-RECORD is the parent of the
//!        *> TRANSACTION-DETAIL. The DETAIL-COUNT field is meant to specify
//!        *> the number of TRANSACTION-DETAIL records that follow the
//!        *> TRANSACTION-RECORD
//!         01 TRANSACTION-RECORD.
//!             02 UID PIC 9(5).
//!             02 DESC PIC X(25).
//!             02 ACCOUNT-ID PIC 9(7).
//!             02 ACCOUNT-HOLDER PIC X(50).
//!             02 DETAIL-COUNT PIC 9(5).
//!         01 TRANSACTION-DETAIL.
//!             02 AMOUNT PIC 9(6)V9(2).
//!             02 START-BALANCE PIC 9(6)V9(2).
//!             02 END-BALANCE PIC 9(6)V9(2).
//! ```
//!
//! # Reading a Copybook
//! The [parse] method provides a convenient way to read a COBOL copybook into the [copybook::CopybookDefinition].
//!

extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::{cmp::Ordering, collections::VecDeque};

use copybook::{CopybookDefinition, FieldDefinition, GroupDefinition};

use crate::rule_parser::Rule;

pub mod copybook;
pub mod parse_errors;
mod rule_parser;

/// Parses string containing a copybook into a [copybook::CopybookDefinition].
///
/// # Arguments
///
/// * `copybook_str` - A string that holds the raw copybook.
///
/// # Example
///
/// ```
/// use copybook_reader::copybook::*;
///
/// let copybook = "01 RECORD.
///                     02 FIRST-FIELD PIC X(5).
///                     02 SECOND-FIELD PIC X(5).";
/// let copybook_definition = copybook_reader::parse(copybook).ok().unwrap();
///
/// // Copybook definitions can be displayed easily by printing.
/// print!("{}", copybook_definition);
///
///
/// assert_eq!(copybook_definition,
/// CopybookDefinition::create_with_statements(vec![
///     StatementDefinition::GroupDefinition(GroupDefinition::create_with_statements(
///         1u32,
///         String::from("RECORD"),
///         vec![
///             StatementDefinition::FieldDefinition(FieldDefinition::new(
///                 2u32,
///                 String::from("FIRST-FIELD"),
///                 String::from("PIC X(5)"),
///             )),
///             StatementDefinition::FieldDefinition(FieldDefinition::new(
///                 2u32,
///                 String::from("SECOND-FIELD"),
///                 String::from("PIC X(5)"),
///             )),
///         ],
///     )),
/// ]));
/// ```
pub fn parse(
    copybook_str: &str,
) -> Result<copybook::CopybookDefinition, parse_errors::CopybookParseError> {
    let result = rule_parser::string_into_file_rule(copybook_str);
    if result.is_err() {
        let error = result.err().unwrap();
        log::error!("pest grammar parse error: {error:?}");
        return Err(parse_errors::CopybookParseError::new(error.as_ref()));
    }

    let file = result.unwrap().next().unwrap();

    let mut copybook_definition = copybook::CopybookDefinition::default();

    // VecDeque can operate as a stack as long as we just use the pop_back()/push_back() methods.
    let mut group_stack: VecDeque<GroupDefinition> = VecDeque::new();

    for statement in file.into_inner() {
        match statement.as_rule() {
            Rule::statement => {
                let statement_definition = rule_parser::statement_rule_into_definition(statement);
                match statement_definition {
                    copybook::StatementDefinition::FieldDefinition(new_field) => {
                        log::debug!("New Field Statement: {:?}", new_field.get_label());
                        place_new_field(new_field, &mut group_stack, &mut copybook_definition)
                    }
                    copybook::StatementDefinition::GroupDefinition(new_group) => {
                        log::debug!("New Group Statement: {:?}", new_group.get_label());
                        place_new_group(new_group, &mut group_stack, &mut copybook_definition);
                    }
                }
            }
            Rule::EOI => {
                log::debug!("Reached End Of Input");

                while let Some(group) = group_stack.pop_back() {
                    log::debug!("Pop Group from Stack Onto Copybook");
                    copybook_definition
                        .add_statement(copybook::StatementDefinition::GroupDefinition(group));
                }
            }
            _ => unreachable!(),
        }
    }
    Ok(copybook_definition)
}

fn place_new_field(
    new_field: FieldDefinition,
    group_stack: &mut VecDeque<GroupDefinition>,
    copybook_definition: &mut CopybookDefinition,
) {
    let mut current_group = group_stack.pop_back();
    match current_group.as_mut() {
        None => {
            log::debug!(
                "Add new Field {:?} as Statement to Copybook",
                new_field.get_label()
            );

            // We can add the new field to the copybook definition as a statement since there won't be
            // any more groups or fields added to it.
            copybook_definition
                .add_statement(copybook::StatementDefinition::FieldDefinition(new_field));
        }
        Some(group) => {
            match new_field.get_level().cmp(group.get_level()) {
                Ordering::Greater => {
                    log::debug!(
                        "Add new Field {:?} to current Group {:?}",
                        new_field.get_label(),
                        group.get_label()
                    );
                    group.add_statement(copybook::StatementDefinition::FieldDefinition(new_field));

                    group_stack.push_back(current_group.unwrap())
                }
                Ordering::Less | Ordering::Equal => {
                    // When the field level is lower or equal to the group level then the current
                    // group has terminated and we need to evaluate where to place the current group

                    match group_stack.back_mut() {
                        Some(prev_group) => {
                            log::debug!(
                                "Add current Group {:?} as StatementDefinition to Previous Group {:?}",
                                group.get_label(),
                                prev_group.get_label()
                            );

                            prev_group.add_statement(
                                copybook::StatementDefinition::GroupDefinition(
                                    current_group.unwrap(),
                                ),
                            );
                        }
                        None => {
                            log::debug!(
                                "Add current Group {:?} to Copybook Definition",
                                group.get_label()
                            );
                            copybook_definition.add_statement(
                                copybook::StatementDefinition::GroupDefinition(
                                    current_group.unwrap(),
                                ),
                            );
                        }
                    };

                    //re-evaluate where the new field should go now that the current group has
                    // been merged with the previous group on the stack.
                    place_new_field(new_field, group_stack, copybook_definition);
                }
            }
        }
    }
}

fn place_new_group(
    new_group: GroupDefinition,
    group_stack: &mut VecDeque<GroupDefinition>,
    copybook_definition: &mut CopybookDefinition,
) {
    match group_stack.back_mut() {
        None => {
            log::debug!("Push new group {:?} to stack", new_group.get_label());
            group_stack.push_back(new_group);
        }
        Some(group) => {
            match new_group.get_level().cmp(group.get_level()) {
                Ordering::Greater => {
                    log::debug!("Push new group {:?} to stack", new_group.get_label());
                    group_stack.push_back(new_group);
                }
                Ordering::Equal => {
                    log::debug!(
                        "Add current group {:?} to copybook and push new group to stack {:?}",
                        group.get_label(),
                        new_group.get_label()
                    );
                    copybook_definition.add_statement(
                        copybook::StatementDefinition::GroupDefinition(
                            group_stack.pop_back().unwrap(),
                        ),
                    );
                    group_stack.push_back(new_group);
                }
                Ordering::Less => {
                    log::debug!(
                        "Merge current group {:?} with previous group on stack.",
                        group.get_label()
                    );

                    // remove the current group from the stack and merge it with the previous group on the stack
                    let current_group = group_stack.pop_back().unwrap();
                    group_stack.back_mut().unwrap().add_statement(
                        copybook::StatementDefinition::GroupDefinition(current_group),
                    );

                    // re-evalutate where the new group should be placed with the updated stack
                    place_new_group(new_group, group_stack, copybook_definition);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use test_log::test;

    #[test]
    fn should_parse_pic_field() {
        let parse_result = parse("01 FIELDNAME PIC X(5).\n");

        match parse_result {
            Ok(copybook_definition) => {
                let expected_copybook = copybook::CopybookDefinition::create_with_statements(vec![
                    copybook::StatementDefinition::FieldDefinition(copybook::FieldDefinition::new(
                        1u32,
                        String::from("FIELDNAME"),
                        String::from("PIC X(5)"),
                    )),
                ]);
                assert_eq!(copybook_definition, expected_copybook);
            }
            Err(_) => unreachable!(),
        };
    }

    #[test]
    fn should_parse_group_fields() {
        let parse_result = parse(
            "\
        01 GROUPNAME.
            05 FIRSTFIELD PIC X(5).
            05 SECONDFIELD PIC X(5).
        ",
        );

        match parse_result {
            Ok(copybook_def) => {
                let expected_copybook = copybook::CopybookDefinition::create_with_statements(vec![
                    copybook::StatementDefinition::GroupDefinition(
                        copybook::GroupDefinition::create_with_statements(
                            1u32,
                            String::from("GROUPNAME"),
                            vec![
                                copybook::StatementDefinition::FieldDefinition(
                                    copybook::FieldDefinition::new(
                                        5u32,
                                        String::from("FIRSTFIELD"),
                                        String::from("PIC X(5)"),
                                    ),
                                ),
                                copybook::StatementDefinition::FieldDefinition(
                                    copybook::FieldDefinition::new(
                                        5u32,
                                        String::from("SECONDFIELD"),
                                        String::from("PIC X(5)"),
                                    ),
                                ),
                            ],
                        ),
                    ),
                ]);

                assert_eq!(copybook_def, expected_copybook);
            }
            Err(_) => unreachable!(),
        }
    }

    #[test]
    fn should_parse_nested_group_fields() {
        let parse_result = parse(
            "\
        01 GROUPONE.
            05 FIRSTFIELD PIC X(5).
            05 SECONDFIELD PIC X(5).
            05 GROUPTWO.
                10 THIRDFIELD PIC X(1).
            05 FOURTHFIELD PIC X(1).
        01 GROUPTHREE.
            05 FIFTHFIELD PIC X(9).
        ",
        );

        match parse_result {
            Ok(copybook_def) => {
                let expected_copybook = copybook::CopybookDefinition::create_with_statements(vec![
                    copybook::StatementDefinition::GroupDefinition(
                        copybook::GroupDefinition::create_with_statements(
                            1u32,
                            String::from("GROUPONE"),
                            vec![
                                copybook::StatementDefinition::FieldDefinition(
                                    copybook::FieldDefinition::new(
                                        5u32,
                                        String::from("FIRSTFIELD"),
                                        String::from("PIC X(5)"),
                                    ),
                                ),
                                copybook::StatementDefinition::FieldDefinition(
                                    copybook::FieldDefinition::new(
                                        5u32,
                                        String::from("SECONDFIELD"),
                                        String::from("PIC X(5)"),
                                    ),
                                ),
                                copybook::StatementDefinition::GroupDefinition(
                                    copybook::GroupDefinition::create_with_statements(
                                        5u32,
                                        String::from("GROUPTWO"),
                                        vec![copybook::StatementDefinition::FieldDefinition(
                                            copybook::FieldDefinition::new(
                                                10u32,
                                                String::from("THIRDFIELD"),
                                                String::from("PIC X(1)"),
                                            ),
                                        )],
                                    ),
                                ),
                                copybook::StatementDefinition::FieldDefinition(
                                    copybook::FieldDefinition::new(
                                        5u32,
                                        String::from("FOURTHFIELD"),
                                        String::from("PIC X(1)"),
                                    ),
                                ),
                            ],
                        ),
                    ),
                    copybook::StatementDefinition::GroupDefinition(
                        copybook::GroupDefinition::create_with_statements(
                            1u32,
                            String::from("GROUPTHREE"),
                            vec![copybook::StatementDefinition::FieldDefinition(
                                copybook::FieldDefinition::new(
                                    5u32,
                                    String::from("FIFTHFIELD"),
                                    String::from("PIC X(9)"),
                                ),
                            )],
                        ),
                    ),
                ]);
                assert_eq!(copybook_def, expected_copybook);
            }
            Err(_) => unreachable!(),
        }
    }

    #[test]
    fn should_parse_nested_group_fields2() {
        let parse_result = parse(
            "\
        01 GROUPONE.
            05 GROUPTWO.
                10 FIRSTFIELD PIC X(5).
        01 GROUPTHREE.
            05 SECONDFIELD PIC X(9).
        ",
        );

        match parse_result {
            Ok(copybook_def) => {
                assert_eq!(copybook_def.get_statements().len(), 2);
            }
            Err(_) => unreachable!(),
        }
    }

    #[test]
    fn should_parse_group_with_trailing_top_field() {
        let parse_result = parse(
            "\
        01 GROUPONE.
            05 GROUPTWO.
                10 FIRSTFIELD PIC X(5).
        01 SECONDFIELD PIC X(9).
        ",
        );

        match parse_result {
            Ok(copybook_def) => {
                assert_eq!(copybook_def.get_statements().len(), 2);
            }
            Err(_) => unreachable!(),
        }
    }

    #[test]
    fn should_parse_group_with_field_trailing_group() {
        let parse_result = parse(
            "\
        01 GROUPONE.
            05 GROUPTWO.
                10 FIRSTFIELD PIC X(5).
            05 SECONDFIELD PIC X(9).
        ",
        );

        match parse_result {
            Ok(copybook_def) => {
                assert_eq!(copybook_def.get_statements().len(), 1);
            }
            Err(_) => unreachable!(),
        }
    }

    #[test]
    fn should_parse_nested_group_without_indentation() {
        let parse_result = parse(
            "\
            01 GROUPONE.
            05 FIRSTFIELD PIC X(5).
            ",
        );

        match parse_result {
            Ok(copybook_def) => {
                let expected_copybook = copybook::CopybookDefinition::create_with_statements(vec![
                    copybook::StatementDefinition::GroupDefinition(
                        copybook::GroupDefinition::create_with_statements(
                            1u32,
                            String::from("GROUPONE"),
                            vec![copybook::StatementDefinition::FieldDefinition(
                                copybook::FieldDefinition::new(
                                    5u32,
                                    String::from("FIRSTFIELD"),
                                    String::from("PIC X(5)"),
                                ),
                            )],
                        ),
                    ),
                ]);
                assert_eq!(copybook_def, expected_copybook);
            }
            Err(_) => unreachable!(),
        }
    }

    #[test]
    fn should_fail_when_grammar_is_invalid() {
        // copybook is invalid because it's missing a period
        let parse_result = parse("01 FIELDNAME");
        assert_eq!(parse_result.is_err(), true);
    }
}

[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](LICENSE)
# Cobol Sequential
The Cobol Sequential project aims to provide a library for reading [COBOL sequential files](https://www.tutorialspoint.com/cobol/cobol_file_organization.htm) into the [Apache Arrow](https://github.com/apache/arrow) In-Memory data format. This will make it easier to work with data from legacy COBOL systems in a modern data format.

## Cobol Sequential File Format
The Cobol Sequential File Format is made up of records that are meant to be read in sequential order.

File Attributes:
 - A file can contain more than one type of record.
    - Some additional record types may contain meta-data related to a file.
    - Some record types may form a parent and child relationship.
 - Often times files may use a mainframe file encoding such as EBCDIC.

Record Attributes:
 - Each record is made up of several fields with strict data types
 that are defined by a COBOL Copybook.
 - A record can have a variable or fixed length.
 - A record could have a byte-prefix.

## Project Status
Currently the project is developing an early proto-type of copybook parser in rust.

# Contributing
See the [contributing guidelines](CONTRIBUTING.md)

This is an open project so feel free to report issues, propose new features, or open pull-requests.

# License
[see the Apache 2.0 license here](LICENSE)
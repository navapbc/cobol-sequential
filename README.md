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
 - A record may or may not be followed by a delimiter depending on if it's fixed or variable length.

There can be multiple variations of the attributes listed above that impact how a file is structured.

## Why Apache Arrow?
Apache Arrow provides a columnar In-Memory data format that is typically used for Analytical based workloads. But it is also designed to be highly interoperable which will help make data that was once difficult to reach compatible with some of the most modern data tools around.

Once you have data in the Apache Arrow format you can:
 - Convert the data to other common formats such as CSV, Parquet, ORC, or Arrow IPC.
 - Share the data across programming languages or processes.
 - Write the data directly over the network via Apache Arrow Flight.
 - Perform computations through the pandas library.
 - Perform computations over large files with the Arrow Compute library.
 - Perform computations over large collections of files with the Arrow DataSet library.

Although, Apache Arrow may not be the final destination for your data, it can help you get there in a way that leaves room for your project to pivot as needs change.

## Project Status
Currently the project is developing an early proto-type of copybook parser in rust.

## Contributing
See the [contributing guidelines](CONTRIBUTING.md)

This is an open project so feel free to report issues, propose new features, or open pull-requests.

## License
[see the Apache 2.0 license here](LICENSE)
# Choose Copybook Parser

* Status: accepted
* Deciders: Ben Bemis
* Date: 2023-04-01

Technical Story: [Add First Decisisons](https://github.com/navapbc/cobol-sequential/issues/7)

## Context and Problem Statement

In order to parse the files outputted from COBOL programs we need to be able to parse the copybook
into an IR (Intermediate Representation) so that we can extract the fields for each defined record
layout. There are some options that already exist that are worth considering as well as the
possibility of building our own.

### Additional Background

COBOL programs are well divided into distinct sections such as the I/O section, File section, working
storage section, and the procedure section. The File section defines the relevant record layouts for
the data that the COBOL program will be writing to a file. Sometimes this section is moved to a
separate file and called a copybook.

Copybook Example:

```CPY
       01 TRANSACTION-RECORD.
           02 UID PIC 9(5).
           02 DESC PIC X(25).
           02 DETAILS OCCURS 3 TIMES.
            03 AMOUNT PIC 9(6)V9(2).
            03 START-BALANCE PIC 9(6)V9(2).
            03 END-BALANCE PIC 9(6)V9(2).
           02 ACCOUNT-ID PIC 9(7).
           02 ACCOUNT-HOLDER PIC X(50).
```

Copybooks can also be bundled into a binary file called an [ADATA File](https://www.ibm.com/docs/en/record-generator/3.0?topic=assembler-creating-adata-files-high-level).
This file type contains the same information and is used by some existing parsers.

## Decision Drivers

* We should be able to parse multiple record layouts, redefines, level 88 statements, and the occurs
clause.
* We should be able to load the copybook into an IR that captures multiple record layouts, field
lengths, and field order.
* We should be able to load copybooks at runtime.
* We should be able to parse the raw copybooks from the source code.
   Some parsers utilize a pre-compiled file instead of the source code for the copybook. This may be
   difficult to obtain in some environments and should be avoided.
* We should be able to parse the copybook directly from the COBOL program.
* It must be supported in our choosen programming language.

## Considered Options

* IBM Record Generator
* JRecord
* Cobol Copybook Jsonifier
* Custom ANTLR Parser
* Custom Pest Parser

## Decision Outcome

Chosen option: Custom Pest Parser, because it can be implemented in our core language, it gives us
more control over it's supported copybooks, and Pest is an easy tool to gain access to.

## Pros and Cons of the Options

### IBM Record Generator

The [IBM Record Generator](https://www.ibm.com/docs/en/record-generator/3.0?topic=what-is-record-generator-java)
is a java utility that generates helper classes to describe language-specific record structures such
as copybook records

* Bad, The product is generally available to existing IBM clients that use z/OS so it may not be
available to everyone.
* Bad, it is only designed to read ADATA files instead of copybooks directly.
* Bad, it is only designed to read ADATA files at compile time instead of runtime.
* Bad, it does not provide support for rust.

### JRecord

[JRecord](https://jrecord.sourceforge.net/JRecord02.html) is another interesting java utility that
provides code for reading Cobol Copybooks as well as a handful of similar file formats. It seems like
the library was intially implemented to support a file editor but is now a standalone java library
that also supports Cobol2Xml and Cobol2Csv. It has support for multiple copybook dialects as well as
different file organziations.

* Good, it can read copybooks dynamically.
* Good, There is a lot of flexibility built into the [file organizations](https://sourceforge.net/p/jrecord/wiki/File%20Organisation/)
* Bad, I was not able to get it to parse our record-sequential-multi example file.
* Bad, It is difficult to get up to speed on how it works.
* Bad, It is not written in our core language.

### Cobol Copybook Jsonifier

The [cobol copybook jsonifier](https://github.com/jrperin/cobol-copybook.jsonifier) seems to be a
small project developed in python that extracts the copybook to JSON and then builds a parser that
can then parse the file into a json object. It is an interesting project worth a look over but it
seems like it may not handle multiple record layouts. It does read copybooks dynamically which is one
of things we want.

* Good, it can parse a copybook into JSON format.
* Good, it can read copybooks dynamically.
* Bad, it is not well documented.
* Bad, it is not distributed to any package distribution sites.
* Bad, It does not seem actively supported.
* Bad, it is not written in rust.

### Custom ANTLR Parser

We could build our own parser with [ANTLR 4 Rust](https://github.com/rrevenantt/antlr4rust). This
will enable us to parse the copybook into a AST that we can then use to build classes to represent
the copybook.

* Good, It could be written in rust.
* Good, This approach gives us complete control over the copybook parsing.
* Bad, This does add more complexity to the project.
* Bad, The ANTLR 4 Rust project has not released a stable version yet.
* Bad, It depends on an older version of rust that utilizes older features.
* Bad, It seems like you need to pull a jar file from a branch on the main antlr repo.
* Bad, utilizing a jar file to generate rust code may complicate the build process.

### Custom Pest Parser

[pest](https://pest.rs/) is a parsing library built in rust that generates a parser based on a grammar
defined in PEG (parsing expression language). This is similar to ANTLR but a little bit different.

* Good, It could be mostly written in rust.
* Good, the copybook grammar would be defined separately.
* Good, This approach gives us complete control over the copybook parsing.
* Good, The Pest project is stable.
* Bad, This does add more cmplexity to the project.

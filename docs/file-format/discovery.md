# Discovery: File Formats Defined by COBOL Copybooks
The COBOL programming language has built in support for writing records to a file in copybook format. The record format can be variable-length or fixed-length but is primarily based on the copybook. So if you have the correct copybook and you know the byte range in the file for the record it is fairly straight-forward to extract values from
the record for specific fields.

Often times the difficulty of parsing files that were outputted from COBOL programs fall into four of the following categories:

  - The file encoding may be specific to a mainframe system.
  - The copybook does not define the relationships between different records within a file.
  - Some files may contain a record prefix or a presence indicator that is encoded as a big-endian integer.

There are also a number of common file types that typically get outputted from COBOL programs such as:
 - Sequential Files (Record Sequential)
 - Line Sequential Files
 - Relative Files
 - Indexed Files

 While researching this document I read online documentation and experimented with some simple COBOL programs that output various file structures. These programs have been checked into version control and can be viewed in the directory [research/discovery/cobol-file-writers](discovery-code/cobol-file-writers/). There will be a badge that will link to a code example wherever there is a relevant code example.

## Objective

The goal of this research document is to better understand how COBOL programs structure the data that they output. As well as document some of the common file types that are based on COBOL copybook records so that we can start to think about generalizations that can be made.

Additional research will be required after this but hopefully this is a good starting point for the project.

## Cobol File Organizations
There are four different file organizations that cobol supports you can read more about them [here](https://www.microfocus.com/documentation/object-cobol/ocu42/fhform.htm)

the file organizations that I have explored are:
 - Line Sequential

   The records in these files are normally variable length and are separated by a record delimiter. If a record delimiter is reached before the last field the parser should consider the remaining fields in the record empty.

   This file organization can also contain multiple record layouts.

- Record Sequential

  Record Sequential files are sometimes just referred to as Sequential files because Record Sequential is the default sequential organization and the default file organization for COBOL.
  
  They contain fixed length records and sometimes a variable number of record layouts. Documentation around this organization can be somewhat misleading about this. The term "variable" is frequently used to describe the record length but COBOL **DOES NOT ALLOW VARIABLE LENGTH RECORDS IN THIS ORGANIZATION**. What is variable here is the number of record layouts.

- Relative

  Relative files have a numeric value assigned to each record that allows COBOL to access records randomly or sequentially. The records are required to be fixed length and the file can
  contain "gaps" where no records exist.

- Indexed

  Indexed files have a key field that allows COBOL to access records randomly or sequentially. Records can be fixed length or variable. COBOL always writes the index to a file separate from the data.
  
  Since the data is typically written to a separate sequential file. I will not spend more time on this file organization because if you had parser that supported Record Sequential, Line Sequential, or Relative you should still
  be able to parse the data in these. It could be interesting to explore a feature that enables random lookups
  in a file with the Index but that does not seem as useful at this time.


## File Examples

### Record Sequential Simple [<img src="https://img.shields.io/badge/COBOL-RecordSequentialSimple-informational.svg?logo=LOGO">](discovery-code/cobol-file-writers/record-sequential-simple/)
The fields in a record match the order and length of fields defined in the copybook. So for example a copybook that looks like this:
```CPY
       01 TRANSACTION-RECORD.
           02 UID PIC 9(5).
           02 DESC PIC X(25).
           02 DETAILS.
            03 AMOUNT PIC 9(6)V9(2).
            03 START-BALANCE PIC 9(6)V9(2).
            03 END-BALANCE PIC 9(6)V9(2).
           02 ACCOUNT-ID PIC 9(7).
           02 ACCOUNT-HOLDER PIC X(50).
```
Would contain the the following values in the corresponding byte ranges for the fixed length record.

| field          | byte range   | value                |
| -------------- | -------------|----------------------|
| UID            | 1-5          | 12345                 |
| DESC           | 6-30         | TEST TRANSACTION      |
| AMOUNT         | 31-38        | 000124.34             |
| START-BALANCE  | 39-46        | 000177.54             |
| END-BALANCE    | 47-54        | 53.2                  |
| ACCOUNT-ID     | 55-62        | empty                 |
| ACCOUNT-HOLDER | 63-102       | empty                 |

*empty here means that the byte range is full of empty spaces*

And so a resulting record would look like this in a file
```txt
12345TEST TRANSACTION         0001243400017754000053200000000                                                  
```

### Occurs Clause [<img src="https://img.shields.io/badge/COBOL-RecordSequentialWithOccurs-informational.svg?logo=LOGO">](discovery-code/cobol-file-writers/record-sequential-with-occurs/)
In a copybook an Occurs clause means the structure can repeat multiple times. Let's take a look at an example:
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
Using the same field values as the example above a record would look like this when written to a file:
```txt
12345TEST TRANSACTION         0001243400017754000053200001243400017754000053200001243400017754000053200000000                                                  
```
The key difference to notice here is that Details section is being repeated. So you can see the fields in order `AMOUNT 1 -> START-BALANCE 1 -> END-BALANCE 1 -> AMOUNT 2 -> START-BALANCE 2 -> etc`.

### Redefines
I won't go into as much detail for redefines. But the general idea is that you can define a new field name that references the memory location of an existing field.

Redefine fields can also reference a subset of the memory of the original field. This is kind
of like performing a substring operation of a field without copying it. You can read more about it [here](https://www.tutorialbrain.com/mainframe/cobol-redefines/)

This does not seem like a feature that we need to support in a POC or MVP because the data representation that we choose should provide developers the utilities they need to reproduce
the redefine fields. It could be an interesting feature to have in a future version so that developers would not need to reproduce the redefine fields but we would have to think about if
there is a way to do this without copying the data.
### Level 88
Level 88 statements allow developers to specify potential values for a field and associate them with a label. 88 statements do not have to be exhaustive so values that aren't defined can still be stored in the field. You can read more about them [here](https://www.ibm.com/docs/en/record-generator/3.0?topic=division-condition-name-level-88-statements)

This seems like a difficult feature to reproduce in another intermediate data representation. Since
the values are not exhaustive it also seems a bit mute to figure out how to implement so it probably does not need to be focused on in an early release.

### Multiple Record Layouts [<img src="https://img.shields.io/badge/COBOL-RecordSequentialMultiLayout-informational.svg?logo=LOGO">](discovery-code/cobol-file-writers/record-sequential-multi-layout)
It is possible for a copybook to define multiple record layouts and for a COBOL program to alternate record layouts in the file. This can make a file format highly customized and difficult to parse. In order to be able
to parse one of these files you should look at additional characteristics of the data such as a count field that defines the
number of the next records to expect in the other layout or an indicator field that exists on each record type where it's value indicates a specific record layout.

Let's look at an example with a count field:

```CPY
       01 TRANSACTION-RECORD.
           02 UID PIC 9(5).
           02 DESC PIC X(25).
           02 ACCOUNT-ID PIC 9(7).
           02 ACCOUNT-HOLDER PIC X(50).
           02 DETAIL-COUNT PIC 9(5).
       01 TRANSACTION-DETAIL.
           02 AMOUNT PIC 9(6)V9(2).
           02 START-BALANCE PIC 9(6)V9(2).
           02 END-BALANCE PIC 9(6)V9(2).
```

Using the same field values as the examples above a transaction record with 3 transaction details could look like this when written to a file:

```TXT
^@\^@^@12345TEST TRANSACTION         0000000                                                  00003^@^X^@^@000124340001775400005320^@^X^@^@000124340001775400005320^@^X^@^@000124340001775400005320^@\^@^@
```

A parser could use the count field to determine the number of TRANSACTION-DETAILs to parse before switching back to parsing with the TRANSACTION-RECORD.

But wait this file has Non-Printable characters that are prefixing the record, here they are being referenced as `^@`, `^X`, and `\`. What are the non-printable characters?

#### Non-Printable Record Prefixes

If you look closely at some of the [documentation for the GnuCobol](https://github.com/ayumin/open-cobol) compiler you will notice this excerpt

```
--with-varseq=<n>		Define the format for variable length sequential
				files.

                For values of 0, 1 and 2, four bytes are
				written preceding each record. The format of
				these four bytes for values of 0, 1, 2 is
				as follows :
				n = 0 (default)
					The first 2 bytes are the record length
					in big-endian order. This is compatible
					with mainframe. Bytes 3 and 4 are set
					to binary 0.
				n = 1
					The 4 bytes are the record length in
					big-endian order.
				n = 2
					The 4 bytes are the record length in
					native machine order (int).
					(This was previously the default)

				For the value of 3, two bytes are written
				preceding each record :
				n = 3
					The first 2 bytes are the record length
					in big-endian order. The record follows
					immediately after beginning at byte 3.
```

If you use `xxd -b` to view the first 4 bytes of the file you will see `00000000 01011100 00000000 00000000`. If we assume the varseq is 0 (the default) then the first 2 bytes should be a big-endian integer! After brushing up on [big endian](https://www.geeksforgeeks.org/little-and-big-endian-mystery/) you will see that the value is `92`. That matches the length of our `TRANSACTION-RECORD` perfectly!

If we look past the `TRANSACTION-RECORD` in the file we will see the byte sequence ` 00000000 00011000 00000000 00000000` the first two bytes translate to `24` which matches the length of the `TRANSACTION-DETAIL` record as well.

> **🛈 NOTE:**
> Only the RECORD SEQUENTIAL file organization should contain the record length prefix. This is because a LINE SEQUENTIAL organization comes with delimiters to separate different records so it's not necessary.

So an additional option for parsing multiple record layouts is to read the record length prefix and lookup which record layout matches that length. But this will only work under two conditions:
 - The file must be of RECORD SEQUENTIAL organization.
 - The record layouts must be have different lengths.
   - If all the record layouts are the same length then COBOL will not output the record length prefix.

Relying on this approach may not be great because as the schema evolves there is some chance that record layouts that were not previously the same length could become the same length. Forcing you to change your parsing mechanism. This prefix is not well known either. If your data is passing through multiple hands before it reaches you, it is possible that these bytes could be stripped out or converted to something else and lose their meaning.

The prefix could definitely be useful for doing validation on a file but it may be better to rely on other characteristics of the data first.


### Line Sequential Files [<img src="https://img.shields.io/badge/COBOL-LineSequentialSimple-informational.svg?logo=LOGO">](discovery-code/cobol-file-writers/line-sequential-simple/)
We have already looked at some examples of Fixed-Length Records in the Record Sequential Organization above so let's look at the Variable-Length records that we would see in a Line Sequential File. For this example we will use this copybook:

```CPY
       01 TRANSACTION-RECORD.
           02 UID PIC 9(5).
           02 DESC PIC X(25).
           02 ACCOUNT-ID PIC 9(7).
           02 ACCOUNT-HOLDER PIC X(50).
```

With this copybook if we populated the fields with the following values they would have the corresponding byte ranges in the record:
| field          | byte range   | value                |
| -------------- | -------------|----------------------|
| UID            | 1-5          | 12345                |
| DESC           | 6-30         | empty                |
| ACCOUNT-ID     | 31-37        | 9876543              |
| ACCOUNT-HOLDER | N/A          | empty                |

You should notice that in the table above the first empty field still reserves bytes for the field even though it's empty. Only the last field is truncated from the record, this means only consecutive empty fields at the end of variable length record will conserve space. You will also see this in a file where we wrote two records:

```TXT
12345                         9876543$
12345                         9876543$
```

When you have multiple records in a sequential file there will be a record delimiter. Which is represented by `$` here. In this case `$` represents a Unix line ending `\n` or LF. But the documentation does state that on some other systems the delimiter will be a carriage return and a line feed character.

#### Line Sequential With Occurs [<img src="https://img.shields.io/badge/COBOL-LineSequentialWithOccurs-informational.svg?logo=LOGO">](discovery-code/cobol-file-writers/line-sequential-with-occurs)
Even if you use an Occurs group of fields and choose not populate the last field in the group the field will still
reserve the bytes for the last field.

COBOL does this because there is no delimiter between each Occur field  to tell when one occur group of values end and the next one begins. So variable length records don't conserve space with
occur fields.

#### Line Sequential with Multiple Record Layouts [<img src="https://img.shields.io/badge/COBOL-LineSequentialMultiLayout-informational.svg?logo=LOGO">](discovery-code/cobol-file-writers/line-sequential-multi-layout)
Line Sequential files can also have multiple record layouts. These files are often easier to view with text editors because unlike having multiple record layouts in a record sequential file there is no record length prefix! This is because each record in the file is separated by the same record delimiter. For example, if you have the following record layouts where the `TRANSACTION-RECORD` always preceds a `TRANSACTION-DETAIL` your copybook could look like this:

```CPY
       01 TRANSACTION-RECORD.
           02 REC-IND PIC X(1).
           02 UID PIC 9(5).
           02 DESC PIC X(25).
           02 ACCOUNT-ID PIC 9(7).
           02 ACCOUNT-HOLDER PIC X(50).
       01 TRANSACTION-DETAIL.
           02 REC-IND PIC X(1).
           02 AMOUNT PIC 9(6)V9(2).
           02 START-BALANCE PIC 9(6)V9(2).
           02 END-BALANCE PIC 9(6)V9(2).
```

And the output in the file would look like this:

```TXT
T12345TEST TRANSACTION         0000000$
D000124340001775400005320$
D000124340001775400005320$
D000124340001775400005320$
```

The newline characters are a lot easier on the eyes and it is fairly easy to pick out different records. But without the record length prefix you **MUST** use some application specific business logic in order to distinguish which record layout is associated with each record. For this example, I created a `REC-IND` field at the begining of each record layout. If the
value is `T` then the record is a `TRANSACTION-RECORD` but if the value is `D` it is a `TRANSACTION-DETAIL` record.

There are many ways that a COBOL application could be identifying records, some methods I have identified so far are:
 - expected trailer record count field
 - record identifier field
 - record length (this is not a good idea with variable length records or line sequential files)

### Relative Files [<img src="https://img.shields.io/badge/COBOL-RelativeSimple-informational.svg?logo=LOGO">](discovery-code/cobol-file-writers/relative-simple)
The records in a relative file always have a fixed-length. A relative file has a numeric number assigned to every record, this number represents the position of the record relative to the begining of the file. So for example if you
wanted a record to be the 6th record in a file you would specify 6 as the value for the key. This allows a parser that is aware of the record length to skip to that byte-offset and read only the 6th record.

This also means that there can be "gaps" in the file. So for example if only records 1, 2, and 4 were written to the file
then the space for record 3 would be zeroed out.

Let's show an example of this, if we had this copybook:
```CPY
       01 TRANSACTION-RECORD.
           02 UID PIC 9(5).
           02 DESC PIC X(25).
           02 ACCOUNT-ID PIC 9(7).
           02 ACCOUNT-HOLDER PIC X(50).
```
with a 1 digit key, for this example we also populated the UIDs for each record as `00010`, `00020`, `00040`.

```TXT
W^@^@^@^@^@^@^@00010                         9876543                                                  W^@^@^@^@^@^@^@00020                         9876543                                                  ^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@^@W^@^@^@^@^@^@^@00040                         9876543                                                  
```

You should notice that bytes 191-286 contain Non-Printable characters. This section of the file is meant to hold a record with the key `3` but since we never wrote one it the bytes have been zeroed out.

Our record length is 87 characters so it almost fills the full gap of 95 characters of Non-Printable characters. It seems that COBOL is prefixing each record with 8 additional bytes.

Upon further inspection, If the record is populated the first byte will be `W` and the next 7 bytes will be a binary 0 value which tends to show up as an Non-printable character in most text editors. If a record is not populated all bytes
will be reserved with a binary 0 value. This does seem to allow a parser to identify which spaces have been  populated.

I'm still looking for documentation that confirms this behavior but it does seem to make sense.

Now this file organization is a bit more specialized than the other two I have looked at. It could be useful to provide the ability to read these files sequentially so that developers can gain access to them, but I'm not sure if there is value in this project providing random access to these files since it seems like once you read them sequentially you could load them into a datastore that manages that for you. Although, I suppose you may need to keep the key values with the record to maintain compatibility in some scenarios.

## Common Challenges

### File Encoding
Most often files like these are generated by COBOL programs running on a mainframe system. So it seems unlikely
that the file will be encoded in utf-8 so whatever
parsing strategies are implemented must account for
different file encodings. It is best to ask the owners of the process that produces the file which encoding they use. Keep in mind there are often several variations of each encoding.

I would think the record length prefix would not get borked when converting between file encodings.
Although, I am not certain that this will always hold so it may be good to encourage people to check.


### Nullability
COBOL also does not seem to have a clear concept of nullability. You can leave a field blank but cobol will pad the field with default values most of the time.
The only exception being when a field is at the end of a variable length record.

The default values for COBOL variables could be interpretted as null values in some cases but considering that a default value for a number or a decimal contains zeros, It is very possible that that could have some kind of business value and does not actually indicate a missing value or a null.

So it seems like a good solution would empower applications to make their own business decision on how to treat COBOL default values.
### Differences in COBOL compilers and Differences over Time
When you look at the documentation for the [GnuCobol SELECT Clause](https://gnucobol.sourceforge.io/HTML/gnucobpg.html#SELECT) you may notice a
section that says:

> The COLLATING SEQUENCE, RECORD DELIMITER, RESERVE and ALL OTHER clauses are syntactically recognized but are otherwise non-functional.

From this statement I would infer that a developer used to be able to 
specify a delimiter on any file organization with the GnuCobol compiler. Maybe this is a point that someone with COBOL experience could weigh in on.

But potentially this could mean that programs which still use an older version of their compilers could produce files that are structured a bit differently.

Additionally, entirely different compilers may do things differently as well. For example, I have not be able to find a way to create fixed-length record in a Sequential File using the GnuCobol compiler, but there
is some [ibm documentation](https://www.ibm.com/docs/en/cobol-zos/6.2?topic=cobol-describing-structure-line-sequential-file) that suggests that can be done with their compiler. Now I am in-experienced with COBOL so I could be missing something here.

But I think this means that a good solution would not adopt the
concepts of line sequential, record sequential, or relative too strictly because they do seem somewhat loosely defined. Instead, we should provide an interface that focuses more on file attributes so that if a developer is dealing with a line sequential file with fixed width records they could do that.

### Files could be borked by an Upstream Process
I think it's worth thinking about the possibility of file formats being borked upstream.

These could be small issues such as corrupting a record length prefix. Which I think a parser could handle in most situations if we allow developers to specify other parsing options. So this kind of goes along with the idea of not coupling too tightly with the definitions of file types that we have here. Instead a developer should be able to specify their own file structure and methodology of parsing record layouts.

### Expressing Multiple Record Layouts
I think the reliable pieace of these file formats is how the record is parsed into fields which is defined by the copybook.
But the copybook does not define how record layouts are organized in the file.

This is also where I think we need to make
some assumptions in order to get the data into a generic intermediate representation:
 1. When there are multiple record layouts in a single file it is helpful to assume that the other record layouts represent a nested data structure.
 2. The records that are associated with a single entity will be contiguous.

I have actually made these two assumptions earlier in the examples I provided for multiple record layouts. But these assumptions will allow us to basically define a parent record layout and child record layouts and assemble the data from multiple matching contiguous records into a single entity in a new generic data representation like (JSON object, Avro record, Apache Arrow record, or Flat Buffer record).

So for our `TRANSACTION-RECORD` and `TRANSACTION-DETAIL` we might have some configuration that basically says:
```psuedo
TRANSACTION-RECORD is the parent
  contains list of TRANSACTION-DETAILs up to the max TRANSACTION-RECORD.DTL-CNT
```
this is still a rough idea and we would need to figure out what the actual interface for this configuration would be. But it would express that there is a top level parent record that contains child records with a methodolgy for determining when to use a different record layout when parsing.

There could be a need to define record layouts in this configuration that are like meta data records. I have seen real world examples where a record layout is only used once as the first record in a file to describe attributes of the file that are useful to have when processing records. So we might say:
```psuedo
FILE-HEADER-RECORD is meta data
  and is always first record

TRANSACTION-RECORD is the parent
  contains list of TRANSACTION-DETAILs up to the max TRANSACTION-RECORD.DTL-CNT
```
We would want to be able to parse this meta data into our generic
data representation and provide it alongside a record.

In order to soften the blow of our two assumptions above in the future we could consider a RecordLayout filter. In cases where the multiple record layouts do not form a nested structure a filter could allow the parser to still parse the records that are related to each other into a generic data representation. A
developer could then parse the file multiple times with different filters to get all the data out of the file and into different structures in memory. This may suit some more complex
scenarios and allow a developer to lean more on the libraries for
the generic data representation that we choose to re-assemble the data as necessary.

### Record Parsing Options
As we have seen with the examples above, there the files can be fairly customized. Some file structure options that we should consider are
 - Record Prefix
   - 2-byte big-endian record length
   - 3-byte big-endian record length
   - 4-byte big-endian record length
   - 8-byte record presence indicator
   - Ignore
 - Record Delimiter
 - Record length (variable or fixed)

## Resources
 - [MicroFocus COBOL File Organizations](https://www.microfocus.com/documentation/object-cobol/ocu42/fhform.htm)
 - [Tutorial Brain COBOL Redefines](https://www.tutorialbrain.com/mainframe/cobol-redefines/)
 - [IBM COBOL Level 88](https://www.ibm.com/docs/en/record-generator/3.0?topic=division-condition-name-level-88-statements)
 - [GnuCobol Usage Docs](https://github.com/ayumin/open-cobol)
 - [IBM Z/os Line Sequential](https://www.ibm.com/docs/en/cobol-zos/6.2?topic=cobol-describing-structure-line-sequential-file)
 - [IBM MainFramer](https://www.ibmmainframer.com/cobol-tutorial/cobol-occurs-clause/)
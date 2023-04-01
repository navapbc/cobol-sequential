# Choose In-Memory Data Format

* Status: accepted

* Deciders: Ben Bemis
* Date: 2023-04-01

Technical Story: [Add First Decisisons](https://github.com/navapbc/cobol-sequential/issues/7)

## Context and Problem Statement

After we read the data from the sequential file format what format do we put the data in? The
format that we transform the data to will need to be convenient for users of this library.

## Decision Drivers <!-- optional -->

* The In-Memory data format needs to be compatible with the copybook and sequential file structure.

    This does mean that we need to support nested data structures and we should be able to map copybook
    data types to data types in the in-memory format.

* The In-Memory data format needs to be Interoperable.

    This is where the copybook representation fails the most and this library could add the most
    value. There are incredibly few technologies that can consume data in the copybook format.
    To help developers the most we should choose an In-Memory format that enables access to as many
    modern data tools as possible.

* The In-Memory data format should have supporting tools for data analysis.

    It is common for projects to start with the motive of making old data formats more accessible.
    When this happens these types of projects sometimes unlock a flood gate of data questions that
    no one had thought to ask before. This can quickly become frustrating for teams that were not
    expecting to dive into data analysis and did not build tools in a way to prepare for it.

* The In-Memory data format should be Performant.

## Considered Options

* JSON
* Avro
* Flat Buffers
* Apache Arrow

## Decision Outcome

Chosen option: Apache Arrow, because the copybook data types and structure is most compatible with
the data types that apache arrow provides. Apache Arrow also unlocks an entire eco-system of formats,
file types, transmission methods, transformation tools, and analysis tools that will be accessible
to projects that utilize it. Making Apache Arrow the most interoperable option.

## Pros and Cons of the Options

### JSON

I'm listing JSON here for completeness because it is a common format that is picked but it has many drawbacks.

* Good, Data Types are compatible and does support nested types.
* Good, Highly Interoperable
  * It's hard to find a software product that does not have some level of support for JSON. Even
  SQL databases support JSON these days.
* Good, You can write it directly to a json file that is viewable in any text editor
* Good, Decent support for data analysis sometimes
  * Depending on what technologies you are using and what type of data analysis you are doing
JSON can be ok. Often times it can be more of a hindrance but it typically depends on what technology
you are using to store it.
* Bad, JSON does not have a typed schema.
  * Most statically typed languages will need to write additonal code to handle different data types.
  * Going to a loose schema loses a lot of information about the data because the copybook does
provide strict data types. This can be incredibly frustrating if a system downstream wants to store
the data in a strictly typed data store such as SQL.
* Bad, Limited support for transformations
  * The JSON format itself does not provide extra utilities for transformations. But databases do
provide some capability here as well as pandas.
* Bad, Poor Performance
  * JSON is known for poor performance. You can choose to compress it to reduce it's size when
transmitting over a network but that has a cpu cost.

### Avro

[Apache Avro](https://avro.apache.org/docs/) is a data serialization system that relies on a data
schema to serialize and transmit data to other systems.

* Good, The data structures are mostly compatible.
  * Avro does allow developers to define nested data structures but it does not support decimal
types directly.
* Good, Interoperability
  * Several tools support Avro such as Kafka, AWS Athena, and Spark.
  * Data can be written to files in a binary format with the schema. Which makes it easier for
  developers to parse files.
* Good, Moderate Performance
  * The Avro format is much faster compared to the JSON format but it still requires
  serialization/deserialization costs when transmitting over the network.
* Bad, There is no native support for decimal types. There is support for floats and doubles but we
may need to use caution in storing all decimals in either of those types to avoid precision errors
or floating point errors.
* Bad, Requires developers to write a schema definition in a separate file.
  * A schema file could be advantageous for sharing flat buffers across systems more easily. But it
  is typically written by the developer and we would most likely need to generate it from a copybook
  file.
* No Transformation Support
* No Data Analysis Support
  * I don't think it is surprising that avro does not provide transformation tools or analysis tools
  since it is primarily designed for transmitting data. You could definitely still do these things
  with data in the avro format but it will likely require more custom code.

### Flat Buffers

[Flat Buffers](https://google.github.io/flatbuffers/) provide an In-memory binary buffer with nested
objects that are defined by a schema with forwards/backwards compatibility.

* Good, The Data Structures are mostly compatible.
  * flat buffers do allow developers to store nested data types, but there is not a clear data
  type to use for decimal values even though it does support floats and doubles.
* Good, Performance
  * FlatBuffers do have pretty good benchmarks against protocol buffers and json. Flat buffers do
  adopt the "zero-copy" approach to managing data. Although, I haven't seen benchmarks against arrow.
* Bad, There is no native support for decimal types. There is support for floats and doubles but we
may need to use caution in storing all decimals in either of those types to avoid precision errors
or floating point errors.
* Bad, Moderate to Low Interoperability
  * Flat buffers do allow other programs to read the buffers that are created. But finding technologies
  that support flat buffers is still difficult.
  * You can write flat buffers to a binary file but I do not see tools for converting to csv,
  parquet, or other common file formats.
  * There are [some examples](https://github.com/google/flatbuffers/blob/master/grpc/tests/JavaGrpcTest.java)
  of folks using flat buffers with gRPC but there does not seem to be much documentation about
  this so it still seems somewhat niche.
* Bad, Minimal Transformation Support
  * The flat buffers library has [minimal support](https://google.github.io/flatbuffers/flatbuffers_guide_tutorial.html#autotoc_md187)
  for modifying values.
* Bad, Not much Data Analysis Support either
* Bad, Requires developers to write a schema definition in a separate file.
  * A schema file could be advantageous for sharing flat buffers across systems more easily. But it
  is typically written by the developer and we would most likely need to generate it from a copybook
  file.

### Apache Arrow

[Apache Arrow](https://arrow.apache.org/overview/) provides a column oriented in-memory data format.
It is language-agnostic and a little bit more data analysis oriented.

* Good, The Data Structures are Compatible
  * Arrow provides nested types and supports the necessary data types.
  * Arrow also has a decimal data type that should match the decimals defined by the copybook.
  * It also has more modern data types for dates and timestamps that do not exist in the copybook.
  But this could enable developers to transform fields into more modern data types after it is loaded
  into an arrow record.
* Good, Very High Interoperability
  * Arrow has an "on-the-wire" representation that does not require deserialization. This is very
  powerful and has lead to integrations for writing data over [gRPC Arrow Flight](https://arrow.apache.org/blog/2019/10/13/introducing-arrow-flight/#flight-basics),
   the IPC protocol, and [Open DataBase Connectivity (ODBC) Support](https://pypi.org/project/arrow-odbc/).
  * Arrow Buffers can be shared across processes without deserialization which reduces the cost of
  passing data between different programs on the same machine. Even if they are written in different
  languages.
  * Supports writing data to different file formats such as CSV, parquet, Arrow IPC, and Feather.
  Converting data to CSV or parquet could also open up a lot of possibilities with BigQuery or AWS Athena.
  * pandas is a very popular way to work with data and apache arrow does have [pandas integration](https://arrow.apache.org/docs/python/pandas.html)
* Good, Transformation Support
  * Arrow does provide [compute APIs](https://arrow.apache.org/docs/cpp/compute.html) which allow
  you to do some transformations on a record batch. I do believe a developer can define their own
  transformations if a function they need does not exist. One caveat is that this functionality is
  still fairly new and may not be fully developed in all languages.
  * Data can also be pulled into pandas for transformations if
you are working in python.
* Good, Data Analysis Support
  * Apache arrow is really good for doing analytics on a small subset of fields within a data set.
* Good, Performance
  * Most operations in apache arrow are "zero-copy" operations meaning that they are performed by
  referencing the same memory locations rather than copying, modifying, and storing data in a
  separate memory location which can save your cpu some work and some memory.
  * The "on-the-wire" representation allows developers to save cpu when sharing the data with other
  systems or processes since you do not need to deserialize/serialize the data.
  * The column oriented nature of apache arrow provides an additional dimension for workload
  parallelization in some cases. Since the data can be sliced as a subset of fields you can
  parallelize by row and column but this may not be appropriate for a data set that needs to ensure
  transactional integrity.
* Good, The schema can be defined dynamically in code.
  * This is good for us because we can generate the schema from the copybook.
* Good, Apache arrow does allow you to create Field and Schema meta data. Which we could use to
track the original copybook data-type. This could come in handy if the schema needs to be converted
back to it's copybook format or if the field values
should be padded to their original fixed-length values.
* Bad, Different levels of support per language
  * Apache Arrow is rapidly developing but it may not have the same levels of support for all
  programming languages that it currently supports so developers should think about what is most
  important to them when choosing a programming language. But luckily, switching between programming
  languages with apache arrow when needed should have a low performance cost.

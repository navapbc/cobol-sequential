# Choose a Core Language

* Status: accepted
* Deciders: Ben Bemis
* Date: 2023-04-01

Technical Story: [Add First Decisisons](https://github.com/navapbc/cobol-sequential/issues/7)

## Context and Problem Statement

To help make this tool appeal to more teams and engineers I think it makes sense to plan to build FFIs
(Foriegn Functional Interfaces) in multiple languages so that teams can ultimately choose the language
that fits with their tech stack and preferences the best. This approach should also mesh well with
Apache Arrow since arrow buffers can be shared across programming languages. So then we must choose
a programming language for the core implementation.

## Decision Drivers

* Performance should be a big factor in order to prevent a FFI from feeling limited.
* Memory Safety should be prioritized to prevent memory management bugs.

## Considered Options

* C/C++
* Golang
* Rust

## Decision Outcome

Chosen option: Rust, because it offers really great performance that should not inhibit future FFIs
and it offers really great memory safety features.

## Pros and Cons of the Options

### C/C++

* Good, C/C++ has really great performance.
* Bad, C/C++ can be difficult to write memory safe code.

### Golang

* Good, Golang has pretty good performance.
* Good, is easier to learn than C.
* Good, Go's garbage collector makes memory management easier.
* Bad, Go's garbage collector does have a little bit more overhead than C or Rust.

### Rust

* Good, Rust is more performant than Golang.
* Good, Rust guarantees memory safety at compile time.
* Good, Rust does not have a garbage collector.
* Bad, Learning Rust may have a steeper learning curve than other languages.

## Links

* [Rust vs Go](https://bitfieldconsulting.com/golang/rust-vs-go)
* [Go vs Rust](https://www.getclockwise.com/blog/rust-vs-go)
* [Go](https://go.dev/tour/welcome/1)
* [rust](https://doc.rust-lang.org/book/ch00-00-introduction.html)

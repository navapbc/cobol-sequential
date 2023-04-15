# Cobol Sequential Rust

Rust is the core implementation for Cobol sequential and you will find all the rust code under this
directory.

## Local Development

This Rust Project primarily uses [Cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html)
for building, running, testing, and dependency management.

NOTE: All commands are expected to be run from within the `rust` directory.

You can compile the project with

```bash
cargo build
```

Test the project with

```bash
cargo test
```

## Rust Crate Documentation

Cargo will generate html documentation based on the inline comments found in the rust code. To
view the rust documentation check the `target/doc/copybook_reader/index.html` folder after running
the command:

```bash
cargo doc
```

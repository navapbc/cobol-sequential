# Contributing Guidelines

Welcome to our open source project! We appreciate your interest in contributing to our community.
We strive to create a welcoming and inclusive environment for all contributors.

Before you get started, please read through the following guidelines to ensure a smooth and
productive collaboration.

## 📱Maintainer Response Times

Please be patient with the project maintainers. This project is not supported full-time and it
may take some time for someone to get back to you.

## 🪲Did you find a bug?

This project uses github issues for tracking bugs and new development work.

To report a new Bug:

1. First review the [open issues](https://github.com/navapbc/cobol-sequential/issues) to see if
someone has already encountered your issue.
2. If no one has reported this issue yet, create a new github issue, and select the bug report issue
template.
3. Fill out the bug report the best you can and make sure that you provide reproducible steps.

## 📖Read before making a change

When making a change plase ensure that you use a pull request from your **own** fork and always create
[feature branches](https://www.atlassian.com/git/tutorials/comparing-workflows/feature-branch-workflow)
for your changes.

### Linting

In order to enforce consistent style in our code and documentation we have linters hooked up to
our CI. To help aid your local development it is useful to hook this linters into
your IDE. See the list below for linters that can integrate with your IDE:

- markdownlint-cli
  - [VSCode: markdownlint](https://marketplace.visualstudio.com/items?itemName=DavidAnson.vscode-markdownlint)
- rust formatting
  - [VSCode: rust.analyzer](https://rust-analyzer.github.io/)

NOTE: If your IDE is not listed feel free to add a PR for a linter that works with your IDE

### ✅Tests and Documentation are non-optional

Don't forget to include new tests and documentation in every code contribution. This will help
reviewers and future developers understand how your changes are meant to be used.

Contributions that just contain improved tests or documentation are always welcomed.

### 💡Do you want to propose a significant new feature?

For large new ideas please file a new issue describing the problem you would like to solve. Please
note in the issue how involved you would like to be with development.

The github issue will serve as a starting point for discussion and may lead to architecture decisions
before implementation is started.

When proposing a new architecture decision you should use the [MADR](https://adr.github.io/madr/) format.

## ❔Questions

If you have any questions please file a github issue and try to provide as much context as you can.

## 📃License

All contributions made to this repository will be under the Apache 2.0 license. You can read the full
license [here](LICENSE)

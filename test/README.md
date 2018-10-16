# Intentio Reference Test Suite

This directory contains test cases of the Intentio Reference Test Suite. This suite enables Intentio language implementations to ensure 100% compatibility with official Intentio semantics, as specified in the [Intentio Reference].

Notably, this test suite is also used to track work progress on the [Intentio Compiler].

## Table of Contents

<!-- toc -->

- [Directory structure](#directory-structure)
- [Test file syntax](#test-file-syntax)
  * [`compile`](#compile)
  * [`run`](#run)
- [Running test suite](#running-test-suite)

<!-- tocstop -->

## Directory structure

Test cases are placed in nested directory hierarchy inside this directory. Particular test case can be defined in two ways:

1. If current directory contains a file named `test.yaml`, the whole directory is considered a single, **multi-file**, test case. The `test.yaml` contains instructions how to compile and run the test case.
2. **Single-file** test cases should be defined using the shorter form, as single Intentio source files, with the `*.ieo` extension. The test file for single-file test cases is placed inside line comments at the very beginning of the source code.

## Test file syntax

Test files have a simple, YAML-based syntax. Test cases are identified with a path to the directory containing `test.yaml` or source file in case of single-file tests, relative to root directory of the test suite.

A test file is a list of commands describing how to perform particular test. For example:

```yaml
- compile: ["multiply_by_two.ieo"]
- run:
    stdin: "2"
    stdout: "4"
```

### `compile`

```yaml
compile: ["arg0", "arg1"...]
```

Execute tested Intentio Compiler with given arguments. For single-file test cases this command, if not provided, is implicitly prepended, with test file as the single argument. The compilation is expected to pass successfully, without warnings.

### `run`

```yaml
run:
  stdin: ...    # test input passed to program standard input
  stdout: ...   # expected standard output
  stderr: ...   # expected standard error output
```

Execute the resulting binary of the previous `compile` command.

Input/output stream can be specified using following syntaxes:

```yaml
# Pass `data` as-is to tested process
stdin: "data"

# Pass each list item as new line to tested process
stdin:
  - line1
  - line2
```

## Running test suite

The `test-runner` tool from the [Intentio Compiler] repository can be used to run the test suite.

[Intentio Reference]: https://github.com/intentio-lang/reference
[Intentio Compiler]: https://github.com/intentio-lang/intentio

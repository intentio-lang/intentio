# Intentio Reference Test Suite

This directory contains test cases of the Intentio Reference Test Suite. This suite enables Intentio language implementations to ensure 100% compatibility with official Intentio semantics, as specified in the [Intentio Reference].

Notably, this test suite is also used to track work progress on the [Intentio Compiler].

## Table of Contents

<!-- toc -->

- [Directory structure](#directory-structure)
- [Test file syntax](#test-file-syntax)
  * [`COMPILE`](#compile)
  * [`RUN`](#run)
- [Running test suite](#running-test-suite)

<!-- tocstop -->

## Directory structure

Test cases are placed in nested directory hierarchy inside this directory. Particular test case can be defined in two ways:

1. If current directory contains a file named `Testfile`, the whole directory is considered a single, **multi-file**, test case. The `Testfile` contains instructions how to compile and run the test case.
2. **Single-file** test cases should be defined using the shorter form, as single Intentio source files, with the `*.ieo` extension. The test file for single-file test cases is placed inside line comments at the very beginning of the source code.

## Test file syntax

Test files have a simple syntax. Test cases are identified with path to the directory containing `Testfile` or source file in case of single-file tests, relative to root directory of the test suite.

A test file is a list of commands describing how to perform particular test. For example:

```
COMPILE multiply_by_two.ieo
RUN
>>> 2
... 4
```

### `COMPILE`

```
COMPILE arg0 [arg1...]
```

Execute tested Intentio Compiler with given arguments. For single-file test cases this command, if not provided, this command is implicitly prepended, with test file as the single argument. The compilation is expected to pass successfully, without warnings.

### `RUN`

```
RUN
```

Execute the resulting binary of the previous `COMPILE` command.

Starting with the very next line, until the next command, the test file can specify the expected standard input and standard output interactions of the tested executable. The lines starting with `>>> ` (note leading space) sequence describe lines passed to the standard input, while the lines prefixed with `... ` describe expected lines on the standard output. Trailing lines may be omitted. Relative ordering between input and output lines is not tested, for example, the following specifications are equivalent:

```
>>> a    |   >>> a
>>> b    |   ... c
... c    |   >>> b
```

But the following are not:

```
>>> a    |   >>> b
>>> b    |   >>> a
... c    |   ... c
```

## Running test suite

The `test-runner` tool from the [Intentio Compiler] repository can be used to run the test suite.

[Intentio Reference]: https://github.com/intentio-lang/reference
[Intentio Compiler]: https://github.com/intentio-lang/intentio

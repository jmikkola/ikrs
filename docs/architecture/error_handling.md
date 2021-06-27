# Error Handling

This document covers how error handling inside the compiler works.

# Kinds of errors

There are three kinds of errors the compiler needs to be able to handle:

* Internal errors. These are errors that should never happen unless there's a bug in the compiler.
* Failures. This is when some operation in the compiler didn't work in an expected way. For example,
  failure to read a file or type inference not being able to find a typing.
* User errors. This is when the user needs to do something to fix the error (for example, a syntax
  error). Note that user errors generally also result in failures.

These three kinds of errors are handled differently.

## Internal errors

Internal errors should never happen, so the compiler just panics. This is because there isn't
necessarily a reasonable thing the compiler can do afterwards, and because the user should never see
one, so it doesn't need to be presented nicely. Using panics instead of `Result<_>` saves a lot of
extra error handling machinery (saving both code and execution time).

## Failures

Each of the operations the compiler performs, from parsing the arguments to emitting the binary, can
fail. These failures are expected, and should be handled nicely. Failures should also stop
processing downstream of the operation that failed. For example, if the compiler fails to read a
file, it won't attempt to tokenize the results afterwards. This makes `Result<_>` return values a
natural fit.

These are annotated with some amount of context (e.g. which file failed to load).

For simple user errors (like specifying a bad path to a file), this mechanism is also used to report
the problem.

## User errors

Not all kinds of user errors rely on the `Result` error handling mechanism to communicate the error.

For example, a single file a can have multiple tokenization or parse error (and a project can have
many files!). These are difficult to model as a single error returned via a `Result`. Instead, these
stages return a generic error in the `Result` (reporting the failure of that stage) but also keep a
separate list of all the errors they encountered. The caller of that stage manually handles printing
out the errors discovered (grep for `eprintln`).


# Library

This currently uses the `anyhow` library to make it easier to handle mixed errors.

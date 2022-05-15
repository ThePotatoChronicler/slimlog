# Syntax

Slimlog parser only understands four literals,
which made the parser very easy to write

## Statement

Statements are the base of the language, your program is a single recursive statement,
usually a `do` statement. The syntax is as follows:
```
  (command [args..])
```
The brackets are required and so it the `command`, which means you cannot
have empty brackets in slimlog, that would be a syntax error.
`args` can be any type, including another statement. `command` must
be a valid `identifier`

## String

String literals are like from any other language, and do not need to be
explained too deeply.
```
  'string'
  "string"
```
As of now, there are no differences between the delimiters, and there
are no escape sequences, except for those that mlog itself uses.

## Number

Numbers are always doubles, and due to how they're currently implemented,
they will be converted during compilation to Rust's f64 type, possibly
changing them from how they appear in the final code. This will possibly
be fixed in future versions of slimlog.
```
  0
  1.0
  2.5
```

## Identifier

Identifiers is a string of non-delimited characters that does not start with
a number. They're used as commands, variables, operators, keywords, and
all functions are free to interpret them however they want, although
there is some amount of standardisation kept between them.

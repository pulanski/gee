# Compiler Front-end for the minimal Programming Language, Gee

### Overview

The compiler front-end parses **Gee programs** following the grammatical
structure described in **[`grammar.txt`](./grammar.txt)**. The front-end handles
the analysis-phase typical of modern compilers. Both the phases of lexing and
parsing are done by hand rather than via the use of a lexicial analyzer
generator (e.g. `flex`, `lex`, etc.) or a parser generator (e.g. `ANTLR`,
`yacc`, `LALRPOP`, etc.). More information on the various techniques for lexing
and parsing along with common tools used in practice can be found
**[here](https://en.wikipedia.org/wiki/Comparison_of_parser_generators)** if
you're interested.

Parsing is done via an **LL(1) Recursive-descent parser**. The parser returns
the first syntactic error it encounters during a parse of the program back to
the user. This could later be expanded on by adding by adding resilient parsing
where all of the errors are added to a stream and returned to the user together,
however that was outside the scope of the project.

After a successful parse pass, **context-sensitive analysis** is conducted via a
**type-checker** which verifies the validity of the program subject to the
type-system. Similar to parsing, this phase was also fairly minimal in scope,
but communicates the general idea behind this phase typical in compilation of
today's production-grade languages, and as such, could be built upon in the
future. The type-checker will return errors for cases such as a variable
originally defined as one type (e.g. **string**) are reassigned to a value of
another type (e.g. **boolean**) or attempting to access variables which are
undefined. Similar to the parser, the type-checker will stop upon the first
error it encounters.

### Front-end components

- **Lexer** detailed in the **`Lexer`** class found within
  **[`gee.py`](./gee.py)**.
- LL(1) Recursive-descent parser detailed via a _collection of parse routines_
  found in **`gee.py`** which generate an _AST_ defined via a collection of
  **AST node classes** found within **[`gee.py`](./gee.py)**.
- Type-checker found within **[`gee.py`](./gee.py)** which ensures programs
  adhere to the type system of **Gee**.

### Usage

If you want to test the front-end, you can do so via a command of the form seen
below on any of the provided test cases in the `tests` directory or on a test
case of your own. For any of the provided test cases found within the `tests`
directory, you can verify their correctness by checking their output against an
associated `test-case-out.txt` file.

```shell
python3 gee.py tests/type/fact.txt
```

OR

```shell
python3 gee.py tests/type/fact.txt > tests/type/fact-actual.txt
diff tests/type/fact-out.txt tests/type/fact-actual.txt
```

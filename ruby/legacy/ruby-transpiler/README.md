# ruby-transpiler

This transpiler is meant to take code written in a Typed Ruby Dialect and
rewrite it as regular ol' Ruby code with runtime type checks.

The code is separated into 5 crates, explained briefly:
* `lexer` is responsible for taking a source code (`String`) and turning into a
  list of tokens (`Vec<Token>`)
* `ast` contains the definitions for the Abstract Syntax Tree of the TRD
* `parser` is responsible for taking a tokenized program (`Vec<Token>`) and
  turning it into an AST (`Node`).
* `unparser` is responsible for taking a parsed TRD program and turning it into
  a regular Ruby program.
* `cli` is the binary that for now only allows you to take a filename to be read
  and transpiled.

## Contributing

The best way to contribute for now would be adding test examples under
`unparser/src/lib.rs` and grinding the code through using the existing code as
a base and the compiler as an assistant.

I might not be accepting refactoring efforts during the initial phase of the
proyect.

I'm not versioning nor publishing the crates for now.

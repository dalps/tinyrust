# Tiny Rust

Install the dependencies:

```sh
opam install menhir ppx_jane ANSITerminal minttea nice_parser re
```

Or alternatively:

```sh
opam install . --deps-only
```

Run the frontend with the syntax `dune exec tinyrust <max_steps> <program>`. For example:

```sh
dune exec tinyrust 100 test/examples/01-print.rs
```

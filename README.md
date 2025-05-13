# Tiny Rust

`tinyrust` is an interpreter of a small subset of the Rust language, focusing primarily on reimplementing its borrow-checking rules in a small-step fashion.

It comes with a friendly TUI (see below) that lets you step through the instructions of a valid Rust program and inspect the environment and memory during its execution.

## Play with Tiny Rust
First, install the dependencies:

```sh
opam install menhir ppx_jane ANSITerminal minttea nice_parser re
```

Then, run the frontend with the syntax `dune exec tinyrust <max_steps> <program>`. For example:

```sh
dune exec tinyrust 100 test/examples/01-print.rs
```

Here's what it looks like:

![image](https://github.com/user-attachments/assets/842d09ca-3131-4417-b3ca-054ef819de48)

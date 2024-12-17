open Ast

module P = Nice_parser.Make (struct
  type result = Ast.crate
  type token = Tiny_parser.token
  exception ParseError = Tiny_parser.Error
  let parse = Tiny_parser.crate
  include Tiny_lexer
end)

include P

let _ = P.pp_exceptions ()

let%expect_test "" = parse_string "fn main() {}" |> Format.printf "%a" pp_crate

let%expect_test "" =
  "\"hello\"" |> String.split_on_char '\"' |> fun s ->
  List.nth s 1 |> print_endline

let%expect_test "" =
  parse_string
    {|
      fn main() {
        let x = 3;   // variabile immutabile di tipo intero
        let y = x+1;
        x = x+y;     // errore: x immutabile
        println!("{x}")
      }
    |}
  |> Format.printf "%a" pp_crate

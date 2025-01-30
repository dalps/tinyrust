open TinyrustLib
open Utils
open Common

(** ------------------------------------------
    Start of parser tests
    ------------------------------------------ *)

let%test_unit "test_parser" =
  Array.iter
    (fun (ex, p) ->
      try
        Parser.parse_string p |> ignore;
        pr "✔ %s\n" ex
      with _ ->
        pr "✘ Couldn't parse %s\n" ex)
    examples_dict

open TinyrustLib
open Parser

let _ = P.pp_exceptions ()

let pr = Printf.printf

let%test_unit "" =
  parse_string
    {|
      fn main(x) {
        let x = 3;   // variabile immutabile di tipo intero
        let y = x+1;
        x = x+y;     // errore: x immutabile
        {println!("{x}");}
      }
    |}
  |> ignore

let examples_dir = "/home/dalpi/tinyrust/test/examples/"
let example name = examples_dir ^ name
let examples =
  let dirs = Sys.readdir examples_dir in
  Array.sort String.compare dirs;
  dirs

(* let%expect_test "" =
  let e = parse_file (example examples.(0)) in
  pr "%s" (show_statement e) *)


(* let%test_unit "test_parse" =
  Array.iter (fun name -> parse_file (example name) |> ignore) examples *)

(* let%test_unit "test_parse_1" =
  parse_file (example examples.(0)) |> ignore *)

(* let%test_unit "test_parse_2" =
  parse_file (example examples.(1)) |> ignore
let%test_unit "test_parse_3" =
  parse_file (example examples.(2)) |> ignore *)

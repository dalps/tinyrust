open TinyrustLib
open Trace
open Parser
open Common
open State

let helper n s = s |> parse_string |> trace_prog n |> ignore

let%expect_test "test_trace_2" =
  {|
  fn main() {
    let x = 2; // prima dichiarazione di x
    let x = x + 1; // seconda dichiarazione di x
    {
        let x = x * 2; // terza dichiarazione di x
        println!("{x}"); // output: 6
    };
    println!("{x}") // output: 3
  }
  |}
  |> helper 30

let tests : (string * int * string trace_result) array =
  (* gas, outcome *)
  [|
    ("01-print.rs", 25, Ok "3\n4\n");
    ("02-intError.rs", 25, Error (CannotMutate "x"));
    ("03-intOk.rs", 25, Ok "7\n");
    ("04-stringError.rs", 25, Error (UnboundVar "x"));
    ("05-stringOk.rs", 25, Ok "Ciao, mondo\n");
    ("06-scopeOk.rs", 25, Ok "6\n3\n");
    ("07-scopeError.rs", 25, Error (UnboundVar "y"));
    ("08-func.rs", 25, Ok "7\n");
    ("09-proc.rs", 25, Ok "7\n");
    ("10-ite.rs", 25, Ok "dispari\n");
    ("11-ownError.rs", 25, Error (MovedValue "x"));
    ("12-lendFnError.rs", 25, Error (MovedValue "x"));
    ("13-borrow.rs", 25, Ok "Ciao\nCiao\n");
    ( "14-borrowFun.rs",
      25,
      Ok "il parametro x: Ciao\nil parametro prestato: Ciao\n" );
    ("15-borrowError.rs", 25, Error (DataRace ("x", true, false)));
    ("16-borrowMut.rs", 25, Ok "Ciao, mondo\nCiao, mondo\n");
    ("17-borrowMutError.rs", 40, Error (MutBorrowOfNonMut "x"));
    ("18-loop.rs", 50, Error (OutOfGas 50));
    ("19-loopBreak.rs", 50, Ok "3\n2\n1\n0\n");
    ("20-loopNested.rs", 50, Ok "0,0\n0,1\n1,0\n1,1\n");
    ("21-exprBlock.rs", 25, Ok "7");
    ("22-funExpr.rs", 25, Error (UnboundVar "interna"));
    ("23-scopeCheck.rs", 25, Error (UnboundVar "y"));
  |] [@@ocamlformat "disable"]

let%expect_test "test_trace" =
  Array.iter2
    (fun (name, prog) (_, gas, exp) ->
      let prog = Parser.parse_string prog in
      let st, _, res = Trace.trace_prog gas prog in
      let icon =
        match (res, exp) with
        | Ok _, Ok _ | Error _, Error _ -> "✔"
        | Ok _, Error _ | Error _, Ok _ -> "✘"
      in
      let ok, error = ("Ok", "Error") in
      let res_kind, res_output =
        match res with
        | Ok _ -> (ok, st.output)
        | Error err -> (error, string_of_trace_error err)
      in
      let exp_kind, exp_output =
        match exp with
        | Ok output -> (ok, output)
        | Error err -> (error, string_of_trace_error err)
      in
      pr "------------------------\n%s %s\n------------------------\n" icon name;
      List.iter
        (fun (title, kind, output) -> pr "%-9s %-9s\n%s\n\n" title kind output)
        [
          ("Output:", res_kind, res_output); ("Expected:", exp_kind, exp_output);
        ];
      pr "%s\n" (St.to_string st))
    examples_dict tests

let%expect_test "" =
  {|
    fn foo(x: i32, y: i32) -> i32 {
      let z = x + y;
      z + z
    }
    fn double(x: i32) -> i32 {
      if x + 0 == 0 {
        println!("hi");
      }
      else {
        println!("hello");
      };
      x + x
    }
    fn main() {
      let mut x = 3;
      let y = foo(x,42);
      println!("{x} bla bla {y}")
    }
  |}
  |> helper 30

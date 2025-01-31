open TinyrustLib
open Trace
open Parser
open Common
open Prettyprint
open Errors
open Utils

let helper n s = s |> parse_string |> trace_prog n |> ignore

let tests : (string * int * string trace_result) array =
  [|
    ("01-print.rs",           25, Ok "3\n4\n");
    ("02-intError.rs",        25, Error (CannotMutate "x"));
    ("03-intOk.rs",           25, Ok "7\n");
    ("04-stringError.rs",     25, Error (CannotMutate "x"));
    ("05-stringOk.rs",        25, Ok "Ciao, mondo\n");
    ("06-scopeOk.rs",         25, Ok "6\n3\n");
    ("07-scopeError.rs",      25, Error (UnboundVar "y"));
    ("08-func.rs",            25, Ok "10\n");
    ("09-proc.rs",            25, Ok "7\n");
    ("10-ifThenElse.rs",      25, Ok "dispari\n");
    ("11-ownError.rs",        25, Error (MovedValue "x"));
    ("12-ownFnError.rs",      25, Error (MovedValue "x"));
    ("13-borrow.rs",          25, Ok "Ciao\nCiao\n");
    ("14-borrowFn.rs",        25, Ok "il parametro prestato: Ciao\nil parametro x: Ciao\n" );
    ("15-borrowError.rs",     25, Error (DataRace ("x", true, false)));
    ("16-borrowMut.rs",       25, Ok "Ciao, mondo\nCiao, mondo\n");
    ("17-borrowMutError.rs",  40, Error (MutBorrowOfNonMut "x"));
    ("18-loop.rs",            50, Error (OutOfGas 50));
    ("19-loopBreak.rs",       50, Ok "3\n2\n1\n0\n");
    ("20-loopNested.rs",      50, Ok "0,0\n0,1\n1,0\n1,1\n2,0\n2,1\n");
    ("21-exprBlock.rs",       25, Ok "7\n");
    ("22-funExpr.rs",         25, Error (UnboundVar "interna"));
    ("23-scopeCheck.rs",      25, Error (UnboundVar "y"));
    ("23-scopeCheck.rs",      25, Error (UnboundVar "y"));
    ("23-scopeCheck.rs",      25, Error (UnboundVar "y"));
    ("23-scopeCheck.rs",      25, Error (UnboundVar "y"));
    ("23-scopeCheck.rs",      25, Error (UnboundVar "y"));
    ("23-scopeCheck.rs",      25, Error (UnboundVar "y"));
    ("23-scopeCheck.rs",      25, Error (UnboundVar "y"));
  |] [@@ocamlformat "disable"]

(* let _ =
  try
    Array.iter2
      (fun (name, prog) (_, gas, exp) ->
        let prog = Parser.parse_string prog in
        let out = Trace.trace_prog gas prog in
        let icon =
          match (out.result, exp) with
          | Ok _, Ok _ | Error _, Error _ -> "✔"
          | Ok _, Error _ | Error _, Ok _ -> "✘"
        in
        let ok, error = ("Ok", "Error") in
        let res_kind, res_output =
          match out.result with
          | Ok _ -> (ok, out.state.output)
          | Error err -> (error, string_of_trace_error err)
        in
        let exp_kind, exp_output =
          match exp with
          | Ok output -> (ok, output)
          | Error err -> (error, string_of_trace_error err)
        in
        pr "------------------------\n%s %s\n------------------------\n" icon
          name;
        List.iter
          (fun (title, kind, output) ->
            pr "%-9s %-9s\n%s\n\n" title kind output)
          [
            ("Output:", res_kind, res_output);
            ("Expected:", exp_kind, exp_output);
          ]
        (* pr "%s\n" (string_of_traceoutcome out) *))
      examples_dict tests
  with _ -> () *)

let%expect_test "test_06" =
  let _, prog = examples_dict.(5) in
  parse_string prog
  |> List.map (string_of_statement 0)
  |> String.concat "\n" |> pr "%s"

let%expect_test "test_08" =
  let _, prog = examples_dict.(7) in
  parse_string prog
  |> List.map (string_of_statement 0)
  |> String.concat "\n" |> pr "%s"

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
    ("01-print.rs",           25,   Ok "3\n4\n");
    ("02-intError.rs",        25,   Error (CannotMutate "x"));
    ("03-intOk.rs",           25,   Ok "7\n");
    ("04-stringError.rs",     25,   Error (MutBorrowOfNonMut "x"));
    ("05-stringOk.rs",        25,   Ok "Ciao, mondo\n");
    ("06-scopeOk.rs",         25,   Ok "6\n3\n");
    ("07-scopeError.rs",      25,   Error (UnboundVar "y"));
    ("08-func.rs",            25,   Ok "10\n");
    ("09-proc.rs",            25,   Ok "7\n");
    ("10-ifThenElse.rs",      25,   Ok "dispari\n");
    ("11-ownError.rs",        25,   Error (MovedValue "x"));
    ("12-ownFnError.rs",      25,   Error (MovedValue "x"));
    ("13-borrow.rs",          25,   Ok "Ciao\nCiao\n");
    ("14-borrowFn.rs",        25,   Ok "il parametro prestato: Ciao\nil parametro x: Ciao\n" );
    ("15-borrowError.rs",     25,   Error (DataRace {borrowed = "x"; is = `imm; want = `mut}));
    ("16-borrowMut.rs",       25,   Ok "Ciao, mondo\nCiao, mondo\n");
    ("17-borrowMutError.rs",  40,   Error (MutBorrowOfNonMut "x"));
    ("18-loop.rs",            50,   Error (OutOfGas 50));
    ("19-loopBreak.rs",       50,   Ok "3\n2\n1\n0\n");
    ("20-loopNested.rs",      150,  Ok "0,0\n0,1\n1,0\n1,1\n2,0\n2,1\n");
    ("21-exprBlock.rs",       25,   Ok "7\n");
    ("22-funExpr.rs",         50,   Error (UnboundVar "interna"));
    ("23-scopeCheck.rs",      50,   Error (UnboundVar "y"));
    ("24-borrowOk.rs",        50,   Ok "hellociao\n");
    ("25-borrowError.rs",     50,   Error (BorrowOfMovedValue "x"));
    ("26-borrowError.rs",     50,   Error (DataRace {borrowed = "x"; is = `mut; want = `mut}));
    ("27-borrowError.rs",     50,   Error (DataRace {borrowed = "x"; is = `mut; want = `imm}));
    ("28-borrowError.rs",     100,  Error (CannotMoveOut "x"));
    ("29-toplevelFn.rs",      100,  Ok "3 x 8 = 24\n");
    ("30-assignMoves.rs",     100,  Ok "Ciao\n");
    ("31-assignNewString.rs", 100,  Ok "hello ciao\n");
    ("32-multipleArgs.rs",    100,  Ok "42\n57\n3\n2\n");
    ("33-borrowError.rs",     50,   Error (DataRace {borrowed = "x"; is = `mut; want = `imm}));
    ("34-returnMoves.rs",     50,   Ok "prima di fie: ciao\ndentro fie: ciao\ndopo fie: ciao\n");
    ("35-returnMoves.rs",     50,   Ok "prima di fie: ciao\ndentro fie: ciao\ndopo fie: ciao\n");
    ("36-dropError.rs",       50,   Error (MovedValue "x"));
  |] [@@ocamlformat "disable"]

let%test "tests_count__vs__examples_count" =
  let n_examples = Array.length examples_dict in
  let n_tests = Array.length tests in
  pr "%d = %d\n" n_examples n_tests;
  n_examples = n_tests

let%expect_test "test_trace" =
  let n_success, n_failures = (ref 0, ref 0) in
  Array.iter2
    (fun (name, prog) (_, gas, expected) ->
      let prog = Parser.parse_string prog in
      let actual = Trace.trace_prog gas prog in
      let good, icon =
        match (actual.result, expected) with
        | Ok _, Ok o2 when actual.state.output = o2 ->
            incr n_success;
            (true, "✔")
        | Error err1, Error err2 when err1 = err2 ->
            incr n_success;
            (true, "✔")
        | _ ->
            incr n_failures;
            (false, "✘")
      in
      let ok_str, error_str = ("Ok", "Error") in
      let res_kind, res_output =
        match actual.result with
        | Ok _ -> (ok_str, actual.state.output)
        | Error err -> (error_str, string_of_trace_error err)
      in
      let exp_kind, exp_output =
        match expected with
        | Ok output -> (ok_str, output)
        | Error err -> (error_str, string_of_trace_error err)
      in
      ANSITerminal.(
        printf
          (if good then [ green ] else [ red ])
          {|
------------------------
 %s %s
------------------------

|} icon
          name);
      List.iter
        (fun (title, kind, output) -> pr "%-9s %-9s\n%s\n\n" title kind output)
        [
          ("Output:", res_kind, res_output); ("Expected:", exp_kind, exp_output);
        ])
    examples_dict tests;
    pr "\n%d passed, %d failed\n" !n_success !n_failures

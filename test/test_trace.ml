open TinyrustLib
open Trace
open Parser
open Common

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

let tests =
  (* gas, outcome *)
  [|
    ("01-print.rs", 25, Ok "3\n4\n");
    ("02-intError.rs", 25, Error "cannot mutate immutable variable x");
    ("03-intOk.rs", 25, Ok "7\n");
    ("04-stringError.rs", 25, Error "cannot mutate immutable variable x");
    ("05-stringOk.rs", 25, Ok "Ciao, mondo\n");
    ("06-scopeOk.rs", 25, Ok "6\n3\n");
    ("07-scopeError.rs", 25, Error "y not found in this scope");
    ("08-func.rs", 25, Ok "7\n");
    ("09-proc.rs", 25, Error "borrow of x");
    ("10-ite.rs", 25, Error "borrow of x");
    ("11-ownError.rs", 25, Ok "Ciao\nCiao\n");
    ( "12-lendFnError.rs",
      25,
      Ok {| il parametro prestato: Ciao il parametro x: Ciao |} );
    ("13-borrow.rs", 25, Error "cannot mutate immutable variable x");
    ("14-borrowFun.rs", 25, Ok {| Ciao, mondo Ciao, mondo |});
    ("15-borrowError.rs", 25, Error "cannot borrow x as immutable");
    ("16-borrowMut.rs", 25, Ok "Ciao, mondo\nCiao, mondo\n");
    ("17-borrowMutError.rs", 40, Error "cannot mutate immutable variable x");
    ("18-loop.rs", 50, Ok "1\n2\n3\n4\n5\n...");
    ("19-loopBreak.rs", 50, Ok "3\n2\n1\n0");
    ("20-loopNested.rs", 50, Ok "0,0\n0,1\n1,0\n1,1\n");
    ("21-exprBlock.rs", 25, Error "y not found in this scope");
    ("22-funExpr.rs", 25, Error "interna not found in this scope");
    ("23-scopeCheck.rs", 25, Error "y not found in this scope");
  |]

let%expect_test "test_trace" =
  Array.iter2
    (fun (name, prog) (name', gas, outcome) ->
      pr "Running: %s, Test: %s\n" name name';
      let prog = Parser.parse_string prog in
      let _, res = Trace.trace_prog gas prog in
      (match (res, outcome) with
      | Ok _, Ok _ | Error _, Error _ -> pr "✔ %s\n" name
      | Ok _, Error exp ->
          pr "✘ %s went ok, but I expected the following error:\n  %s\n" name
            exp
      | Error err, Ok exp ->
          pr
            "✘ %s gave the following error:\n\
            \  %s\n\
             But I expected the following output: %s\n"
            name
            (State.string_of_trace_error err)
            exp);
      pr "---\n")
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
